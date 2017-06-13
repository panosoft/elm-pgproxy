module ConnectionManager
    exposing
        ( Model
        , Config
        , Msg
        , init
        , update
        , getConnectRequestMustExist
        , getConnectionId
        , getClientIdsAndRequests
        , isNonListenConnection
        , isListeningOnChannel
        , connect
        , removeClient
        , disconnect
        , listen
        , unlisten
        , subscriptions
        )

import Task exposing (Task)
import Tuple exposing (..)
import Time exposing (Time)
import Dict exposing (Dict)
import StringUtils exposing (..)
import Websocket exposing (..)
import Postgres exposing (..)
import Proxy.Decoder exposing (..)
import Utils.Ops exposing (..)
import Utils.Record exposing (..)
import Utils.Log exposing (..)
import Utils.Error exposing (..)
import Services.Common.Taggers exposing (..)


type alias Config msg =
    { routeToMeTagger : Msg -> msg
    , pgConnectTimeout : Time
    , errorTagger : ErrorTagger String msg
    , logTagger : LogTagger String msg
    , connectErrorTagger : Request -> String -> msg
    , connectedTagger : Request -> msg
    , connectionLostTagger : ConnectionId -> String -> msg
    , disconnectErrorTagger : Request -> ( ConnectionId, String ) -> msg
    , disconnectedTagger : Request -> ConnectionId -> msg
    , listenUnlistenErrorTagger : Request -> ( ConnectionId, String ) -> msg
    , listenUnlistenSuccessTagger : Request -> ( ConnectionId, ListenChannel, ListenUnlisten ) -> msg
    , listenEventTagger : List ClientId -> ( ConnectionId, ListenChannel, String ) -> msg
    }


type alias ConnectRequestStr =
    String


type alias Request =
    String


type alias SharedListenConnection msg =
    { clientId : ClientId
    , connectionId : ConnectionId
    , subscription : Sub msg
    }


type alias Model msg =
    { connectRequests : Dict ClientId ( ConnectRequest, Request )
    , connectionIds : Dict ClientId ConnectionId
    , sharedListenConnections : Dict ( ConnectRequestStr, ListenChannel ) (SharedListenConnection msg)
    }


init : Model msg
init =
    { connectRequests = Dict.empty
    , connectionIds = Dict.empty
    , sharedListenConnections = Dict.empty
    }


msgToCmd : msg -> Cmd msg
msgToCmd msg =
    Task.perform identity <| Task.succeed msg


type Msg
    = ConnectError Request ( ConnectionId, String )
    | Connected ClientId Request ConnectionId
    | ConnectionLost Request ( ConnectionId, String )
    | DisconnectError Request ( ConnectionId, String )
    | Disconnected Request ConnectionId
    | InternalListenDisconnectError Request ( ConnectionId, String )
    | InternalListenDisconnected Request ListenChannel ConnectionId ConnectionId
    | ListenUnlistenError Request ( ConnectionId, String )
    | ListenUnlistenSuccess Request ( ConnectionId, ListenChannel, ListenUnlisten )
    | ListenEvent ( ConnectionId, ListenChannel, String )
    | InternalUnlistenConnectError Request ( ConnectionId, String )
    | InternalUnlistenConnected Request ClientId ( ConnectRequestStr, ListenChannel ) ConnectionId


update : Config msg -> Msg -> Model msg -> ( ( Model msg, Cmd Msg ), List msg )
update config msg model =
    (\error -> config.errorTagger ( NonFatalError, "Unable to disconnect:" +-+ error ))
        |> (\disconnectError ->
                case msg of
                    ConnectError request ( connectionId, error ) ->
                        ( { model | connectRequests = Dict.remove connectionId model.connectRequests } ! [], [ config.connectErrorTagger request error ] )

                    Connected clientId request connectionId ->
                        Dict.get clientId model.connectRequests
                            |?> (\_ -> ( createConnection clientId connectionId model ! [], [ config.logTagger ( LogLevelDebug, "Created New Connection:" +-+ ( clientId, connectionId ) ), config.connectedTagger request ] ))
                            ?= (disconnectInternal config clientId connectionId True request model
                                    |> (\( ( model, cmd ), msgs ) -> ( model ! [ cmd ], List.append msgs [ config.logTagger ( LogLevelDebug, "DESTROYED NewConnection:" +-+ ( clientId, connectionId ) ) ] ))
                               )

                    ConnectionLost request ( connectionId, error ) ->
                        destroyConnection connectionId model
                            |> (\model -> ( model ! [], [ config.connectionLostTagger connectionId error ] ))

                    DisconnectError request ( connectionId, error ) ->
                        ( model ! [], [ disconnectError error, config.disconnectErrorTagger request ( connectionId, error ) ] )

                    Disconnected request connectionId ->
                        ( destroyConnection connectionId model ! [], [ config.disconnectedTagger request connectionId ] )

                    InternalListenDisconnectError request ( connectionId, error ) ->
                        ( model ! [], [ config.listenUnlistenErrorTagger request ( connectionId, error ), disconnectError error ] )

                    InternalListenDisconnected request channel newConnectionId oldConnectionId ->
                        ( model ! [], [ config.logTagger ( LogLevelDebug, "Internally disconnected connectionId:" +-+ oldConnectionId ) ] )

                    ListenUnlistenError request ( connectionId, error ) ->
                        ( model ! [], [ config.listenUnlistenErrorTagger request ( connectionId, error ) ] )

                    ListenUnlistenSuccess request ( connectionId, channel, listenUnlisten ) ->
                        ( model ! [], [ config.listenUnlistenSuccessTagger request ( connectionId, channel, listenUnlisten ) ] )

                    ListenEvent ( listenConnectionId, channel, message ) ->
                        (model.connectionIds
                            |> Dict.filter (\_ connectionId -> connectionId == listenConnectionId)
                            |> Dict.keys
                        )
                            |> (\clientIds -> ( model ! [], [ config.listenEventTagger clientIds ( listenConnectionId, channel, message ) ] ))

                    InternalUnlistenConnectError request ( connectionId, error ) ->
                        ( model ! [], [ config.listenUnlistenErrorTagger request ( connectionId, error ) ] )

                    InternalUnlistenConnected request clientId ( connectRequestStr, channel ) connectionId ->
                        { model | sharedListenConnections = Dict.remove ( connectRequestStr, channel ) model.sharedListenConnections }
                            |> (\model ->
                                    Dict.get clientId model.connectRequests
                                        |?> (\_ -> ( createConnection clientId connectionId model ! [], [ config.logTagger ( LogLevelDebug, "Created New Connection:" +-+ ( clientId, connectionId ) ), config.listenUnlistenSuccessTagger request ( connectionId, channel, UnlistenType ) ] ))
                                        ?= (disconnectInternal config clientId connectionId True request model
                                                |> (\( ( model, cmd ), msgs ) -> ( model ! [ cmd ], List.append msgs [ config.logTagger ( LogLevelDebug, "DESTROYED NewConnection:" +-+ ( clientId, connectionId ) ) ] ))
                                           )
                               )
           )


createConnection : ClientId -> ConnectionId -> Model msg -> Model msg
createConnection clientId connectionId model =
    { model | connectionIds = Dict.insert clientId connectionId model.connectionIds }


getConnectionIdMustExist : ClientId -> Model msg -> ConnectionId
getConnectionIdMustExist clientId model =
    Dict.get clientId model.connectionIds ?!= (\_ -> Debug.crash "BUG")


getClientIds : ConnectionId -> Model msg -> List (ClientId)
getClientIds connectionId model =
    model.connectionIds
        |> Dict.filter (\_ dictConnectionId -> dictConnectionId == connectionId)
        |> Dict.keys


destroyConnection : ConnectionId -> Model msg -> Model msg
destroyConnection connectionId model =
    getClientIds connectionId model
        |> List.foldl
            (\clientId model ->
                { model
                    | connectRequests = Dict.remove clientId model.connectRequests
                    , connectionIds = Dict.remove clientId model.connectionIds
                    , sharedListenConnections = Dict.filter (\_ sharedListenConnection -> sharedListenConnection.connectionId /= connectionId) model.sharedListenConnections
                }
            )
            model


makeConnectRequestComparable : ConnectRequest -> String
makeConnectRequestComparable =
    makeComparable [ .host, toString << .port_, .database, .user ]


getSharedClientIds : ClientId -> Model msg -> List ClientId
getSharedClientIds clientId model =
    Dict.get clientId model.connectionIds
        |?> (\clientConnectionId ->
                Dict.filter (\_ connectionId -> clientConnectionId == connectionId) model.connectionIds
                    |> Dict.keys
            )
        ?= []


reconnectCommon :
    (Config msg -> ClientId -> ConnectRequestStr -> ListenChannel -> ConnectionId -> Request -> Model msg -> ( ( Model msg, Cmd msg ), List msg ))
    -> Config msg
    -> ListenChannel
    -> ClientId
    -> Request
    -> Model msg
    -> ( ( Model msg, Cmd msg ), List msg )
reconnectCommon reconnectCallback config channel clientId listenUnlistenRequest model =
    getConnectRequestMustExist clientId model
        |> (\( connectRequest, _ ) ->
                makeConnectRequestComparable connectRequest
                    |> (\connectRequestStr ->
                            getConnectionIdMustExist clientId model
                                |> (\connectionId -> reconnectCallback config clientId connectRequestStr channel connectionId listenUnlistenRequest model)
                       )
           )


disconnectInternal : Config msg -> ClientId -> ConnectionId -> Bool -> Request -> Model msg -> ( ( Model msg, Cmd Msg ), List msg )
disconnectInternal config clientId connectionId discardConnection request model =
    (List.length (getSharedClientIds clientId model) == 1)
        ? ( ( model ! [ Postgres.disconnect (DisconnectError request) (Disconnected request) connectionId discardConnection ], [ config.logTagger ( LogLevelDebug, "disconnect LAST, ClientId:" +-+ clientId ) ] )
          , ( { model
                | connectRequests = Dict.remove clientId model.connectRequests
                , connectionIds = Dict.remove clientId model.connectionIds
              }
                ! []
            , [ config.logTagger ( LogLevelDebug, "disconnect NOT LAST, ClientId:" +-+ clientId ), config.disconnectedTagger request connectionId ]
            )
          )



-- API


getConnectRequestMustExist : ClientId -> Model msg -> ( ConnectRequest, Request )
getConnectRequestMustExist clientId model =
    Dict.get clientId model.connectRequests ?!= (\_ -> Debug.crash "BUG")


getClientIdsAndRequests : ConnectionId -> Model msg -> List ( ClientId, Request )
getClientIdsAndRequests connectionId model =
    model.connectionIds
        |> Dict.filter (\_ clientConnectionId -> clientConnectionId == connectionId)
        |> Dict.keys
        |> List.map (\clientId -> ( clientId, second <| getConnectRequestMustExist clientId model ))


getConnectionId : ClientId -> Model msg -> Maybe ConnectionId
getConnectionId clientId model =
    Dict.get clientId model.connectionIds


isNonListenConnection : ClientId -> Model msg -> Bool
isNonListenConnection clientId model =
    getConnectionId clientId model
        |?> (\clientConnectionId ->
                (model.sharedListenConnections
                    |> Dict.filter (\_ { connectionId } -> clientConnectionId == connectionId)
                    |> Dict.toList
                )
                    == []
            )
        ?= True


isListeningOnChannel : ClientId -> ListenChannel -> Model msg -> Bool
isListeningOnChannel clientId channel model =
    Dict.get clientId model.connectionIds
        |?> (\connectionId ->
                (model.sharedListenConnections
                    |> Dict.filter (\_ sharedListenConnection -> sharedListenConnection.connectionId == connectionId)
                    |> Dict.toList
                    |> List.head
                )
                    |?> (\( ( _, listenChannel ), _ ) -> listenChannel == channel)
                    ?= False
            )
        ?!= (\_ -> Debug.crash "BUG")


connect : Config msg -> ConnectRequest -> ClientId -> Request -> Model msg -> ( ( Model msg, Cmd msg ), List msg )
connect config connectRequest clientId request model =
    ( { model | connectRequests = Dict.insert clientId ( connectRequest, request ) model.connectRequests }
        ! [ Cmd.map config.routeToMeTagger <| Postgres.connect (ConnectError request) (Connected clientId request) (ConnectionLost request) (round config.pgConnectTimeout) connectRequest.host connectRequest.port_ connectRequest.database connectRequest.user connectRequest.password ]
    , []
    )


removeClient : ClientId -> Model msg -> Model msg
removeClient clientId model =
    { model | connectRequests = Dict.remove clientId model.connectRequests, connectionIds = Dict.remove clientId model.connectionIds }


disconnect : Config msg -> ClientId -> Bool -> Request -> Model msg -> ( ( Model msg, Cmd msg ), List msg )
disconnect config clientId discardConnection request model =
    disconnectInternal config clientId (getConnectionIdMustExist clientId model) discardConnection request model
        |> (\( ( model, cmd ), msgs ) -> ( model ! [ Cmd.map config.routeToMeTagger cmd ], msgs ))


listen : Config msg -> ListenChannel -> ClientId -> Request -> Model msg -> ( ( Model msg, Cmd msg ), List msg )
listen =
    reconnectCommon
        (\config clientId connectRequestStr channel connectionId request model ->
            (isNonListenConnection clientId model)
                ?! ( \_ ->
                        (Dict.get ( connectRequestStr, channel ) model.sharedListenConnections
                            |?> (\sharedListenConnection ->
                                    ( { model | connectionIds = Dict.insert clientId sharedListenConnection.connectionId model.connectionIds }
                                        ! [ Cmd.map config.routeToMeTagger <| Postgres.disconnect (InternalListenDisconnectError request) (InternalListenDisconnected request channel sharedListenConnection.connectionId) connectionId True ]
                                    , [ config.listenUnlistenSuccessTagger request ( connectionId, channel, ListenType ) ]
                                    )
                                )
                            ?= ( { model | sharedListenConnections = Dict.insert ( connectRequestStr, channel ) { clientId = clientId, connectionId = connectionId, subscription = Sub.map config.routeToMeTagger <| Postgres.listen (ListenUnlistenError request) (ListenUnlistenSuccess request) ListenEvent connectionId channel } model.sharedListenConnections } ! [], [] )
                        )
                   , \_ -> Debug.crash "BUG"
                   )
        )


unlisten : Config msg -> ListenChannel -> ClientId -> Request -> Model msg -> ( ( Model msg, Cmd msg ), List msg )
unlisten =
    reconnectCommon
        (\config clientId connectRequestStr channel connectionId request model ->
            Dict.get ( connectRequestStr, channel ) model.sharedListenConnections
                |?> (\_ ->
                        (List.length (getSharedClientIds clientId model) == 1)
                            ? ( ( model ! [], [ config.listenUnlistenSuccessTagger request ( connectionId, channel, UnlistenType ) ] )
                              , getConnectRequestMustExist clientId model
                                    |> (\( connectRequest, channel ) ->
                                            ( model
                                                ! [ Cmd.map config.routeToMeTagger <| Postgres.connect (InternalUnlistenConnectError request) (InternalUnlistenConnected request clientId ( connectRequestStr, channel )) (ConnectionLost request) (round config.pgConnectTimeout) connectRequest.host connectRequest.port_ connectRequest.database connectRequest.user connectRequest.password ]
                                            , []
                                            )
                                       )
                              )
                    )
                ?!= (\_ -> Debug.crash "BUG")
        )


subscriptions : Model msg -> List ( ClientId, Sub msg )
subscriptions model =
    List.map (\sharedListenConnection -> ( sharedListenConnection.clientId, sharedListenConnection.subscription )) <| Dict.values model.sharedListenConnections
