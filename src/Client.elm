module Client
    exposing
        ( Model
        , Msg(..)
        , Config
        , init
        , stop
        , update
        , destroyDisconnected
        , disconnected
        , disconnectedAt
        , notify
        )

import Task exposing (Task)
import Tuple exposing (..)
import Time exposing (Time)
import StringUtils exposing (..)
import Postgres exposing (..)
import Proxy.Decoder exposing (..)
import ParentChildUpdate exposing (..)
import Websocket exposing (..)
import Respond exposing (..)
import Utils.Ops exposing (..)
import Utils.Log exposing (..)
import Utils.Error exposing (..)
import ConnectionManager
import Services.Common.Taggers as ServicesCommon exposing (..)
import DebugF exposing (..)


type alias Config msg =
    { wsPort : WSPort
    , pgConnectTimeout : Time
    , errorTagger : ServicesCommon.ErrorTagger String msg
    , logTagger : LogTagger String msg
    , sendErrorTagger : ( WSPort, ClientId, String, String ) -> msg
    , sentTagger : ( WSPort, ClientId, String ) -> msg
    , clientDestroyedTagger : ClientId -> msg
    , listenEventTagger : List ClientId -> ( ConnectionId, ListenChannel, String ) -> msg
    , debug : Bool
    }


type alias Model =
    { running : Bool
    , fatalError : Maybe String
    , disconnectedAt : Maybe Time
    , clientId : ClientId
    , listenRequest : Request
    }


init : ClientId -> Model
init clientId =
    { running = True
    , fatalError = Nothing
    , disconnectedAt = Nothing
    , clientId = clientId
    , listenRequest = ""
    }


stop : Model -> Model
stop model =
    { model | running = False }


connectionManagerConfig : Config msg -> ConnectionManager.Config Msg
connectionManagerConfig config =
    { routeToMeTagger = ConnectionManagerMsg
    , pgConnectTimeout = config.pgConnectTimeout
    , errorTagger = ConnectionManagerError
    , logTagger = ConnectionManagerLog
    , connectErrorTagger = ConnectError
    , connectedTagger = Connected
    , connectionLostTagger = ConnectionLost
    , disconnectErrorTagger = DisconnectError
    , disconnectedTagger = Disconnected
    , listenUnlistenErrorTagger = ListenUnlistenError
    , listenUnlistenSuccessTagger = ListenUnlistenSuccess
    , listenEventTagger = ListenEvent
    , debug = config.debug
    }


msgToCmd : msg -> Cmd msg
msgToCmd msg =
    Task.perform identity <| Task.succeed msg


withModel : Config msg -> Model -> ( ( ConnectionManager.Model Msg, Cmd Msg ), List Msg ) -> ( ( ( Model, ConnectionManager.Model Msg ), Cmd Msg ), List msg )
withModel config model ( ( connectionManagerModel, cmd ), myMsgs ) =
    myMsgs
        |> List.foldl
            (\msg ( model, connectionManagerModel, cmds, previousMsgs ) ->
                update config msg ( model, connectionManagerModel )
                    |> (\( ( ( model, connectionManagerModel ), cmd ), msgs ) -> ( model, connectionManagerModel, cmd :: cmds, List.append previousMsgs msgs ))
            )
            ( model, connectionManagerModel, [ cmd ], [] )
        |> (\( model, connectionManagerModel, cmds, msgs ) -> ( ( model, connectionManagerModel ) ! cmds, msgs ))


type alias Request =
    String


type Msg
    = SendError ( WSPort, ClientId, String, String )
    | Sent ( WSPort, ClientId, String )
    | ConnectionManagerError ( ErrorType, String )
    | ConnectionManagerLog ( LogLevel, String )
    | ConnectionManagerMsg ConnectionManager.Msg
    | ConnectError Request String
    | Connected Request
    | ConnectionLost ConnectionId String
    | DisconnectError (Maybe Request) ( ConnectionId, String )
    | Disconnected (Maybe Request) ConnectionId
    | ListenUnlistenError Request ( ConnectionId, String )
    | ListenUnlistenSuccess Request ( ConnectionId, ListenChannel, ListenUnlisten )
    | ListenEvent (List ClientId) ( ConnectionId, ListenChannel, String )
    | PGExecuteSqlError Request ( ConnectionId, String )
    | PGExecuteSqlSuccess Request ( ConnectionId, Int )
    | PGQueryError Request ( ConnectionId, String )
    | PGQuerySuccess Request ( ConnectionId, List String )
    | Destroyed
    | Request ProxyRequest Request


update : Config msg -> Msg -> ( Model, ConnectionManager.Model Msg ) -> ( ( ( Model, ConnectionManager.Model Msg ), Cmd Msg ), List msg )
update config msg ( model, connectionManagerModel ) =
    (config.debug ? ( DebugF.logFC Magenta Black "*** DEBUG:PGProxy Client msg" (cleanElmString <| toString msg), "" ))
        |> (\_ ->
                (let
                    respondMessage =
                        model.running ? ( Respond.respondMessage SendError Sent config.wsPort, (\_ _ _ _ _ _ -> Cmd.none) )

                    respondError =
                        model.running ? ( Respond.respondError SendError Sent config.wsPort, (\_ _ _ -> Cmd.none) )

                    respondSuccess =
                        model.running ? ( Respond.respondSuccess SendError Sent config.wsPort, (\_ _ -> Cmd.none) )

                    listeningOnConnection request model =
                        ( model ! [ respondError ("Operation NOT allowed since connection is used for listening") model.clientId request ], [] )

                    alreadyListeningOnConnection request model =
                        ( model ! [ respondError ("Operation NOT allowed since connection is ALREADY used for listening") model.clientId request ], [] )

                    notListeningToChannel request model =
                        ( model ! [ respondError ("Operation NOT allowed since connection is NOT listening to specified channel") model.clientId request ], [] )

                    notConnected request model =
                        ( model ! [ respondError ("Operation NOT allowed since not connected") model.clientId request ], [] )

                    updateConnectionManger =
                        updateChildParent (ConnectionManager.update (connectionManagerConfig config)) (update config) second ConnectionManagerMsg (\( model, _ ) connectionManagerModel -> ( model, connectionManagerModel ))
                 in
                    (\connectionManagerModel ( ( model, cmd ), msgs ) -> ( ( model, connectionManagerModel ) ! [ cmd ], msgs ))
                        |> (\withConnectionManagerModel ->
                                case msg of
                                    SendError ( wsPort, clientId, message, error ) ->
                                        ( { model | fatalError = Just message } ! [], [ config.sendErrorTagger ( wsPort, clientId, message, error ) ] )
                                            |> withConnectionManagerModel connectionManagerModel

                                    Sent ( wsPort, clientId, message ) ->
                                        ( model ! [], [ config.sentTagger ( wsPort, clientId, message ) ] )
                                            |> withConnectionManagerModel connectionManagerModel

                                    ConnectionManagerError ( errorType, error ) ->
                                        ( model ! [], [ config.errorTagger ( errorType, "ConnectionManger:" +-+ error ) ] )
                                            |> withConnectionManagerModel connectionManagerModel

                                    ConnectionManagerLog ( logLevel, message ) ->
                                        ( model ! [], [ config.logTagger ( logLevel, "ConnectionManger:" +-+ message ) ] )
                                            |> withConnectionManagerModel connectionManagerModel

                                    ConnectionManagerMsg msg ->
                                        updateConnectionManger msg ( model, connectionManagerModel )

                                    ConnectError request error ->
                                        ( model ! [ respondError ("Unable to connect to database:" +-+ error) model.clientId request ], [] )
                                            |> withConnectionManagerModel connectionManagerModel

                                    Connected request ->
                                        ( model ! [ respondSuccess model.clientId request ], [] )
                                            |> withConnectionManagerModel connectionManagerModel

                                    ConnectionLost connectionId error ->
                                        ConnectionManager.getClientIdsAndRequests connectionId connectionManagerModel
                                            |> List.map (\( clientId, request ) -> respondError "Lost connection to database" model.clientId request)
                                            |> (\cmds -> ( model ! cmds, [] ))
                                            |> withConnectionManagerModel connectionManagerModel

                                    DisconnectError maybeRequest ( connectionId, error ) ->
                                        model.running
                                            ? ( maybeRequest
                                                    |?> (\request -> ( model ! [ respondError ("Unable to disconnect from database:" +-+ error) model.clientId request ], [] ))
                                                    ?= ( model ! [], [] )
                                                    |> withConnectionManagerModel connectionManagerModel
                                              , update config Destroyed ( model, connectionManagerModel )
                                              )

                                    Disconnected maybeRequest connectionId ->
                                        model.running
                                            ? ( maybeRequest
                                                    |?> (\request -> ( model ! [ respondSuccess model.clientId request ], [] ))
                                                    ?= ( model ! [], [] )
                                                    |> withConnectionManagerModel connectionManagerModel
                                              , update config Destroyed ( model, connectionManagerModel )
                                              )

                                    ListenUnlistenError request ( connectionId, error ) ->
                                        ( model ! [ respondError error model.clientId request ], [] )
                                            |> withConnectionManagerModel connectionManagerModel

                                    ListenUnlistenSuccess request ( connectionId, channel, listenUnlisten ) ->
                                        ( model ! [ respondSuccess model.clientId request ], [] )
                                            |> withConnectionManagerModel connectionManagerModel

                                    PGExecuteSqlError request ( connectionId, error ) ->
                                        ( model ! [ respondError error model.clientId request ], [] )
                                            |> withConnectionManagerModel connectionManagerModel

                                    PGExecuteSqlSuccess request ( connectionId, count ) ->
                                        ( model ! [ respondMessage (Just True) False (Just ( "count", toString count )) Nothing model.clientId request ], [] )
                                            |> withConnectionManagerModel connectionManagerModel

                                    PGQueryError request ( connectionId, error ) ->
                                        ( model ! [ respondError error model.clientId request ], [] )
                                            |> withConnectionManagerModel connectionManagerModel

                                    PGQuerySuccess request ( connectionId, results ) ->
                                        (\records -> ", \"records\": [" ++ (String.join ", " <| List.map ((\s -> "\"" ++ s ++ "\"") << jsonStringEscape) records) ++ "]")
                                            |> (\formatRecords ->
                                                    ( model ! [ respondMessage (Just True) False Nothing (Just <| formatRecords results) model.clientId request ], [] )
                                                        |> withConnectionManagerModel connectionManagerModel
                                               )

                                    ListenEvent clientIds ( connectionId, channel, message ) ->
                                        ( model ! [], [ config.listenEventTagger clientIds ( connectionId, channel, message ) ] )
                                            |> withConnectionManagerModel connectionManagerModel

                                    Destroyed ->
                                        ( model ! [], [ config.clientDestroyedTagger model.clientId ] )
                                            |> withConnectionManagerModel connectionManagerModel

                                    Request proxyRequest request ->
                                        model.fatalError
                                            |?> (\error ->
                                                    ( model ! [ respondError ("Cannot honor request due to previous fatal error:" +-+ error) model.clientId request ], [] )
                                                        |> withConnectionManagerModel connectionManagerModel
                                                )
                                            ?= (case proxyRequest of
                                                    Connect connectRequest ->
                                                        ConnectionManager.connect (connectionManagerConfig config) connectRequest model.clientId request connectionManagerModel
                                                            |> withModel config model

                                                    Disconnect disconnectRequest ->
                                                        ConnectionManager.disconnect (connectionManagerConfig config) model.clientId disconnectRequest.discardConnection (Just request) connectionManagerModel
                                                            |> withModel config model

                                                    Query queryRequest ->
                                                        ConnectionManager.isNonListenConnection model.clientId connectionManagerModel
                                                            ? ( ConnectionManager.getConnectionId model.clientId connectionManagerModel
                                                                    |?> (\connectionId -> ( model ! [ Postgres.query (PGQueryError request) (PGQuerySuccess request) connectionId queryRequest.sql queryRequest.recordCount ], [] ))
                                                                    ?= notConnected request model
                                                              , listeningOnConnection request model
                                                              )
                                                            |> withConnectionManagerModel connectionManagerModel

                                                    MoreQueryResults moreQueryResultsRequest ->
                                                        ConnectionManager.isNonListenConnection model.clientId connectionManagerModel
                                                            ? ( ConnectionManager.getConnectionId model.clientId connectionManagerModel
                                                                    |?> (\connectionId -> ( model ! [ Postgres.moreQueryResults (PGQueryError request) (PGQuerySuccess request) connectionId ], [] ))
                                                                    ?= notConnected request model
                                                              , listeningOnConnection request model
                                                              )
                                                            |> withConnectionManagerModel connectionManagerModel

                                                    ExecuteSql executeSqlRequest ->
                                                        ConnectionManager.isNonListenConnection model.clientId connectionManagerModel
                                                            ? ( ConnectionManager.getConnectionId model.clientId connectionManagerModel
                                                                    |?> (\connectionId -> ( model ! [ Postgres.executeSql (PGExecuteSqlError request) (PGExecuteSqlSuccess request) connectionId executeSqlRequest.sql ], [] ))
                                                                    ?= notConnected request model
                                                              , listeningOnConnection request model
                                                              )
                                                            |> withConnectionManagerModel connectionManagerModel

                                                    Listen listenRequest ->
                                                        { model | listenRequest = request }
                                                            |> (\model ->
                                                                    ConnectionManager.isNonListenConnection model.clientId connectionManagerModel
                                                                        ? ( ConnectionManager.listen (connectionManagerConfig config) listenRequest.channel model.clientId request connectionManagerModel
                                                                                |> withModel config model
                                                                          , alreadyListeningOnConnection request model
                                                                                |> withConnectionManagerModel connectionManagerModel
                                                                          )
                                                               )

                                                    Unlisten unlistenRequest ->
                                                        ConnectionManager.isListeningOnChannel model.clientId unlistenRequest.channel connectionManagerModel
                                                            ? ( ConnectionManager.unlisten (connectionManagerConfig config) unlistenRequest.channel model.clientId request connectionManagerModel
                                                                    |> withModel config model
                                                              , notListeningToChannel request model
                                                                    |> withConnectionManagerModel connectionManagerModel
                                                              )

                                                    UnknownProxyRequest error ->
                                                        ( model ! [ respondError ("Unknown request: " ++ request ++ " Error: " ++ error) model.clientId request ], [] )
                                                            |> withConnectionManagerModel connectionManagerModel
                                               )
                           )
                )
           )


disconnected : Model -> Time -> Model
disconnected model time =
    { model | disconnectedAt = Just time, running = False }


disconnectedAt : Model -> Maybe Time
disconnectedAt model =
    model.disconnectedAt


destroyDisconnected : Config msg -> ( Model, ConnectionManager.Model Msg ) -> Bool -> (Msg -> msg) -> ( ( ( Model, ConnectionManager.Model Msg ), Cmd msg ), List msg )
destroyDisconnected config ( model, connectionManagerModel ) force routeToMeTagger =
    (force ? ( { model | running = False }, model ))
        |> (\model ->
                (disconnectedAt model |?> always True ?= force)
                    ?! ( \_ ->
                            ConnectionManager.getConnectionId model.clientId connectionManagerModel
                                |?> (\connectionId ->
                                        ConnectionManager.disconnect (connectionManagerConfig config) model.clientId True Nothing connectionManagerModel
                                            |> withModel config model
                                            |> (\( ( ( model, connectionManagerModel ), cmd ), msgs ) -> ( ( ( model, connectionManagerModel ), Cmd.map routeToMeTagger <| cmd ), msgs ))
                                    )
                                ?= ( ( model, ConnectionManager.removeClient model.clientId connectionManagerModel ) ! [ Cmd.map routeToMeTagger <| msgToCmd <| Destroyed ]
                                   , []
                                   )
                       , \_ -> Debug.crash ("BUG: Client was not disconnected (and not forced)" +-+ model)
                       )
           )


notify : Config msg -> Model -> (Msg -> msg) -> String -> Cmd msg
notify config model routeToMeTagger message =
    Respond.respondUnsolicited (routeToMeTagger << SendError) (routeToMeTagger << Sent) config.wsPort "notification" message model.clientId model.listenRequest
