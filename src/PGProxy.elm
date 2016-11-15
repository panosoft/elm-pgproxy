module PGProxy
    exposing
        ( initModel
        , update
        , subscriptions
        , Model
        , Msg
        )

{-| Postgres Proxy

This is a proxy for Postgres Effects Manager on the client.

@docs initModel, update, subscriptions, Model, Msg

-}

import String
import Time exposing (Time)
import Process
import List
import Task exposing (Task)
import Dict exposing (Dict)
import Regex exposing (..)
import Json.Decode as JD exposing (..)
import Websocket exposing (..)
import Proxy.Decoder exposing (..)
import Postgres exposing (..)


type alias ClientState =
    { fatalError : Maybe String
    , lastJson : Maybe String
    , connectionId : Maybe ConnectionId
    }


type alias Response =
    String


type alias ResponseType =
    String


type alias ClientDict =
    Dict ClientId ClientState


type ServiceRunningStatus
    = Running
    | NotRunning


type alias ListenDict =
    Dict ( ClientId, ListenChannel ) (Sub Msg)


{-| Model
-}
type alias Model msg =
    { authenticate : SessionId -> Bool
    , wsPort : WSPort
    , errorTagger : String -> msg
    , logTagger : String -> msg
    , startedMsg : msg
    , stoppedMsg : msg
    , running : ServiceRunningStatus
    , listenError : Bool
    , clients : ClientDict
    , listens : ListenDict
    }


type alias SessionId =
    String


pgConnectTimeout : Int
pgConnectTimeout =
    15000


path : String
path =
    "/pgproxy"


(?=) : Maybe a -> a -> a
(?=) =
    flip Maybe.withDefault


(|?>) : Maybe a -> (a -> b) -> Maybe b
(|?>) =
    flip Maybe.map


{-| lazy version of ?= operator
-}
(?!=) : Maybe a -> (() -> a) -> a
(?!=) maybe lazy =
    case maybe of
        Just x ->
            x

        Nothing ->
            lazy ()


(&>) : Task x a -> Task x b -> Task x b
(&>) t1 t2 =
    t1 `Task.andThen` \_ -> t2


(///) : Result err value -> (err -> value) -> value
(///) result f =
    case result of
        Ok value ->
            value

        Err err ->
            f err


jsonStringEscape : String -> String
jsonStringEscape string =
    let
        replace old new =
            Regex.replace All (regex old) (\_ -> new)
    in
        string
            |> replace "\\t" "\\\\t"
            |> replace "\\n" "\\\\n"
            |> replace "\"" "\\\""


log : Model msg -> String -> msg
log model message =
    model.logTagger message


{-| Initialize the PGProxy's model

    Usage:
        PGProxy.initModel PGError PGLog PGProxyStarted PGProxyStopped authenticate wsPort

    where:
        PGError, PGLog, PGProxyStarted and PGProxyStopped are your application messages
        authenticate is a function that takes a sessionId and authenticates that the session is valid
        wsPort is the Websocket Port to Listen on. The path is '/pgproxy'
-}
initModel : (String -> msg) -> (String -> msg) -> msg -> msg -> (SessionId -> Bool) -> WSPort -> ( Model msg, Msg, Msg )
initModel errorTagger logTagger startedMsg stoppedMsg authenticate wsPort =
    ( { authenticate = authenticate
      , wsPort = wsPort
      , errorTagger = errorTagger
      , logTagger = logTagger
      , startedMsg = startedMsg
      , stoppedMsg = stoppedMsg
      , running = NotRunning
      , listenError = False
      , clients = Dict.empty
      , listens = Dict.empty
      }
    , Start
    , Stop
    )


initClientState : ClientState
initClientState =
    ClientState Nothing Nothing Nothing


getClientState : Model msg -> ClientId -> ClientState
getClientState model clientId =
    let
        maybeClientState =
            Dict.get clientId model.clients

        crash =
            case maybeClientState of
                Nothing ->
                    Debug.crash <| "Client id: " ++ (toString clientId) ++ " is not in state"

                Just _ ->
                    ""
    in
        maybeClientState ?= initClientState


setClientField : ClientId -> (ClientState -> ClientState) -> Model msg -> Model msg
setClientField clientId mutateClientState model =
    let
        clientState =
            getClientState model clientId

        newClients =
            Dict.insert clientId (mutateClientState clientState) model.clients
    in
        { model | clients = newClients }


setFatalError : ClientId -> String -> Model msg -> Model msg
setFatalError clientId error =
    setClientField clientId (\clientState -> { clientState | fatalError = Just error })


setLastJson : ClientId -> String -> Model msg -> Model msg
setLastJson clientId json =
    setClientField clientId (\clientState -> { clientState | lastJson = Just json })


setConnectionId : ClientId -> ConnectionId -> Model msg -> Model msg
setConnectionId clientId connectionId =
    setClientField clientId (\clientState -> { clientState | connectionId = Just connectionId })


clearConnectionId : ClientId -> Model msg -> Model msg
clearConnectionId clientId =
    setClientField clientId (\clientState -> { clientState | connectionId = Nothing })


respond : ClientId -> Model msg -> Response -> ( Model msg, Cmd Msg )
respond clientId model =
    (,) model << Websocket.send SendError Sent model.wsPort clientId


getRequestId : String -> String
getRequestId request =
    case JD.decodeString ("requestId" := int) request of
        Ok requestId ->
            toString requestId

        Err _ ->
            "Missing request Id"


respondMessage : Maybe Bool -> Bool -> Maybe ( String, String ) -> Maybe String -> ResponseType -> ClientId -> Model msg -> ( Model msg, Cmd Msg )
respondMessage maybeSuccess unsolicited maybeKeyValuePair keysFormatted type_ clientId model =
    let
        clientState =
            getClientState model clientId

        unsolicitedMessage =
            case unsolicited of
                True ->
                    ", \"unsolicited\": true"

                False ->
                    ""

        typeMessage =
            case unsolicited of
                False ->
                    ", \"type\": \"" ++ type_ ++ "\""

                True ->
                    ""

        successMessage success =
            ", \"success\": "
                ++ case success of
                    True ->
                        "true"

                    False ->
                        "false"

        keyMessage =
            maybeKeyValuePair
                |?> (\( key, message ) -> (", \"" ++ key ++ "\": \"" ++ (jsonStringEscape message) ++ "\""))
                ?= ""

        responseFormatted =
            ("{\"requestId\": " ++ (getRequestId <| clientState.lastJson ?= ""))
                ++ typeMessage
                ++ unsolicitedMessage
                ++ (maybeSuccess |?> successMessage ?= "")
                ++ (keysFormatted ?= keyMessage)
                ++ "}"
    in
        respond clientId model responseFormatted


respondError : String -> ResponseType -> ClientId -> Model msg -> ( Model msg, Cmd Msg )
respondError message =
    respondMessage (Just False) False (Just ( "error", message )) Nothing


respondUnsolicited : String -> String -> ResponseType -> ClientId -> Model msg -> ( Model msg, Cmd Msg )
respondUnsolicited key message =
    respondMessage Nothing True (Just ( key, message )) Nothing


respondSimpleSuccess : ResponseType -> ClientId -> Model msg -> ( Model msg, Cmd Msg )
respondSimpleSuccess =
    respondMessage (Just True) False Nothing Nothing


delayCmd : Msg -> Time -> Cmd Msg
delayCmd msg delay =
    Task.perform (\_ -> Nop) (\_ -> msg) <| Process.sleep delay


{-| PGProxy Msgs -
-}
type Msg
    = Nop
    | Start
    | Stop
    | Stopping
    | ConnectionStatus ( WSPort, ClientId, IPAddress, ConnectionStatus )
    | ListenError ( WSPort, Path, String )
    | WSMessage ( ClientId, QueryString, String )
    | SendError ( WSPort, ClientId, String )
    | Sent ( WSPort, ClientId, String )
    | PGConnectError ClientId ( ConnectionId, String )
    | PGConnected ClientId ConnectionId
    | PGConnectionLost ClientId ( ConnectionId, String )
    | PGDisconnectError ClientId ( ConnectionId, String )
    | PGDisconnected ClientId ConnectionId
    | PGExecuteSqlError ClientId ( ConnectionId, String )
    | PGExecuteSqlSuccess ClientId ( ConnectionId, Int )
    | PGQueryError String ClientId ( ConnectionId, String )
    | PGQuerySuccess String ClientId ( ConnectionId, List String )
    | PGListenError ClientId ListenUnlisten ( ConnectionId, String )
    | PGListenSuccess ClientId ( ConnectionId, ListenChannel, ListenUnlisten )
    | PGListenEvent ClientId ( ConnectionId, ListenChannel, String )
    | InternalDisconnectError ClientId ( ConnectionId, String )
    | InternalDisconnected ClientId ConnectionId


{-| PGProxy update
-}
update : Msg -> Model msg -> ( ( Model msg, Cmd Msg ), List msg )
update msg model =
    let
        removeClient clientId model =
            let
                newListens =
                    (Dict.keys model.listens)
                        |> List.filter (\( id, _ ) -> id == clientId)
                        |> List.foldl (\key listens -> Dict.remove key listens) model.listens
            in
                { model
                    | clients = Dict.remove clientId model.clients
                    , listens = newListens
                }
    in
        case msg of
            Nop ->
                ( model ! [], [] )

            Start ->
                ( { model | running = Running } ! [], [ model.startedMsg ] )

            Stop ->
                ( model ! [ delayCmd Stopping 5000 ], [] )

            Stopping ->
                let
                    newModel =
                        List.foldl (\clientId model -> removeClient clientId model) model <| Dict.keys model.clients
                in
                    ( { newModel | running = NotRunning } ! [], [ model.stoppedMsg ] )

            ConnectionStatus ( wsPort, clientId, ipAddress, status ) ->
                let
                    logCmd =
                        log model <| String.join " " [ toString status, "from ipAddress:", ipAddress, " on port:", toString wsPort, "for clientId:", toString clientId ]
                in
                    ( (case status of
                        Connected ->
                            { model | clients = Dict.insert clientId initClientState model.clients } ! []

                        Disconnected ->
                            let
                                maybeConnectionId =
                                    Dict.get clientId model.clients
                                        |?> .connectionId
                                        ?= Nothing

                                pgDisconnect =
                                    maybeConnectionId
                                        |?> (\connectionId -> Postgres.disconnect (InternalDisconnectError clientId) (InternalDisconnected clientId) connectionId True)
                                        ?= Cmd.none
                            in
                                removeClient clientId model ! [ pgDisconnect ]
                      )
                    , [ logCmd ]
                    )

            InternalDisconnectError clientId ( connectionId, error ) ->
                ( model ! [], [ log model <| "Internal Disconnect Error: " ++ toString ( clientId, connectionId, error ) ] )

            InternalDisconnected clientId connectionId ->
                ( model ! [], [ log model <| "Internal Disconnect Complete for clientId" ++ toString clientId ] )

            ListenError ( wsPort, path, error ) ->
                ( { model | listenError = True } ! [], [ model.errorTagger ("Unable to listen to websocket on port: " ++ (toString model.wsPort) ++ " for path: " ++ path ++ " error: " ++ error) ] )

            SendError ( wsPort, clientId, error ) ->
                let
                    errorMsg =
                        ("Unable to send to websocket on port: ") ++ (toString model.wsPort) ++ " for clientId: " ++ (toString clientId) ++ " error: " ++ error
                in
                    ( setFatalError clientId errorMsg model ! [], [ model.errorTagger errorMsg ] )

            Sent ( wsPort, clientId, message ) ->
                ( model ! [], [] )

            WSMessage ( clientId, _, json ) ->
                let
                    ( type_, decodeResult ) =
                        decodeRequest json

                    request =
                        decodeResult /// (\err -> UnknownProxyRequest err)

                    clientState =
                        getClientState model clientId

                    {- If there is a fatal error for this client NO other requests can be honored -}
                    ( newModel, cmd ) =
                        clientState.fatalError
                            |?> (\fatalError -> respondError fatalError type_ clientId model)
                            ?= handleRequest model clientId json request clientState
                in
                    ( setLastJson clientId json newModel ! [ cmd ], [] )

            PGConnectError clientId ( connectionId, error ) ->
                ( respondError error "connect" clientId model, [] )

            PGConnected clientId connectionId ->
                ( respondSimpleSuccess "connect" clientId <| setConnectionId clientId connectionId model, [] )

            PGConnectionLost clientId ( connectionId, error ) ->
                ( respondUnsolicited "connectionLostError" error "connect" clientId model, [] )

            PGDisconnectError clientId ( connectionId, error ) ->
                ( respondError error "disconnect" clientId model, [] )

            PGDisconnected clientId connectionId ->
                ( respondSimpleSuccess "disconnect" clientId <| clearConnectionId clientId model, [] )

            PGExecuteSqlError clientId ( connectionId, error ) ->
                ( respondError error "executeSql" clientId model, [] )

            PGExecuteSqlSuccess clientId ( connectionId, count ) ->
                ( respondMessage (Just True) False (Just ( "count", toString count )) Nothing "executeSql" clientId model, [] )

            PGQueryError type_ clientId ( connectionId, error ) ->
                ( respondError error type_ clientId model, [] )

            PGQuerySuccess type_ clientId ( connectionId, results ) ->
                let
                    formatRecords records =
                        let
                            quote s =
                                "\"" ++ s ++ "\""
                        in
                            ", \"records\": [" ++ (String.join ", " <| List.map (quote << jsonStringEscape) records) ++ "]"
                in
                    ( respondMessage (Just True) False Nothing (Just <| formatRecords results) type_ clientId model, [] )

            PGListenError clientId listenErrorType ( connectionId, error ) ->
                let
                    type_ =
                        case listenErrorType of
                            ListenType ->
                                "listen"

                            UnlistenType ->
                                "unlisten"
                in
                    ( respondError error type_ clientId model, [] )

            PGListenSuccess clientId ( connectionId, channel, type_ ) ->
                case type_ of
                    ListenType ->
                        ( respondSimpleSuccess "listen" clientId model, [] )

                    UnlistenType ->
                        ( respondSimpleSuccess "unlisten" clientId model, [] )

            PGListenEvent clientId ( connectionId, channel, message ) ->
                ( respondUnsolicited "notification" message "listen" clientId model, [] )


getSessionId : String -> SessionId
getSessionId request =
    case JD.decodeString ("sessionId" := string) request of
        Ok sessionId ->
            toString sessionId

        Err _ ->
            "Missing session Id"


handleRequest : Model msg -> ClientId -> String -> ProxyRequest -> ClientState -> ( Model msg, Cmd Msg )
handleRequest model clientId json request clientState =
    let
        connectionId =
            clientState.connectionId ?= -1

        withConnectionId clientId model type_ cmd =
            case connectionId of
                -1 ->
                    respondError "Connection does not exist" type_ clientId model

                _ ->
                    ( model, cmd )
    in
        case model.running of
            NotRunning ->
                (model ! [])

            Running ->
                case request of
                    Connect connectionRequest ->
                        ( model, Postgres.connect (PGConnectError clientId) (PGConnected clientId) (PGConnectionLost clientId) pgConnectTimeout connectionRequest.host connectionRequest.port_ connectionRequest.database connectionRequest.user connectionRequest.password )

                    Disconnect disconnectionRequest ->
                        withConnectionId clientId model "disconnect" <| Postgres.disconnect (PGDisconnectError clientId) (PGDisconnected clientId) connectionId disconnectionRequest.discardConnection

                    Query queryRequest ->
                        withConnectionId clientId model "query" <| Postgres.query (PGQueryError "query" clientId) (PGQuerySuccess "query" clientId) connectionId queryRequest.sql queryRequest.recordCount

                    MoreQueryResults moreQueryResultsRequest ->
                        withConnectionId clientId model "moreQueryResults" <| Postgres.moreQueryResults (PGQueryError "moreQueryResults" clientId) (PGQuerySuccess "moreQueryResults" clientId) connectionId

                    ExecuteSql executeSqlRequest ->
                        withConnectionId clientId model "executeSql" <| Postgres.executeSql (PGExecuteSqlError clientId) (PGExecuteSqlSuccess clientId) connectionId executeSqlRequest.sql

                    Listen listenRequest ->
                        let
                            sub =
                                Postgres.listen (PGListenError clientId ListenType) (PGListenSuccess clientId) (PGListenEvent clientId) connectionId listenRequest.channel

                            ( finalModel, cmd ) =
                                Dict.get ( clientId, listenRequest.channel ) model.listens
                                    |?> (\_ -> respondError ("Listen already exists for Channel: " ++ listenRequest.channel) "listen" clientId model)
                                    ?= ( { model | listens = Dict.insert ( clientId, listenRequest.channel ) sub model.listens }, Cmd.none )
                        in
                            withConnectionId clientId finalModel "listen" cmd

                    Unlisten unlistenRequest ->
                        let
                            ( finalModel, cmd ) =
                                Dict.get ( clientId, unlistenRequest.channel ) model.listens
                                    |?> (\_ -> ( { model | listens = Dict.remove ( clientId, unlistenRequest.channel ) model.listens }, Cmd.none ))
                                    ?= respondError ("Listen does not exists for Channel: " ++ unlistenRequest.channel) "unlisten" clientId model
                        in
                            withConnectionId clientId finalModel "unlisten" cmd

                    UnknownProxyRequest error ->
                        respond clientId model <| Debug.crash "Unknown request: " ++ json ++ " Error: " ++ error


{-| subscriptions
-}
subscriptions : Model msg -> Sub Msg
subscriptions model =
    case model.running of
        Running ->
            case not model.listenError of
                True ->
                    Sub.batch <| Websocket.listen ListenError WSMessage ConnectionStatus model.wsPort path :: Dict.values model.listens

                False ->
                    Sub.none

        NotRunning ->
            Sub.none
