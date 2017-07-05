module PGProxy
    exposing
        ( init
        , update
        , subscriptions
        , Config
        , Model
        , Msg
        )

{-| Postgres Proxy

This is a proxy for Postgres Effects Manager on the client.

@docs init, update, subscriptions, Config, Model, Msg

-}

import Tuple
import Set exposing (Set)
import String
import StringUtils exposing (..)
import Time exposing (Time)
import Process
import List
import Task exposing (Task)
import Dict exposing (Dict)
import Json.Decode as JD exposing (..)
import ParentChildUpdate exposing (..)
import Websocket exposing (..)
import Respond exposing (..)
import Proxy.Decoder exposing (..)
import Postgres exposing (..)
import Utils.Error exposing (..)
import Utils.Log exposing (..)
import Utils.Ops exposing (..)
import Services.Common.Error as Error
import Services.Common.Taggers as ServicesCommon exposing (..)
import Client
import ConnectionManager
import DebugF exposing (..)


type alias SharedListenConnection =
    { connectionId : ConnectionId
    , clientIds : Set ClientId
    , subscription : Sub Msg
    }


{-| Model
-}
type alias Model =
    { version : Int
    , dumpVersion : Int
    , running : Bool
    , stopping : Bool
    , listenError : Bool
    , clients : Dict ClientId Client.Model
    , connectionManagerModel : ConnectionManager.Model Client.Msg
    , currentTime : Time
    , idleTime : Time
    , stopDisconnectingCount : Int
    }


{-| PGProxy's config
-}
type alias Config sessionModel msg =
    { authenticate : sessionModel -> SessionId -> ( sessionModel, Bool )
    , wsPort : WSPort
    , path : String
    , delayBeforeStop : Time
    , pgConnectTimeout : Time
    , errorTagger : ServicesCommon.ErrorTagger String msg
    , logTagger : LogTagger String msg
    , startedMsg : Result String () -> msg
    , stoppedMsg : Result String () -> msg
    , garbageCollectDisconnectedClientsAfterPeriod : Time
    , debug : Bool
    , idleDumpStateFrequency : Time
    , hostMap : Dict String String
    , portMap : Dict Int Int
    , databaseMap : Dict String String
    , userMap : Dict String String
    , passwordMap : Dict String String
    }


tickPeriod : Time
tickPeriod =
    1 * Time.second


type alias SessionId =
    String


clientConfig : Config sessionModel msg -> Client.Config Msg
clientConfig config =
    { wsPort = config.wsPort
    , pgConnectTimeout = config.pgConnectTimeout
    , errorTagger = ClientError
    , logTagger = ClientLog
    , sendErrorTagger = SendError
    , sentTagger = Sent
    , clientDestroyedTagger = ClientDestroyed
    , listenEventTagger = ListenEvent
    , debug = config.debug
    }


{-| Initialize the PGProxy
-}
init : Config sessionModel msg -> (Msg -> msg) -> ( ( Model, Cmd msg ), Msg, Msg )
init config tagger =
    ( { version = 0
      , dumpVersion = -1
      , running = False
      , stopping = False
      , listenError = False
      , clients = Dict.empty
      , connectionManagerModel = ConnectionManager.init
      , currentTime = 0
      , idleTime = 0
      , stopDisconnectingCount = 0
      }
        ! [ Postgres.debug config.debug ]
    , Start
    , Stop
    )


getClientModel : ClientId -> Model -> Maybe Client.Model
getClientModel clientId model =
    Dict.get clientId model.clients


delayMsg : Msg -> Time -> Cmd Msg
delayMsg msg delay =
    Task.perform (\_ -> msg) <| Process.sleep delay


msgToCmd : msg -> Cmd msg
msgToCmd msg =
    Task.perform identity <| Task.succeed msg


{-| PGProxy Msgs -
-}
type Msg
    = Tick Time
    | Start
    | Stop
    | DelayedStop
    | Stopped
    | ConnectionStatus ( WSPort, Path, ClientId, IPAddress, ConnectionStatus )
    | ClientError ( ErrorType, String )
    | ClientLog ( LogLevel, String )
    | ListenError ( WSPort, Path, String )
    | SendError ( WSPort, ClientId, String, String )
    | Sent ( WSPort, ClientId, String )
    | WSMessage ( ClientId, QueryString, String )
    | ClientMsg ClientId Client.Msg
    | ListenEvent (List ClientId) ( ConnectionId, ListenChannel, String )
    | ClientDestroyed ClientId


{-| PGProxy update
-}
update : Config sessionModel msg -> Msg -> ( Model, sessionModel ) -> ( ( ( Model, sessionModel ), Cmd Msg ), List msg )
update config msg ( model, sessionModel ) =
    (case msg of
        Tick _ ->
            model

        _ ->
            { model | version = model.version + 1 }
    )
        |> (\model ->
                let
                    debugMsg =
                        case msg of
                            Tick _ ->
                                ""

                            ClientLog _ ->
                                ""

                            ClientError _ ->
                                ""

                            _ ->
                                config.debug ? ( DebugF.logFC Cyan Black "*** DEBUG:PGProxy msg" (cleanElmString <| toString msg), "" )
                in
                    let
                        withSessionModel sessionModel ( ( model, cmd ), msgs ) =
                            ( ( ( model, sessionModel ), cmd ), msgs )

                        destroyClients model force clientIdAndModels =
                            clientIdAndModels
                                |> List.foldl
                                    (\( clientId, clientModel ) ( model, cmds, previousMsgs ) ->
                                        Client.destroyDisconnected (clientConfig config) ( clientModel, model.connectionManagerModel ) force (ClientMsg clientId)
                                            |> (\( ( ( clientModel, connectionManagerModel ), cmd ), msgs ) -> ( { model | clients = Dict.insert clientId clientModel model.clients, connectionManagerModel = connectionManagerModel }, cmd :: cmds, List.append previousMsgs msgs ))
                                    )
                                    ( model, [], [] )
                                |> (\( model, cmds, myMsgs ) ->
                                        myMsgs
                                            |> List.foldl
                                                (\msg ( ( model, sessionModel ), cmds, previousMsgs ) ->
                                                    update config msg ( model, sessionModel )
                                                        |> (\( ( ( model, sessionModel ), cmd ), msgs ) -> ( ( model, sessionModel ), cmd :: cmds, List.append previousMsgs msgs ))
                                                )
                                                ( ( model, sessionModel ), cmds, [] )
                                            |> (\( ( model, sessionModel ), cmds, msgs ) -> ( ( model, sessionModel ) ! cmds, msgs ))
                                   )

                        updateClient clientId clientModel =
                            updateChildParent (Client.update (clientConfig config))
                                (update config)
                                (\( model, _ ) -> ( clientModel, model.connectionManagerModel ))
                                (ClientMsg clientId)
                                (\( model, sessionModel ) ( clientModel, connectionManagerModel ) -> ( { model | clients = Dict.insert clientId clientModel model.clients, connectionManagerModel = connectionManagerModel }, sessionModel ))

                        debugDumpState model =
                            (config.debug && model.version /= model.dumpVersion)
                                ? ( (DebugF.toStringF model)
                                        |> cleanElmString
                                        |> DebugF.log "*** DEBUG:PGProxy model"
                                  , ""
                                  )

                        debugDumpAllStates model =
                            (config.debug && model.version /= model.dumpVersion)
                                ?! ( \_ ->
                                        debugDumpState model
                                            |> (always Postgres.dumpState)
                                   , always Cmd.none
                                   )
                                |> (\cmd -> ( { model | dumpVersion = model.version }, cmd ))
                    in
                        (case msg of
                            Tick _ ->
                                { model | idleTime = model.idleTime + tickPeriod }

                            _ ->
                                { model | idleTime = 0 }
                        )
                            |> (\model ->
                                    case msg of
                                        Tick time ->
                                            { model | currentTime = time }
                                                |> (\model ->
                                                        (((model.idleTime >= config.idleDumpStateFrequency) ?! ( \_ -> debugDumpAllStates { model | idleTime = 0 }, \_ -> ( model, Cmd.none ) ))
                                                            |> (\( model, cmd ) -> ( model ! [ cmd ], [] ))
                                                            |> withSessionModel sessionModel
                                                        )
                                                            |> (\( ( ( model, sessionModel ), dumpCmd ), dumpMsgs ) ->
                                                                    model.clients
                                                                        |> Dict.toList
                                                                        |> List.filterMap (\( clientId, clientModel ) -> Client.disconnectedAt clientModel |?> (\disconnectedAt -> ( clientId, disconnectedAt )))
                                                                        |> (List.foldl
                                                                                (\( clientId, disconnectedAt ) ( model, cmds ) ->
                                                                                    (time - disconnectedAt >= config.garbageCollectDisconnectedClientsAfterPeriod)
                                                                                        ? ( ( model, (msgToCmd <| ClientDestroyed clientId) :: cmds )
                                                                                          , ( model, [] )
                                                                                          )
                                                                                )
                                                                                ( model, [ dumpCmd ] )
                                                                           )
                                                                        |> (\( model, cmds ) -> ( (( model, sessionModel ) ! cmds), dumpMsgs ))
                                                               )
                                                   )

                                        Start ->
                                            ( { model | running = True } ! [], [ config.startedMsg <| Ok () ] )
                                                |> withSessionModel sessionModel

                                        Stop ->
                                            model.clients
                                                |> Dict.map (\clientId clientModel -> Client.stop clientModel)
                                                |> (\clients -> { model | clients = clients })
                                                |> (\model ->
                                                        ConnectionManager.stop model.connectionManagerModel
                                                            |> (\connectionManagerModel -> { model | connectionManagerModel = connectionManagerModel })
                                                            |> (\model ->
                                                                    ( { model | running = False, stopping = True } ! [ delayMsg DelayedStop config.delayBeforeStop ], [] )
                                                                        |> withSessionModel sessionModel
                                                               )
                                                   )

                                        DelayedStop ->
                                            Dict.toList model.clients
                                                |> (\clientIdAndModels ->
                                                        (clientIdAndModels == [])
                                                            ?! ( (\_ -> update config Stopped ( model, sessionModel ))
                                                               , (\_ ->
                                                                    clientIdAndModels
                                                                        |> List.map (Tuple.mapSecond (\clientModel -> Client.disconnected clientModel model.currentTime))
                                                                        |> destroyClients model True
                                                                 )
                                                               )
                                                   )

                                        ClientDestroyed clientId ->
                                            { model | clients = Dict.remove clientId model.clients }
                                                |> (\model ->
                                                        (Dict.toList model.clients == [] && not model.running)
                                                            ?! ( (\_ -> update config Stopped ( model, sessionModel ))
                                                               , (\_ -> ( ( model, sessionModel ) ! [], [] ))
                                                               )
                                                   )

                                        Stopped ->
                                            ( model ! [], [ config.stoppedMsg <| Ok () ] )
                                                |> withSessionModel sessionModel
                                                |> (\( ( ( model, sessionModel ), cmd ), msgs ) ->
                                                        debugDumpAllStates model
                                                            |> (\( model, cmd ) -> ( ( model, sessionModel ) ! [ cmd ], msgs ))
                                                   )

                                        ConnectionStatus ( wsPort, path, clientId, ipAddress, status ) ->
                                            config.logTagger ( LogLevelDebug, String.join " " [ toString status, "from ipAddress: ", ipAddress, " on port: ", toString wsPort, " on path: ", toString path, " for clientId: ", toString clientId ] )
                                                |> (\logMsg ->
                                                        case status of
                                                            Connected ->
                                                                ( { model | clients = Dict.insert clientId (Client.init clientId) model.clients } ! [], [ logMsg ] )
                                                                    |> withSessionModel sessionModel

                                                            Disconnected ->
                                                                getClientModel clientId model
                                                                    |?> (\clientModel ->
                                                                            model.clients
                                                                                |> Dict.filter (\dictClientId _ -> dictClientId == clientId)
                                                                                |> Dict.toList
                                                                                |> List.map (Tuple.mapSecond (\clientModel -> Client.disconnected clientModel model.currentTime))
                                                                                |> destroyClients model False
                                                                        )
                                                                    ?= ( ( model, sessionModel ) ! [], [] )
                                                   )

                                        ClientError ( errorType, error ) ->
                                            ( model ! [], [ config.errorTagger ( errorType, "Client:" +-+ error ) ] )
                                                |> withSessionModel sessionModel

                                        ClientLog ( logLevel, message ) ->
                                            ( model ! [], [ config.logTagger ( logLevel, "Client:" +-+ message ) ] )
                                                |> withSessionModel sessionModel

                                        ListenError ( wsPort, path, error ) ->
                                            ( { model | listenError = True } ! [], [ config.errorTagger ( FatalError, ("Unable to listen to websocket on port:" +-+ wsPort +-+ "for path:" +-+ path +-+ "error:" +-+ error) ) ] )
                                                |> withSessionModel sessionModel

                                        SendError ( wsPort, clientId, message, error ) ->
                                            ("Unable to send:" +-+ message +-+ "to websocket on port:" +-+ wsPort +-+ "for clientId:" +-+ clientId +-+ "error:" +-+ error)
                                                |> (\errorMsg ->
                                                        ( model ! [], [ config.errorTagger ( NonFatalError, errorMsg ) ] )
                                                            |> withSessionModel sessionModel
                                                   )

                                        Sent ( wsPort, clientId, message ) ->
                                            ( model ! [], [] )
                                                |> withSessionModel sessionModel

                                        WSMessage ( clientId, _, request ) ->
                                            not model.stopping
                                                ? ( decodeRequest request
                                                        |> (\( type_, decodeResult ) ->
                                                                ( decodeResult ??= (\err -> UnknownProxyRequest err), Dict.get clientId model.clients )
                                                                    |> (\( unmappedProxyRequest, maybeClientModel ) ->
                                                                            (case unmappedProxyRequest of
                                                                                Connect request ->
                                                                                    Connect
                                                                                        { request
                                                                                            | host = Dict.get request.host config.hostMap ?= "invalid"
                                                                                            , port_ = Dict.get request.port_ config.portMap ?= 0
                                                                                            , database = Dict.get request.database config.databaseMap ?= "invalid"
                                                                                            , user = Dict.get request.user config.userMap ?= "invalid"
                                                                                            , password = Dict.get request.password config.passwordMap ?= "invalid"
                                                                                        }

                                                                                _ ->
                                                                                    unmappedProxyRequest
                                                                            )
                                                                                |> (\proxyRequest ->
                                                                                        maybeClientModel
                                                                                            |?> (\clientModel ->
                                                                                                    (config.authenticate sessionModel <| getSessionId request)
                                                                                                        |> (\( sessionModel, authenticated ) ->
                                                                                                                authenticated
                                                                                                                    ? ( getClientModel clientId model
                                                                                                                            |?> (\clientModel -> updateClient clientId clientModel (Client.Request proxyRequest request) ( model, sessionModel ))
                                                                                                                            ?= (( model ! [], [] ) |> withSessionModel sessionModel)
                                                                                                                      , (( model ! [ respondError SendError Sent config.wsPort Error.invalidSession clientId request ], [] )
                                                                                                                            |> withSessionModel sessionModel
                                                                                                                        )
                                                                                                                      )
                                                                                                           )
                                                                                                )
                                                                                            ?= (( model ! [ respondError SendError Sent config.wsPort Error.invalidSession clientId request ], [] )
                                                                                                    |> withSessionModel sessionModel
                                                                                               )
                                                                                   )
                                                                       )
                                                           )
                                                  , ( model ! [], [ config.logTagger ( LogLevelInfo, "Ignoring request:" +-+ request +-+ "since shutting down" ) ] )
                                                        |> withSessionModel sessionModel
                                                  )

                                        ListenEvent clientIds ( connectionId, channel, message ) ->
                                            clientIds
                                                |> List.filterMap
                                                    (\clientId ->
                                                        getClientModel clientId model
                                                            |?> (\clientModel -> Just ( clientId, clientModel ))
                                                            ?= Nothing
                                                    )
                                                |> List.map (\( clientId, clientModel ) -> Client.notify (clientConfig config) clientModel (ClientMsg clientId) message)
                                                |> (\cmds -> ( model ! cmds, [] ))
                                                |> withSessionModel sessionModel

                                        ClientMsg clientId msg ->
                                            getClientModel clientId model
                                                |?> (\clientModel -> updateClient clientId clientModel msg ( model, sessionModel ))
                                                ?= (ConnectionManager.removeClient clientId model.connectionManagerModel
                                                        |> (\connectionManagerModel ->
                                                                ( { model | connectionManagerModel = connectionManagerModel } ! [], [ config.logTagger ( LogLevelInfo, "Ignoring message for non-existent clientId:" +-+ clientId ) ] )
                                                                    |> withSessionModel sessionModel
                                                           )
                                                   )
                               )
           )


getSessionId : String -> SessionId
getSessionId json =
    JD.decodeString (field "sessionId" string) json ??= always Error.invalidSession


{-| subscriptions
-}
subscriptions : Config sessionModel msg -> Model -> Sub Msg
subscriptions config model =
    Websocket.listen ListenError WSMessage ConnectionStatus config.wsPort config.path
        |> (\websocketSub ->
                Sub.batch
                    [ Time.every tickPeriod Tick
                    , model.running
                        ? ( model.listenError
                                ? ( Sub.none
                                  , Sub.batch <| websocketSub :: (List.map (\( clientId, subscription ) -> Sub.map (ClientMsg clientId) subscription) <| ConnectionManager.subscriptions model.connectionManagerModel)
                                  )
                          , websocketSub
                          )
                    ]
           )
