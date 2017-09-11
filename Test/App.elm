port module Test.App exposing (..)

import Dict exposing (Dict)
import Platform
import Time
import String exposing (..)
import Websocket exposing (..)
import PGProxy
import ParentChildUpdate exposing (..)
import Utils.Ops exposing (..)
import Utils.Error exposing (..)
import Utils.Log exposing (..)
import DebugF exposing (..)


port exitApp : Float -> Cmd msg


port externalStop : (() -> msg) -> Sub msg


type ServerRunningStatus
    = Running
    | NotRunning


type alias Model =
    { status : ServerRunningStatus
    , serviceCount : Int
    , pgProxyModel : PGProxy.Model
    , pgProxyStartMsg : PGProxy.Msg
    , pgProxyStopMsg : PGProxy.Msg
    }


type alias SessionId =
    String


wsPort : WSPort
wsPort =
    8080


type alias SessionModel =
    {}


authenticate : SessionModel -> SessionId -> ( SessionModel, Bool )
authenticate _ _ =
    ( {}, True )


initModel : ( Model, List (Cmd Msg) )
initModel =
    let
        ( ( pgProxyModel, pgProxyCmd ), pgProxyStartMsg, pgProxyStopMsg ) =
            PGProxy.init pgProxyConfig PGProxyMsg
    in
        ( { status = NotRunning
          , serviceCount = 1
          , pgProxyModel = pgProxyModel
          , pgProxyStartMsg = pgProxyStartMsg
          , pgProxyStopMsg = pgProxyStopMsg
          }
        , [ pgProxyCmd ]
        )


pgProxyConfig : PGProxy.Config SessionModel Msg
pgProxyConfig =
    { authenticate = authenticate
    , wsPort = wsPort
    , path = "/pgproxy"
    , delayBeforeStop = 5 * Time.second
    , pgConnectTimeout = 15 * Time.second
    , errorTagger = PGProxyError
    , logTagger = PGProxyLog
    , startedMsg = PGProxyStarted
    , stoppedMsg = PGProxyStopped
    , garbageCollectDisconnectedClientsAfterPeriod = 5 * Time.second
    , debug = True
    , idleDumpStateFrequency = 5 * Time.second
    , hostMap = Dict.empty
    , portMap = Dict.empty
    , databaseMap = Dict.empty
    , userMap = Dict.empty
    , passwordMap = Dict.empty
    }


init : ( Model, Cmd Msg )
init =
    let
        ( model, servicesCmds ) =
            initModel
    in
        model ! (List.append servicesCmds [ Websocket.startServer ServerError ServerStatus UnhandledMessage Nothing Nothing wsPort ])


main : Program Never Model Msg
main =
    Platform.program
        { init = init
        , update = update
        , subscriptions = subscriptions
        }


type Msg
    = Nop
    | StopServer ()
    | ServerError ( WSPort, String )
    | ServerStatus ( WSPort, ServerStatus )
    | UnhandledMessage ( WSPort, Path, QueryString, ClientId, String )
    | PGProxyStarted (Result String ())
    | PGProxyStopped (Result String ())
    | PGProxyError ( ErrorType, String )
    | PGProxyLog ( LogLevel, String )
    | PGProxyMsg PGProxy.Msg


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    let
        serviceStopped model =
            let
                newModel =
                    { model | serviceCount = model.serviceCount - 1 }

                cmd =
                    case newModel.serviceCount of
                        0 ->
                            Websocket.stopServer ServerError ServerStatus wsPort

                        _ ->
                            Cmd.none
            in
                ( newModel, cmd )

        startStopServices model serviceUpdates =
            let
                ( newModel, cmds ) =
                    List.foldl
                        (\updateService ( model, cmds ) ->
                            let
                                ( newModel, cmd ) =
                                    updateService model
                            in
                                ( newModel, cmd :: cmds )
                        )
                        ( model, [] )
                        serviceUpdates
            in
                newModel ! cmds

        startServices model =
            startStopServices model
                [ updatePGProxy model.pgProxyStartMsg
                ]

        stopServices model =
            startStopServices model
                [ updatePGProxy model.pgProxyStopMsg
                ]

        updatePGProxy =
            updateChildApp (PGProxy.update pgProxyConfig) update (\model -> ( model.pgProxyModel, {} )) PGProxyMsg (\model ( pgProxyModel, _ ) -> { model | pgProxyModel = pgProxyModel })
    in
        case msg of
            Nop ->
                model ! []

            StopServer _ ->
                let
                    l =
                        Debug.log "Stopping Server" "..."
                in
                    stopServices model

            ServerError ( wsPort, error ) ->
                let
                    l =
                        Debug.log "ServerError" ( wsPort, error )
                in
                    model ! [ exitApp -1 ]

            ServerStatus ( wsPort, status ) ->
                let
                    l =
                        Debug.log "ServerStatus" ( wsPort, status )
                in
                    case status of
                        Started ->
                            startServices { model | status = Running }

                        Stopped ->
                            ( model, exitApp 1 )

            UnhandledMessage ( wsPort, path, queryString, clientId, message ) ->
                let
                    l =
                        Debug.log "UnhandledMessage" ( wsPort, path, queryString, clientId, message )
                in
                    model ! []

            PGProxyStarted result ->
                let
                    l =
                        Debug.log "PGProxyStarted" result
                in
                    model ! []

            PGProxyStopped result ->
                let
                    l =
                        Debug.log "PGProxyStopped" result
                in
                    serviceStopped model

            PGProxyError ( errorType, error ) ->
                let
                    fatalError =
                        errorType == FatalError

                    logIt =
                        fatalError ?! ( \_ -> Debug.crash "FATAL ERROR", \_ -> Debug.log "PGProxyError" )

                    l =
                        logIt error
                in
                    model ! []

            PGProxyLog ( level, message ) ->
                let
                    l =
                        DebugF.log "PGProxyLog" ((toUpper <| toString level) ++ ": " ++ message)
                in
                    model ! []

            PGProxyMsg msg ->
                updatePGProxy msg model


subscriptions : Model -> Sub Msg
subscriptions model =
    let
        stopServer =
            externalStop StopServer

        pgProxySubs =
            Sub.map PGProxyMsg <| PGProxy.subscriptions pgProxyConfig model.pgProxyModel
    in
        case model.status of
            Running ->
                Sub.batch [ stopServer, pgProxySubs ]

            NotRunning ->
                stopServer
