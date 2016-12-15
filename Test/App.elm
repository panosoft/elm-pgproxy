port module Test.App exposing (..)

import Html exposing (..)
import Html.App
import Websocket exposing (..)
import PGProxy
import ParentChildUpdate exposing (..)


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


type Msg
    = Nop
    | StopServer ()
    | ServerError ( WSPort, String )
    | ServerStatus ( WSPort, ServerStatus )
    | UnhandledMessage ( WSPort, Path, QueryString, ClientId, String )
    | PGProxyStarted
    | PGProxyStopped
    | PGProxyError String
    | PGProxyLog String
    | PGProxyModule PGProxy.Msg


(?=) : Maybe a -> a -> a
(?=) =
    flip Maybe.withDefault


(|?>) : Maybe a -> (a -> b) -> Maybe b
(|?>) =
    flip Maybe.map


authenticate : SessionId -> Bool
authenticate _ =
    True


initModel : ( Model, List (Cmd Msg) )
initModel =
    let
        ( ( pgProxyModel, pgProxyCmd ), pgProxyStartMsg, pgProxyStopMsg ) =
            PGProxy.init
    in
        ( { status = NotRunning
          , serviceCount = 1
          , pgProxyModel = pgProxyModel
          , pgProxyStartMsg = pgProxyStartMsg
          , pgProxyStopMsg = pgProxyStopMsg
          }
        , [ Cmd.map PGProxyModule pgProxyCmd ]
        )


pgProxyConfig : PGProxy.Config Msg
pgProxyConfig =
    { authenticate = authenticate
    , wsPort = wsPort
    , errorTagger = PGProxyError
    , logTagger = PGProxyLog
    , startedMsg = PGProxyStarted
    , stoppedMsg = PGProxyStopped
    }


init : ( Model, Cmd Msg )
init =
    let
        ( model, servicesCmds ) =
            initModel
    in
        model ! (List.append servicesCmds [ Websocket.startServer ServerError ServerStatus UnhandledMessage Nothing Nothing wsPort ])


main : Program Never
main =
    Html.App.program
        { init = init
        , view = (\_ -> text "")
        , update = update
        , subscriptions = subscriptions
        }


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    let
        serviceStopped : Model -> ( Model, Cmd Msg )
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

        startStopServices : Model -> List (Model -> ( Model, Cmd Msg )) -> ( Model, Cmd Msg )
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

        startServices : Model -> ( Model, Cmd Msg )
        startServices model =
            startStopServices model
                [ updatePGProxy model.pgProxyStartMsg
                ]

        stopServices : Model -> ( Model, Cmd Msg )
        stopServices model =
            startStopServices model
                [ updatePGProxy model.pgProxyStopMsg
                ]

        updatePGProxy : PGProxy.Msg -> Model -> ( Model, Cmd Msg )
        updatePGProxy =
            ParentChildUpdate.updateChildApp (PGProxy.update pgProxyConfig) update .pgProxyModel PGProxyModule (\model pgProxyModel -> { model | pgProxyModel = pgProxyModel })
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

                    newModel =
                        { model | status = Running }
                in
                    case status of
                        Started ->
                            startServices newModel

                        Stopped ->
                            ( model, exitApp 1 )

            UnhandledMessage ( wsPort, path, queryString, clientId, message ) ->
                let
                    l =
                        Debug.log "UnhandledMessage" ( wsPort, path, queryString, clientId, message )
                in
                    model ! []

            PGProxyStarted ->
                let
                    l =
                        Debug.log "PGProxyStarted" ""
                in
                    model ! []

            PGProxyStopped ->
                let
                    l =
                        Debug.log "PGProxyStopped" ""
                in
                    serviceStopped model

            PGProxyError error ->
                let
                    l =
                        Debug.log "PGProxyError" error
                in
                    model ! []

            PGProxyLog message ->
                let
                    l =
                        Debug.log "PGProxyLog" message
                in
                    model ! []

            PGProxyModule msg ->
                updatePGProxy msg model


subscriptions : Model -> Sub Msg
subscriptions model =
    let
        stopServer =
            externalStop StopServer

        pgProxySubs =
            Sub.map PGProxyModule <| PGProxy.subscriptions pgProxyConfig model.pgProxyModel
    in
        case model.status of
            Running ->
                Sub.batch [ stopServer, pgProxySubs ]

            NotRunning ->
                stopServer
