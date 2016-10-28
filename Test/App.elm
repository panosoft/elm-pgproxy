port module Test.App exposing (..)

import Html exposing (..)
import Html.App
import Websocket exposing (..)
import PGProxy


port exitApp : Float -> Cmd msg


port externalStop : (() -> msg) -> Sub msg


type alias Model =
    { serviceCount : Int
    , pgProxyModel : PGProxy.Model Msg
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
    | PGError String
    | PGProxyModule PGProxy.Msg


(?=) : Maybe a -> a -> a
(?=) =
    flip Maybe.withDefault


(|?>) : Maybe a -> (a -> b) -> Maybe b
(|?>) =
    flip Maybe.map


(///) : Result err value -> (err -> value) -> value
(///) result f =
    case result of
        Ok value ->
            value

        Err err ->
            f err


authenticate : SessionId -> Bool
authenticate _ =
    True


initModel : Model
initModel =
    let
        ( pgProxyModel, pgProxyStartMsg, pgProxyStopMsg ) =
            PGProxy.initModel PGError PGProxyStarted PGProxyStopped authenticate wsPort
    in
        { serviceCount = 1
        , pgProxyModel = pgProxyModel
        , pgProxyStartMsg = pgProxyStartMsg
        , pgProxyStopMsg = pgProxyStopMsg
        }


init : ( Model, Cmd Msg )
init =
    initModel ! [ Websocket.startServer ServerError ServerStatus UnhandledMessage Nothing Nothing wsPort ]


main : Program Never
main =
    Html.App.program
        { init = init
        , view = (\_ -> text "")
        , update = update
        , subscriptions = subscriptions
        }


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


doAppMsgs : Model -> List Msg -> (msg -> Msg) -> Cmd msg -> ( Model, Cmd Msg )
doAppMsgs model msgs serviceTagger serviceCmd =
    let
        doUpdate msg model cmds =
            let
                ( newModel, cmd ) =
                    update msg model
            in
                ( newModel, cmd :: cmds )

        ( finalModel, appCmds ) =
            List.foldl (\msg ( model, cmds ) -> doUpdate msg model cmds) ( model, [] ) msgs
    in
        finalModel ! (Cmd.map serviceTagger serviceCmd :: appCmds)


updatePGProxyModel : (PGProxy.Model Msg -> ( ( PGProxy.Model Msg, Cmd PGProxy.Msg ), List Msg )) -> Model -> ( Model, Cmd Msg )
updatePGProxyModel f model =
    let
        ( ( newPGProxyModel, pgProxyCmd ), msgs ) =
            f model.pgProxyModel
    in
        doAppMsgs { model | pgProxyModel = newPGProxyModel } msgs PGProxyModule pgProxyCmd


startStopServices : Model -> List (Model -> ( Model, Cmd Msg )) -> ( Model, Cmd Msg )
startStopServices model serviceUpdates =
    let
        ( newModel, cmds ) =
            List.foldl
                (\f ( model, cmds ) ->
                    let
                        ( newModel, cmd ) =
                            f model
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
        [ updatePGProxyModel <| PGProxy.update model.pgProxyStartMsg
        ]


stopServices : Model -> ( Model, Cmd Msg )
stopServices model =
    startStopServices model
        [ updatePGProxyModel <| PGProxy.update model.pgProxyStopMsg
        ]


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
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
                model ! []

        ServerStatus ( wsPort, status ) ->
            let
                l =
                    Debug.log "ServerStatus" ( wsPort, status )
            in
                case status of
                    Started ->
                        startServices model

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

        PGError error ->
            let
                l =
                    Debug.log "PGError" error
            in
                model ! []

        PGProxyModule msg ->
            updatePGProxyModel (PGProxy.update msg) model


subscriptions : Model -> Sub Msg
subscriptions model =
    externalStop StopServer
