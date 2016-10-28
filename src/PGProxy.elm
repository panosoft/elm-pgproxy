module PGProxy
    exposing
        ( initModel
        , update
        , Model
        , Msg
        )

import Dict exposing (Dict)
import Websocket exposing (..)
import Proxy.Decoder exposing (..)


type alias ClientState =
    { lastError : Maybe String
    }


type alias Response =
    String


type alias ClientDict =
    Dict ClientId ClientState


type alias Model msg =
    { authenticate : SessionId -> Bool
    , wsPort : WSPort
    , errorTagger : String -> msg
    , startedMsg : msg
    , stoppedMsg : msg
    , running : Bool
    , listenError : Bool
    , clients : ClientDict
    }


type alias SessionId =
    String


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


(///) : Result err value -> (err -> value) -> value
(///) result f =
    case result of
        Ok value ->
            value

        Err err ->
            f err


initModel : (String -> msg) -> msg -> msg -> (SessionId -> Bool) -> WSPort -> ( Model msg, Msg, Msg )
initModel errorTagger startedMsg stoppedMsg authenticate wsPort =
    ( { authenticate = authenticate
      , wsPort = -1
      , errorTagger = errorTagger
      , startedMsg = startedMsg
      , stoppedMsg = stoppedMsg
      , running = False
      , listenError = False
      , clients = Dict.empty
      }
    , Start
    , Stop
    )


type Msg
    = Nop
    | Start
    | Stop
    | ConnectionStatus ( WSPort, ClientId, ConnectionStatus )
    | ListenError ( WSPort, Path, String )
    | WSMessage ( ClientId, QueryString, String )
    | SendError ( WSPort, ClientId, String )
    | Sent ( WSPort, ClientId, String )


initClientState : ClientState
initClientState =
    ClientState Nothing


getClientState : Model msg -> ClientId -> ClientState
getClientState model clientId =
    let
        maybeClientState =
            Dict.get clientId model.clients

        crash =
            case maybeClientState of
                Nothing ->
                    Debug.crash "Client id: " ++ (toString clientId) ++ " is not in state"

                Just _ ->
                    ""
    in
        maybeClientState ?= initClientState


setLastError : Model msg -> ClientId -> String -> Model msg
setLastError model clientId error =
    let
        clientState =
            getClientState model clientId

        newClients =
            Dict.insert clientId { clientState | lastError = Just error } model.clients
    in
        { model | clients = newClients }


update : Msg -> Model msg -> ( ( Model msg, Cmd Msg ), List msg )
update msg model =
    case msg of
        Nop ->
            ( model ! [], [] )

        Start ->
            ( { model | running = True } ! [], [ model.startedMsg ] )

        Stop ->
            ( { model | running = False } ! [], [ model.stoppedMsg ] )

        ConnectionStatus ( wsPort, clientId, status ) ->
            case status of
                Connected ->
                    ( { model | clients = Dict.insert clientId initClientState model.clients } ! [], [] )

                Disconnected ->
                    -- TODO disconnect from DB
                    ( { model | clients = Dict.remove clientId model.clients } ! [], [] )

        ListenError ( wsPort, path, error ) ->
            ( model ! [], [ model.errorTagger ("Unable to listen to websocket on port: " ++ (toString model.wsPort) ++ " for path: " ++ path ++ " error: " ++ error) ] )

        SendError ( wsPort, clientId, error ) ->
            let
                error =
                    ("Unable to send to websocket on port: ") ++ (toString model.wsPort) ++ " for clientId: " ++ (toString clientId) ++ " error: " ++ error

                l =
                    Debug.log "SendError" ( wsPort, clientId, error )
            in
                ( setLastError model clientId error ! [], [ model.errorTagger error ] )

        Sent ( wsPort, clientId, message ) ->
            let
                l =
                    Debug.log "Send" ( wsPort, clientId, message )
            in
                ( model ! [], [] )

        WSMessage ( clientId, _, json ) ->
            let
                ( type_, decodeResult ) =
                    decodeRequest json

                request =
                    Debug.log "Request" <| decodeResult /// (\err -> UnknownProxyRequest err)

                responsePrefix =
                    "{\"type\": \"" ++ type_ ++ "\", "

                clientState =
                    getClientState model clientId

                response =
                    clientState.lastError
                        |?> (\error -> responsePrefix ++ ", " ++ errorSuffix error)
                        ?= handleRequest model clientId json type_ responsePrefix request

                l =
                    Debug.log "WSMessage" ( clientId, json )

                ll =
                    Debug.log "Response" response
            in
                ( model ! [ Websocket.send SendError Sent model.wsPort clientId response ], [] )


successSuffixCommon : Bool -> String -> String
successSuffixCommon success error =
    case success of
        True ->
            "\"success\": true}"

        False ->
            "\"error\": \"" ++ error ++ "\", \"success\": false}"


successSuffix : String
successSuffix =
    successSuffixCommon True ""


errorSuffix : String -> String
errorSuffix =
    successSuffixCommon False


handleRequest : Model msg -> ClientId -> String -> String -> String -> ProxyRequest -> Response
handleRequest model clientId json type_ responsePrefix request =
    case request of
        Connect connectionRequest ->
            responsePrefix ++ ", " ++ successSuffix

        Disconnect disconnectionRequest ->
            responsePrefix ++ ", " ++ successSuffix

        Query queryRequest ->
            responsePrefix ++ "\"records\": [\"record1\", \"record2\"], " ++ successSuffix

        MoreQueryResults moreQueryResultsRequest ->
            responsePrefix ++ "\"records\": [], " ++ successSuffix

        ExecuteSql executeSqlRequest ->
            responsePrefix ++ "\"count\": 10, " ++ successSuffix

        Listen listenRequest ->
            responsePrefix ++ ", " ++ successSuffix

        Unlisten unlistenRequest ->
            responsePrefix ++ ", " ++ successSuffix

        UnknownProxyRequest error ->
            Debug.crash "Unknown request: " ++ json ++ " Error: " ++ error


subscriptions : Model msg -> Sub Msg
subscriptions model =
    case model.running of
        True ->
            case not model.listenError of
                True ->
                    Websocket.listen ListenError WSMessage ConnectionStatus model.wsPort path

                False ->
                    Sub.none

        False ->
            Sub.none
