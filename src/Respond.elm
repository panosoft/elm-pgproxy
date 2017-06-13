module Respond
    exposing
        ( jsonStringEscape
        , respondMessage
        , respondError
        , respondUnsolicited
        , respondSuccess
        )

import Json.Decode as JD exposing (..)
import Websocket exposing (..)
import Utils.Ops exposing (..)
import Utils.Regex exposing (..)


type alias Request =
    String


type alias Response =
    String


jsonStringEscape : String -> String
jsonStringEscape string =
    string
        |> replaceAll "\\t" "\\\\t"
        |> replaceAll "\\n" "\\\\n"
        |> replaceAll "\"" "\\\""


getRequestId : String -> String
getRequestId request =
    JD.decodeString (field "requestId" int) request
        |??> toString
        ??= (always "Missing requestId")


getRequestType : String -> String
getRequestType request =
    JD.decodeString (field "func" string) request
        ??= (always "Missing requestType")


respond : (( WSPort, ClientId, String, String ) -> msg) -> (( WSPort, ClientId, String ) -> msg) -> ClientId -> WSPort -> Response -> Cmd msg
respond sendErrorTagger sentTagger clientId wsPort =
    Websocket.send sendErrorTagger sentTagger wsPort clientId


respondMessage : (( WSPort, ClientId, String, String ) -> msg) -> (( WSPort, ClientId, String ) -> msg) -> WSPort -> Maybe Bool -> Bool -> Maybe ( String, String ) -> Maybe String -> ClientId -> Request -> Cmd msg
respondMessage sendErrorTagger sentTagger wsPort maybeSuccess unsolicited maybeKeyValuePair maybeKeysFormatted clientId request =
    let
        unsolicitedMessage =
            case unsolicited of
                True ->
                    ", \"unsolicited\": true"

                False ->
                    ""

        typeMessage =
            ", \"type\": \"" ++ getRequestType request ++ "\""

        clientIdMessage =
            ", \"clientId\": \"" ++ (toString clientId) ++ "\""

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
            ("{\"requestId\": " ++ (getRequestId <| request))
                ++ typeMessage
                ++ unsolicitedMessage
                ++ (maybeSuccess |?> successMessage ?= "")
                ++ (maybeKeysFormatted ?= keyMessage)
                ++ clientIdMessage
                ++ "}"
    in
        respond sendErrorTagger sentTagger clientId wsPort responseFormatted


respondError : (( WSPort, ClientId, String, String ) -> msg) -> (( WSPort, ClientId, String ) -> msg) -> WSPort -> String -> ClientId -> String -> Cmd msg
respondError sendErrorTagger sentTagger wsPort message =
    respondMessage sendErrorTagger sentTagger wsPort (Just False) False (Just ( "error", message )) Nothing


respondUnsolicited : (( WSPort, ClientId, String, String ) -> msg) -> (( WSPort, ClientId, String ) -> msg) -> WSPort -> String -> String -> ClientId -> String -> Cmd msg
respondUnsolicited sendErrorTagger sentTagger wsPort key message =
    respondMessage sendErrorTagger sentTagger wsPort Nothing True (Just ( key, message )) Nothing


respondSuccess : (( WSPort, ClientId, String, String ) -> msg) -> (( WSPort, ClientId, String ) -> msg) -> WSPort -> ClientId -> String -> Cmd msg
respondSuccess sendErrorTagger sentTagger wsPort =
    respondMessage sendErrorTagger sentTagger wsPort (Just True) False Nothing Nothing
