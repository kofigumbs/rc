module Bitmoji exposing (Options, PoseId(..), chromeExtensionUrl, default, parseUserId, url)

import Json.Decode exposing (..)


type alias Options =
    { userId : String, poseId : PoseId }


type PoseId
    = Standing
    | Pointing


default =
    { userId = "4b014b97-f9a9-480e-8e7f-3c74def6e9f6", poseId = Standing }


parseUserId : Decoder String
parseUserId =
    andThen
        (\raw ->
            if not <| String.startsWith baseUrl raw then
                fail ""
            else
                dropUntilUserId succeed (fail "") (String.split "-" raw)
        )
        string


dropUntilUserId : (String -> a) -> a -> List String -> a
dropUntilUserId onSucceed onFail segments =
    case segments of
        [] ->
            onFail

        [ a, b, c, d, e, _ ] ->
            onSucceed (String.join "-" [ a, b, c, d, e ])

        _ :: rest ->
            dropUntilUserId onSucceed onFail rest


url : Options -> String
url options =
    baseUrl
        ++ poseId options.poseId
        ++ "-"
        ++ options.userId
        ++ "-v1.png?transparent=1&palette=1"


poseId : PoseId -> String
poseId poseId_ =
    case poseId_ of
        Standing ->
            "49490f4e-eabb-4cab-bcb6-69f361d66706"

        Pointing ->
            "5ee3832d-7743-43c8-b6d7-ea47f11a1798"


baseUrl =
    "https://render.bitstrips.com/v2/cpanel/"


chromeExtensionUrl =
    "https://chrome.google.com/webstore/detail/bitmoji/bfgdeiadkckfbkeigkoncpdieiiefpig?hl=en"
