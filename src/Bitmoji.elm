module Bitmoji exposing (Options, chromeExtensionUrl, default, parseUserId, url)

import Json.Decode exposing (..)


type alias Options =
    { userId : String, poseId : String }


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
        ++ options.poseId
        ++ "-"
        ++ options.userId
        ++ "-v1.png?transparent=1&palette=1"



-- https://render.bitstrips.com/v2/cpanel/49490f4e-eabb-4cab-bcb6-69f361d66706-4b014b97-f9a9-480e-8e7f-3c74def6e9f6-v1.png?transparent=1&palette=1
-- https://render.bitstrips.com/v2/cpanel/4b014b97-f9a9-480e-8e7f-3c74def6e9f6-49490f4e-eabb-4cab-bcb6-69f361d66706-v1.png?transparent=1&palette=1


default =
    { userId = "4b014b97-f9a9-480e-8e7f-3c74def6e9f6", poseId = poseId.standing }


poseId =
    { standing = "49490f4e-eabb-4cab-bcb6-69f361d66706"
    }


baseUrl =
    "https://render.bitstrips.com/v2/cpanel/"


chromeExtensionUrl =
    "https://chrome.google.com/webstore/detail/bitmoji/bfgdeiadkckfbkeigkoncpdieiiefpig?hl=en"
