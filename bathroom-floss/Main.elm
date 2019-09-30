module Main exposing (..)

import Browser
import Browser.Events
import Html
import Html.Attributes
import Svg exposing (..)
import Svg.Attributes exposing (..)


main : Svg msg
main =
    let
        spacing =
            0.05

        head =
            [ cx "5"
            , cy "1"
            , r (p (1 - spacing))
            ]

        body =
            { limbWidth = 0.85
            , armLength = 2.0
            , legLength = 3.0
            , torsoLength = 3.0
            , startX = 5.0
            , startY = 2.0
            }

        extremity =
            "a"
                ++ p (body.limbWidth / 2)
                ++ p (body.limbWidth / 2)
                ++ "0 1 1 "
                ++ p -body.limbWidth
                ++ "0"
    in
    Html.main_ []
        [ Html.node "style" [] [ text styles ]
        , svg [ Svg.Attributes.viewBox "0 0 10 10" ]
            [ circle (fill "black" :: head) []
            , Svg.path
                [ d <|
                    String.join " "
                        [ "M" ++ p body.startX ++ p body.startY
                        , "h" ++ p (body.limbWidth + (spacing * 3 / 2))
                        , "q" ++ p body.limbWidth ++ " 0 " ++ p body.limbWidth ++ p body.limbWidth
                        , "v" ++ p body.armLength
                        , extremity
                        , "v" ++ p -body.armLength
                        , "h" ++ p -spacing
                        , "v" ++ p (body.torsoLength + body.legLength)
                        , extremity
                        , "v" ++ p -body.legLength
                        , "h" ++ p -spacing
                        , "v" ++ p body.legLength
                        , extremity
                        , "v" ++ p -(body.torsoLength + body.legLength)
                        , "h" ++ p -spacing
                        , "v" ++ p body.armLength
                        , extremity
                        , "v" ++ p -body.armLength
                        , "q 0 " ++ p -body.limbWidth ++ p body.limbWidth ++ p -body.limbWidth
                        ]
                ]
                []
            ]
        ]


styles : String
styles =
    """
    html, body {
        margin:0;
        padding:0;
    }
    main {
        height:100vh;
        width:100%;
        display:flex;
        display:flex;
        flex-direction:column;
    }
    svg {
        flex: 1;
    }
    * {
        transform-style: preserve-3d;
    }
    """


p : Float -> String
p float =
    " " ++ String.fromFloat float ++ " "
