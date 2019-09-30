module Main exposing (..)

import Browser
import Browser.Events
import Html
import Html.Attributes
import Svg exposing (..)
import Svg.Attributes exposing (..)


main : Svg msg
main =
    Html.main_ []
        [ Html.node "style" [] [ text styles ]
        , svg [ Svg.Attributes.viewBox "0 0 10 10" ]
            [ circle [ cx "4.9", cy "1", r "0.95", fill "black" ] []
            , Svg.path
                [ d <| """
                    M 5 2
                    H 6
                    Q 7 2 7 3
                    V 5
                    A 0.50 0.50 1 1 1 6 5
                    V 3
                    H 5.95
                    V 9
                    A 0.50 0.50 1 1 1 4.95 9
                    V 6
                    H 4.90
                    V 9
                    A 0.5 0.5 1 1 1 3.90 9
                    V 3
                    H 3.85
                    V 5
                    A 0.50 0.50 1 1 1 2.85 5
                    V 3
                    Q 2.85 2 3.85 2
                """
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
