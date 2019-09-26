module Glyph exposing (Shape(..), view)

import Svg exposing (..)
import Svg.Attributes exposing (..)


size : Float
size =
    0.4


type Shape
    = Dot
    | Star
    | Box


view : Shape -> Float -> ( Int, Int ) -> Svg msg
view shape =
    case shape of
        Dot ->
            dot

        Star ->
            star

        Box ->
            box


dot : Float -> ( Int, Int ) -> Svg msg
dot multiplier ( x, y ) =
    circle
        [ fill "black"
        , cx (String.fromInt x)
        , cy (String.fromInt y)
        , r (String.fromFloat (size * multiplier))
        ]
        []


star : Float -> ( Int, Int ) -> Svg msg
star multiplier ( x, y ) =
    let
        center =
            String.fromFloat (toFloat x)
                ++ " "
                ++ String.fromFloat (toFloat y)
                ++ " "
    in
    Svg.path
        [ fill "transparent"
        , stroke "orange"
        , strokeWidth <| String.fromFloat (multiplier * 0.2)
        , d <|
            ("M "
                ++ String.fromFloat (toFloat x - size)
                ++ " "
                ++ String.fromFloat (toFloat y)
            )
                ++ ("Q "
                        ++ center
                        ++ String.fromFloat (toFloat x)
                        ++ " "
                        ++ String.fromFloat (toFloat y - size)
                   )
                ++ ("Q "
                        ++ center
                        ++ String.fromFloat (toFloat x + size)
                        ++ " "
                        ++ String.fromFloat (toFloat y)
                   )
                ++ ("Q "
                        ++ center
                        ++ String.fromFloat (toFloat x)
                        ++ " "
                        ++ String.fromFloat (toFloat y + size)
                   )
                ++ ("Q "
                        ++ center
                        ++ String.fromFloat (toFloat x - size)
                        ++ " "
                        ++ String.fromFloat (toFloat y)
                   )
        ]
        []


box : Float -> ( Int, Int ) -> Svg msg
box multiplier ( x, y ) =
    Svg.path
        [ fill "green"
        , fillOpacity (String.fromFloat multiplier)
        , stroke "black"
        , strokeDasharray "3.3"
        , strokeDashoffset <| String.fromFloat ((1 - multiplier) * 3.3)
        , strokeWidth "0.2"
        , d <|
            ("M "
                ++ String.fromFloat (toFloat x - size)
                ++ " "
                ++ String.fromFloat (toFloat y - size)
            )
                ++ ("H " ++ String.fromFloat (toFloat x + size))
                ++ ("V " ++ String.fromFloat (toFloat y + size))
                ++ ("H " ++ String.fromFloat (toFloat x - size))
                ++ ("V " ++ String.fromFloat (toFloat y - size))
        ]
        []
