module Glyph exposing (Shape(..), view)

import Svg exposing (Svg, circle, path)
import Svg.Attributes exposing (..)


size : Float
size =
    0.4


type Shape
    = Dot
    | Star
    | Box
    | Pedal


view : Shape -> Float -> ( Int, Int ) -> Svg msg
view shape multiplier ( x, y ) =
    let
        f =
            case shape of
                Dot ->
                    dot

                Star ->
                    star

                Box ->
                    box

                Pedal ->
                    pedal
    in
    f multiplier (toFloat x) (toFloat y)


dot : Float -> Float -> Float -> Svg msg
dot multiplier x y =
    circle
        [ fill "black"
        , cx (decimal x)
        , cy (decimal y)
        , r (decimal (size * multiplier))
        ]
        []


star : Float -> Float -> Float -> Svg msg
star multiplier x y =
    let
        center =
            decimal x ++ " " ++ decimal y ++ " "
    in
    Svg.path
        [ fill "transparent"
        , stroke "orange"
        , strokeWidth <| decimal (multiplier * 0.2)
        , d <|
            ("M " ++ decimal (x - size) ++ " " ++ decimal y)
                ++ ("Q " ++ center ++ decimal x ++ " " ++ decimal (y - size))
                ++ ("Q " ++ center ++ decimal (x + size) ++ " " ++ decimal y)
                ++ ("Q " ++ center ++ decimal x ++ " " ++ decimal (y + size))
                ++ ("Q " ++ center ++ decimal (x - size) ++ " " ++ decimal y)
        ]
        []


box : Float -> Float -> Float -> Svg msg
box multiplier x y =
    let
        measuredLength =
            3.3
    in
    Svg.path
        [ fill "green"
        , fillOpacity (decimal multiplier)
        , stroke "black"
        , strokeDasharray (decimal measuredLength)
        , strokeDashoffset <| decimal ((1 - multiplier) * measuredLength)
        , strokeWidth "0.2"
        , d <|
            ("M " ++ decimal (x - size) ++ " " ++ decimal (y - size))
                ++ ("H " ++ decimal (x + size))
                ++ ("V " ++ decimal (y + size))
                ++ ("H " ++ decimal (x - size))
                ++ ("V " ++ decimal (y - size))
        ]
        []


pedal : Float -> Float -> Float -> Svg msg
pedal multiplier x y =
    Svg.path
        [ fill "url(#pedal)"
        , fillOpacity <| decimal multiplier
        , style <|
            if multiplier <= 0.001 then
                "display: none"
            else
                "transform: rotate3d("
                    ++ decimal x
                    ++ ","
                    ++ decimal y
                    ++ ", 0,"
                    ++ decimal ((1 - multiplier) * 270)
                    ++ "deg);"
        , d <|
            ("M " ++ decimal (x - size) ++ " " ++ decimal y)
                ++ ("Q "
                        ++ decimal (x - size)
                        ++ " "
                        ++ decimal (y - size)
                        ++ " "
                        ++ decimal x
                        ++ " "
                        ++ decimal (y - size)
                   )
                ++ ("Q "
                        ++ decimal (x + size)
                        ++ " "
                        ++ decimal (y - size)
                        ++ " "
                        ++ decimal (x + size)
                        ++ " "
                        ++ decimal y
                   )
                ++ ("Q "
                        ++ decimal (x + size)
                        ++ " "
                        ++ decimal (y + size)
                        ++ " "
                        ++ decimal x
                        ++ " "
                        ++ decimal (y + size)
                   )
                ++ ("Q "
                        ++ decimal (x - size)
                        ++ " "
                        ++ decimal (y + size)
                        ++ " "
                        ++ decimal (x - size)
                        ++ " "
                        ++ decimal y
                   )
        ]
        []


decimal : Float -> String
decimal =
    String.fromFloat
