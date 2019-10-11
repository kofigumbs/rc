module Curve exposing (Shape(..), value)

-- https://en.wikipedia.org/wiki/Bézier_curve


type Shape
    = Linear
    | CubicBezier


linear : Float -> Float -> Float -> Float
linear t p0 p1 =
    p0 + t * (p1 - p0)


controlPoint1 =
    1.05


controlPoint2 =
    0.75


value : Shape -> Float -> Float -> Float -> Float
value shape =
    case shape of
        Linear ->
            linear

        CubicBezier ->
            cubicBezier


cubicBezier : Float -> Float -> Float -> Float
cubicBezier t p0 p3 =
    let
        p1 =
            p0 + controlPoint1 * (p3 - p0)

        p2 =
            p0 + controlPoint2 * (p3 - p0)
    in
    (p0 * ((1 - t) ^ 3))
        + (p1 * 3 * ((1 - t) ^ 2) * t)
        + (p2 * 3 * (1 - t) * (t ^ 2))
        + (p3 * (t ^ 3))
