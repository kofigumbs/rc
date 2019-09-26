module Curve exposing (cubicBezier)

-- https://en.wikipedia.org/wiki/Bézier_curve


controlPoint1 =
    1.05


controlPoint2 =
    0.75


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
