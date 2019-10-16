module Dance exposing (..)

{-| Choose which 4 points (a, b, c, d) to animate in the Bitmoji image!

     TimeMultiplier: rate of movement. Bigger means faster.
     Phase: radians by which to offset movement. Bigger means more delayed.
     Target: coordinates [0, 1]. Point to target.
     Movement: distance in target coordinate system [0, 1]. How far to animate.

-}

import Math.Vector2 exposing (Vec2, vec2)
import Math.Vector3 exposing (Vec3, vec3)


type alias Dance a =
    { a
        | aTimeMultiplier : Float
        , aPhase : Float
        , aTarget : Vec2
        , aMovement : Vec2
        , bTimeMultiplier : Float
        , bPhase : Float
        , bTarget : Vec2
        , bMovement : Vec2
        , cTimeMultiplier : Float
        , cPhase : Float
        , cTarget : Vec2
        , cMovement : Vec2
        , dTimeMultiplier : Float
        , dPhase : Float
        , dTarget : Vec2
        , dMovement : Vec2
    }


lean : Dance { comicId : String }
lean =
    { comicId = "49490f4e-eabb-4cab-bcb6-69f361d66706"

    -- HEAD
    , aTimeMultiplier = 16
    , aPhase = -pi / 2
    , aTarget = vec2 0.5 0.7
    , aMovement = vec2 0 0.2

    -- HIPS
    , bTimeMultiplier = 8
    , bPhase = 0
    , bTarget = vec2 0.5 0.3
    , bMovement = vec2 0.6 0

    -- FEET
    , cTimeMultiplier = 16
    , cPhase = -pi / 2
    , cTarget = vec2 0.5 0
    , cMovement = vec2 0 0.1

    -- ???
    , dTimeMultiplier = 0
    , dPhase = 0
    , dTarget = vec2 0 0
    , dMovement = vec2 0 0
    }


disco : Dance { comicId : String }
disco =
    { comicId = "5ee3832d-7743-43c8-b6d7-ea47f11a1798"

    -- LEFT ARM
    , aTimeMultiplier = 8
    , aPhase = 0
    , aTarget = vec2 0.1 0.6
    , aMovement = vec2 0.2 0.3

    -- HIPS
    , bTimeMultiplier = 8
    , bPhase = -pi / 2
    , bTarget = vec2 0.5 0.3
    , bMovement = vec2 0.5 -0.2

    -- RIGHT ARM
    , cTimeMultiplier = 8
    , cPhase = -pi
    , cTarget = vec2 0.7 0.4
    , cMovement = vec2 0.3 0.5

    -- HEAD
    , dTimeMultiplier = 8
    , dPhase = -pi / 2
    , dTarget = vec2 0.45 0.7
    , dMovement = vec2 0.05 -0.15
    }


type Customization
    = TimeMultiplier Anchor
    | Phase Anchor
    | TargetX Anchor
    | TargetY Anchor
    | MovementX Anchor
    | MovementY Anchor


type Anchor
    = A
    | B
    | C
    | D


customizations : List ( Customization, Dance a -> Float )
customizations =
    [ ( TimeMultiplier A, .aTimeMultiplier )
    , ( Phase A, .aPhase )
    , ( TargetX A, .aTarget >> Math.Vector2.getX )
    , ( TargetY A, .aTarget >> Math.Vector2.getY )
    , ( MovementX A, .aMovement >> Math.Vector2.getX )
    , ( MovementY A, .aMovement >> Math.Vector2.getY )
    , ( TimeMultiplier B, .bTimeMultiplier )
    , ( Phase B, .bPhase )
    , ( TargetX B, .bTarget >> Math.Vector2.getX )
    , ( TargetY B, .bTarget >> Math.Vector2.getY )
    , ( MovementX B, .bMovement >> Math.Vector2.getX )
    , ( MovementY B, .bMovement >> Math.Vector2.getY )
    , ( TimeMultiplier C, .cTimeMultiplier )
    , ( Phase C, .cPhase )
    , ( TargetX C, .cTarget >> Math.Vector2.getX )
    , ( TargetY C, .cTarget >> Math.Vector2.getY )
    , ( MovementX C, .cMovement >> Math.Vector2.getX )
    , ( MovementY C, .cMovement >> Math.Vector2.getY )
    , ( TimeMultiplier D, .dTimeMultiplier )
    , ( Phase D, .dPhase )
    , ( TargetX D, .dTarget >> Math.Vector2.getX )
    , ( TargetY D, .dTarget >> Math.Vector2.getY )
    , ( MovementX D, .dMovement >> Math.Vector2.getX )
    , ( MovementY D, .dMovement >> Math.Vector2.getY )
    ]


customize : Customization -> Float -> Dance a -> Dance a
customize customization value dance =
    case customization of
        TimeMultiplier A ->
            { dance | aTimeMultiplier = value }

        Phase A ->
            { dance | aPhase = value }

        TargetX A ->
            { dance | aTarget = Math.Vector2.setX value dance.aTarget }

        TargetY A ->
            { dance | aTarget = Math.Vector2.setY value dance.aTarget }

        MovementX A ->
            { dance | aMovement = Math.Vector2.setX value dance.aMovement }

        MovementY A ->
            { dance | aMovement = Math.Vector2.setY value dance.aMovement }

        TimeMultiplier B ->
            { dance | bTimeMultiplier = value }

        Phase B ->
            { dance | bPhase = value }

        TargetX B ->
            { dance | bTarget = Math.Vector2.setX value dance.bTarget }

        TargetY B ->
            { dance | bTarget = Math.Vector2.setY value dance.bTarget }

        MovementX B ->
            { dance | bMovement = Math.Vector2.setX value dance.bMovement }

        MovementY B ->
            { dance | bMovement = Math.Vector2.setY value dance.bMovement }

        TimeMultiplier C ->
            { dance | cTimeMultiplier = value }

        Phase C ->
            { dance | cPhase = value }

        TargetX C ->
            { dance | cTarget = Math.Vector2.setX value dance.cTarget }

        TargetY C ->
            { dance | cTarget = Math.Vector2.setY value dance.cTarget }

        MovementX C ->
            { dance | cMovement = Math.Vector2.setX value dance.cMovement }

        MovementY C ->
            { dance | cMovement = Math.Vector2.setY value dance.cMovement }

        TimeMultiplier D ->
            { dance | dTimeMultiplier = value }

        Phase D ->
            { dance | dPhase = value }

        TargetX D ->
            { dance | dTarget = Math.Vector2.setX value dance.dTarget }

        TargetY D ->
            { dance | dTarget = Math.Vector2.setY value dance.dTarget }

        MovementX D ->
            { dance | dMovement = Math.Vector2.setX value dance.dMovement }

        MovementY D ->
            { dance | dMovement = Math.Vector2.setY value dance.dMovement }
