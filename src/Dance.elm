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
    , dTarget = vec2 0.2 0.2
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
    = TimeMultiplier Anchor Float
    | Phase Anchor Float
    | Target Anchor Vec2
    | MovementX Anchor Float
    | MovementY Anchor Float


type Anchor
    = A
    | B
    | C
    | D


customize : Customization -> Dance a -> Dance a
customize customization dance =
    case customization of
        TimeMultiplier A value ->
            { dance | aTimeMultiplier = value }

        Phase A value ->
            { dance | aPhase = value }

        Target A value ->
            { dance | aTarget = value }

        MovementX A value ->
            { dance | aMovement = Math.Vector2.setX value dance.aMovement }

        MovementY A value ->
            { dance | aMovement = Math.Vector2.setY value dance.aMovement }

        TimeMultiplier B value ->
            { dance | bTimeMultiplier = value }

        Phase B value ->
            { dance | bPhase = value }

        Target B value ->
            { dance | bTarget = value }

        MovementX B value ->
            { dance | bMovement = Math.Vector2.setX value dance.bMovement }

        MovementY B value ->
            { dance | bMovement = Math.Vector2.setY value dance.bMovement }

        TimeMultiplier C value ->
            { dance | cTimeMultiplier = value }

        Phase C value ->
            { dance | cPhase = value }

        Target C value ->
            { dance | cTarget = value }

        MovementX C value ->
            { dance | cMovement = Math.Vector2.setX value dance.cMovement }

        MovementY C value ->
            { dance | cMovement = Math.Vector2.setY value dance.cMovement }

        TimeMultiplier D value ->
            { dance | dTimeMultiplier = value }

        Phase D value ->
            { dance | dPhase = value }

        Target D value ->
            { dance | dTarget = value }

        MovementX D value ->
            { dance | dMovement = Math.Vector2.setX value dance.dMovement }

        MovementY D value ->
            { dance | dMovement = Math.Vector2.setY value dance.dMovement }
