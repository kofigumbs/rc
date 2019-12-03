port module Visualizer exposing (main)

import Angle exposing (Angle)
import Array exposing (Array)
import Axis3d exposing (Axis3d)
import Base64
import Bitwise
import Browser
import Browser.Events
import Camera3d
import Color
import Direction3d exposing (Direction3d)
import Html exposing (Html)
import Html.Attributes
import Html.Events
import Illuminance
import Json.Decode as D
import Length exposing (Length)
import Luminance
import Parser as P exposing ((|.), (|=))
import Pixels
import Point3d
import Quantity
import Scene3d
import Scene3d.Chromaticity
import Scene3d.Drawable as Drawable exposing (Drawable, Material)
import Scene3d.Exposure
import Scene3d.Light
import Scene3d.Mesh as Mesh exposing (Mesh)
import Scene3d.Shape as Shape
import Time
import Vector3d exposing (Vector3d)
import Viewpoint3d


main : Program Flags Model Msg
main =
    Browser.element
        { init = init
        , update = update
        , subscriptions = subscriptions
        , view = view
        }


type alias Model =
    { time : Float
    , tick : Int
    , width : Float
    , height : Float
    , dance : Dance
    , clock : Clock
    , channels : Array ( Int, Int, Float )
    }


type alias Dance =
    { error : Bool
    , code : String
    , plan : Array Pose
    , prev : Pose
    , next : Pose
    }


type alias Clock =
    { diffPerBeat : Float
    , index : Int
    , history : Array Float
    }


type alias Flags =
    { width : Float, height : Float, hash : String }


init : Flags -> ( Model, Cmd Msg )
init flags =
    let
        dance =
            if List.member flags.hash [ "", "#", "#!" ] then
                defaultDance
            else
                case P.run hashParser flags.hash of
                    Err _ ->
                        defaultDance

                    Ok (Ok loadedDance) ->
                        loadedDance

                    Ok (Err code) ->
                        { defaultDance | error = True, code = code }
    in
    ( { time = 0
      , tick = 1
      , width = flags.width
      , height = flags.height
      , dance = dance
      , clock = Clock 350 0 (Array.initialize 24 (toFloat >> (*) 350))
      , channels = Array.repeat 8 ( 0, 0, 0 )
      }
    , Cmd.none
    )


type Msg
    = Diff Float
    | Resize Int Int
    | SetCode String
    | GotMidiMessage (List Int)


update : Msg -> Model -> ( Model, Cmd Msg )
update msg ({ dance } as model) =
    case msg of
        Diff diff ->
            let
                newTime =
                    model.time + diff

                newTick =
                    model.tick + 1
            in
            if newTime <= model.clock.diffPerBeat then
                pure { model | time = newTime }
            else
                pure
                    { model
                        | time = decrementAsMuchAsPossible model.clock.diffPerBeat newTime
                        , tick = newTick
                        , dance =
                            { dance
                                | prev = dance.next
                                , next = safeModeGet newTick dance.plan |> Maybe.withDefault dance.prev
                            }
                    }

        Resize width height ->
            pure { model | width = toFloat width, height = toFloat height }

        SetCode code ->
            case P.run planParser code of
                Err _ ->
                    pure { model | dance = { dance | error = True, code = code } }

                Ok plan ->
                    ( { model | dance = { dance | error = False, code = code, plan = plan } }
                    , setHash ("#!" ++ Base64.encode code)
                    )

        GotMidiMessage data ->
            pure (applyMidi data model)


{-| Sometimes we need to subtract more than once if the window has been un-focused for a while.
Otherwise, the next few `Diff` messages will be very large numbers.
-}
decrementAsMuchAsPossible : number -> number -> number
decrementAsMuchAsPossible interval n =
    if interval > n then
        n
    else
        decrementAsMuchAsPossible interval (n - interval)


safeModeGet : Int -> Array a -> Maybe a
safeModeGet index array =
    Array.get (modBy (max 1 (Array.length array)) index) array


port setHash : String -> Cmd msg


pure : a -> ( a, Cmd msg )
pure a =
    ( a, Cmd.none )


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.batch
        [ midiMessage GotMidiMessage
        , Browser.Events.onAnimationFrameDelta Diff
        , Browser.Events.onResize Resize
        ]



-- DANCE-LANG


type alias Pose =
    { head : Move
    , torso : Move
    , leftArm : Move
    , rightArm : Move
    , leftLeg : Move
    , rightLeg : Move
    }


type alias Move =
    { translate : Vector3d Length.Meters ()
    , rotateAxis : Axis3d Length.Meters ()
    , rotateAngle : Float
    }


{-| Steps to change:

1.  Add `Debug.log` lines in `SetCode` msg
2.  Modify editor contents on localhost
3.  Update let-bindings below

-}
defaultDance : Dance
defaultDance =
    let
        code =
            String.trimLeft """
leftarm   down 0.15  left 0.15   back 1
rightarm  forward 1  pitch -15
rightleg  down 2     pitch 45
head      left 0.2
torso     left 0.2   roll 5

-

rightarm  down 0.15  right 0.15  back 1
leftarm   forward 1  pitch -15
leftleg   down 2     pitch 45
head      right 0.2
torso     right 0.2  roll -5
"""

        prev =
            { head = { rotateAngle = 0, rotateAxis = Axis3d.x, translate = Vector3d.meters -0.2 0 0 }
            , leftArm = { rotateAngle = 0, rotateAxis = Axis3d.x, translate = Vector3d.meters -0.15 -0.15 -1 }
            , leftLeg = { rotateAngle = 0, rotateAxis = Axis3d.x, translate = Vector3d.zero }
            , rightArm = { rotateAngle = -15, rotateAxis = Axis3d.x, translate = Vector3d.meters 0 0 1 }
            , rightLeg = { rotateAngle = 45, rotateAxis = Axis3d.x, translate = Vector3d.meters 0 -2 0 }
            , torso = { rotateAngle = 5, rotateAxis = Axis3d.z, translate = Vector3d.meters -0.2 0 0 }
            }

        next =
            { head = { rotateAngle = 0, rotateAxis = Axis3d.x, translate = Vector3d.meters 0.2 0 0 }
            , leftArm = { rotateAngle = -15, rotateAxis = Axis3d.x, translate = Vector3d.meters 0 0 1 }
            , leftLeg = { rotateAngle = 45, rotateAxis = Axis3d.x, translate = Vector3d.meters 0 -2 0 }
            , rightArm = { rotateAngle = 0, rotateAxis = Axis3d.x, translate = Vector3d.meters 0.15 -0.15 -1 }
            , rightLeg = { rotateAngle = 0, rotateAxis = Axis3d.x, translate = Vector3d.zero }
            , torso = { rotateAngle = -5, rotateAxis = Axis3d.z, translate = Vector3d.meters 0.2 0 0 }
            }
    in
    { error = False
    , code = code
    , prev = prev
    , next = next
    , plan = Array.fromList [ prev, next ]
    }


neutral : Pose
neutral =
    { head = noMove
    , torso = noMove
    , leftArm = noMove
    , rightArm = noMove
    , leftLeg = noMove
    , rightLeg = noMove
    }


hashParser : P.Parser (Result String Dance)
hashParser =
    P.succeed identity
        |. P.chompWhile (\x -> x == '#' || x == '!')
        |= P.getChompedString (P.chompWhile (\_ -> True))
        |. P.end
        |> P.andThen
            (\x ->
                case Base64.decode x of
                    Err error ->
                        P.problem error

                    Ok code ->
                        P.run danceParser code
                            |> Result.mapError (\_ -> code)
                            |> P.succeed
            )


danceParser : P.Parser Dance
danceParser =
    P.succeed
        (\code plan ->
            let
                prev =
                    Array.get 0 plan |> Maybe.withDefault neutral

                next =
                    Array.get 1 plan |> Maybe.withDefault prev
            in
            { error = False, code = code, prev = prev, next = next, plan = plan }
        )
        |= P.getSource
        |= planParser


planParser : P.Parser (Array Pose)
planParser =
    P.loop Array.empty poseParser


poseParser : Array Pose -> P.Parser (P.Step (Array Pose) (Array Pose))
poseParser poses =
    P.succeed (|>)
        |. P.spaces
        |= P.loop neutral partParser
        |. P.spaces
        |= P.oneOf
            [ P.succeed (\x -> P.Done (Array.push x poses)) |. P.end
            , P.succeed (\x -> P.Loop (Array.push x poses)) |. break
            ]


partParser : Pose -> P.Parser (P.Step Pose Pose)
partParser old =
    P.oneOf
        [ partKeywordParser
            |> P.andThen
                (\setter ->
                    P.succeed P.Loop
                        |. P.spaces
                        |= P.loop ( old, setter ) moveParser
                        |. P.spaces
                )
        , P.succeed (P.Done old)
        ]


partKeywordParser : P.Parser (Move -> Pose -> Pose)
partKeywordParser =
    P.oneOf
        [ P.succeed (\x a -> { a | head = merge a.head x }) |. P.keyword "head"
        , P.succeed (\x a -> { a | torso = merge a.torso x }) |. P.keyword "torso"
        , P.succeed (\x a -> { a | leftArm = merge a.leftArm x }) |. P.keyword "leftarm"
        , P.succeed (\x a -> { a | rightArm = merge a.rightArm x }) |. P.keyword "rightarm"
        , P.succeed (\x a -> { a | leftLeg = merge a.leftLeg x }) |. P.keyword "leftleg"
        , P.succeed (\x a -> { a | rightLeg = merge a.rightLeg x }) |. P.keyword "rightleg"
        ]


moveParser :
    ( Pose, Move -> Pose -> Pose )
    -> P.Parser (P.Step ( Pose, Move -> Pose -> Pose ) Pose)
moveParser ( old, setter ) =
    P.oneOf
        [ P.succeed (|>)
            |= moveKeywordParser
            |= P.succeed (\x -> P.Loop ( setter x old, setter ))
            |. P.spaces
        , P.succeed (P.Done old)
        ]


moveKeywordParser : P.Parser Move
moveKeywordParser =
    P.oneOf
        [ P.succeed translateIn
            |= P.oneOf
                [ P.succeed Direction3d.y |. P.keyword "up"
                , P.succeed Direction3d.negativeY |. P.keyword "down"
                , P.succeed Direction3d.x |. P.keyword "right"
                , P.succeed Direction3d.negativeX |. P.keyword "left"
                , P.succeed Direction3d.z |. P.keyword "forward"
                , P.succeed Direction3d.negativeZ |. P.keyword "back"
                ]
        , P.succeed rotateAround
            |= P.oneOf
                [ P.succeed Axis3d.x |. P.keyword "pitch"
                , P.succeed Axis3d.y |. P.keyword "yaw"
                , P.succeed Axis3d.z |. P.keyword "roll"
                ]
        ]
        |. P.spaces
        |= P.oneOf [ P.succeed negate |. P.symbol "-" |= P.float, P.float ]


break : P.Parser ()
break =
    P.chompIf (\x -> x == '-')


merge : Move -> Move -> Move
merge a b =
    { translate = Vector3d.plus a.translate b.translate
    , rotateAxis = Axis3d.rotateAround a.rotateAxis (Angle.degrees a.rotateAngle) b.rotateAxis
    , rotateAngle = b.rotateAngle
    }


rotateAround : Axis3d Length.Meters () -> Float -> Move
rotateAround axis angle =
    { noMove | rotateAxis = axis, rotateAngle = angle }


translateIn : Direction3d () -> Float -> Move
translateIn dir length =
    { noMove | translate = Vector3d.withLength (Length.meters length) dir }


noMove : Move
noMove =
    Move Vector3d.zero Axis3d.x 0



-- MIDI STUFF


applyMidi : List Int -> Model -> Model
applyMidi data model =
    let
        command =
            List.head data
                |> Maybe.map (Bitwise.shiftRightBy 4)
    in
    case ( command, data ) of
        ( Just 8, [ status, note, velocity ] ) ->
            let
                channel =
                    Bitwise.and 0x07 status

                ( _, old, _ ) =
                    Array.get channel model.channels
                        |> Maybe.withDefault ( 0, 0, 0 )
            in
            { model | channels = Array.set channel ( old, (modBy 12 note - 6) * 10, model.time ) model.channels }

        ( _, [ 248 ] ) ->
            let
                clockHistory =
                    Array.set model.clock.index model.time model.clock.history
            in
            { model
                | clock =
                    { history =
                        clockHistory
                    , index =
                        modBy (Array.length clockHistory) (model.clock.index + 1)
                    , diffPerBeat =
                        sumDiffs 0 (Array.toList clockHistory)
                            * 6
                            / toFloat (Array.length clockHistory - 1)
                    }
            }

        _ ->
            model


sumDiffs : Float -> List Float -> Float
sumDiffs acc list =
    case list of
        a :: ((b :: _) as rest) ->
            sumDiffs (acc + abs (b - a)) rest

        _ ->
            acc


port midiMessage : (List Int -> msg) -> Sub msg



-- RENDER


view : Model -> Html Msg
view model =
    Html.main_ []
        [ viewSubject model
        , Html.textarea
            [ Html.Attributes.autofocus True
            , Html.Attributes.spellcheck False
            , Html.Events.onInput SetCode
            , Html.Attributes.classList [ ( "error", model.dance.error ) ]
            , Html.Attributes.style "width" (String.fromInt (floor model.width // 2) ++ "px")
            ]
            [ Html.text model.dance.code ]
        ]


viewSubject : Model -> Html msg
viewSubject model =
    let
        viewpoint =
            Viewpoint3d.lookAt
                { focalPoint = Point3d.meters 0 -2 0
                , eyePoint = Point3d.meters 0 2 16
                , upDirection = Direction3d.y
                }

        camera =
            Camera3d.perspective
                { viewpoint = viewpoint
                , verticalFieldOfView = Angle.degrees 30
                , clipDepth = Length.meters 0.1
                }

        sunlight =
            Scene3d.Light.directional Scene3d.Chromaticity.daylight
                (Illuminance.lux 10000)
                (Direction3d.zxY (Angle.degrees 45) (Angle.degrees 195))

        ambientLighting =
            Scene3d.Light.overcast
                { zenithDirection = Direction3d.z
                , chromaticity = Scene3d.Chromaticity.daylight
                , zenithLuminance = Luminance.nits 5000
                }
    in
    Scene3d.render [ Scene3d.clearColor Color.darkPurple ]
        { camera = camera
        , width = Pixels.pixels (model.width / 2)
        , height = Pixels.pixels model.height
        , ambientLighting = Just ambientLighting
        , lights = Scene3d.oneLight sunlight { castsShadows = False }
        , exposure = Scene3d.Exposure.fromMaxLuminance (Luminance.nits 10000)
        , whiteBalance = Scene3d.Chromaticity.daylight
        }
        [ animate model head .head
        , animate model torso .torso
        , animate model arm .leftArm
            |> Drawable.translateIn Direction3d.negativeX armOffset
        , animate model arm .rightArm
            |> Drawable.translateIn Direction3d.x armOffset
        , animate model leg .leftLeg
            |> Drawable.translateIn Direction3d.negativeX legOffset
        , animate model leg .rightLeg
            |> Drawable.translateIn Direction3d.x legOffset
        ]


head : Drawable a
head =
    bodyPart <| Shape.sphere { radius = var.headRadius, subdivisions = 72 }


leg : Drawable a
leg =
    pill var.limbRadius var.legLength
        |> Drawable.translateIn Direction3d.negativeY
            (Quantity.sum [ var.headRadius, var.spacing, var.torsoLength, var.spacing ])


arm : Drawable a
arm =
    pill var.limbRadius var.armLength
        |> Drawable.translateIn Direction3d.negativeY (Quantity.plus var.spacing var.headRadius)


torso : Drawable a
torso =
    pill torsoRadius var.torsoLength
        |> Drawable.translateIn Direction3d.negativeY (Quantity.plus var.spacing var.headRadius)


pill : Length -> Length -> Drawable a
pill radius length =
    let
        height =
            Quantity.minus (Quantity.twice radius) length

        trunk =
            Shape.cylinder { radius = radius, height = height, subdivisions = 72 }

        end =
            Shape.sphere { radius = radius, subdivisions = 72 }
    in
    Drawable.group
        [ bodyPart trunk
            |> Drawable.translateIn Direction3d.z radius
        , bodyPart end
            |> Drawable.translateIn Direction3d.z radius
        , bodyPart end
            |> Drawable.translateIn Direction3d.z (Quantity.plus radius height)
        ]
        |> Drawable.rotateAround Axis3d.x (Angle.degrees 90)


bodyPart : Mesh a (Mesh.Triangles Mesh.WithNormals uv tangents shadows) -> Drawable a
bodyPart =
    Drawable.physical { baseColor = Color.white, roughness = 0.25, metallic = False }


torsoRadius : Length
torsoRadius =
    Quantity.half (Quantity.plus var.headRadius var.limbRadius)


armOffset : Length
armOffset =
    Quantity.sum [ torsoRadius, var.spacing, var.limbRadius ]


legOffset : Length
legOffset =
    Quantity.plus var.spacing var.limbRadius


var =
    { spacing = Length.meters 0.075
    , headRadius = Length.meters 0.85
    , limbRadius = Length.meters 0.375
    , armLength = Length.meters 2.0
    , legLength = Length.meters 2.8
    , torsoLength = Length.meters 2.5
    , movement = Length.meters 0.85
    }



-- ANIMATE


animate : Model -> Drawable () -> (Pose -> Move) -> Drawable ()
animate model drawable part =
    let
        a =
            part model.dance.prev

        b =
            part model.dance.next

        vectorA =
            Vector3d.unwrap a.translate

        vectorB =
            Vector3d.unwrap b.translate

        curve start end =
            cubicBezier (model.time / model.clock.diffPerBeat) start end
    in
    drawable
        |> Drawable.rotateAround a.rotateAxis (Angle.degrees (curve a.rotateAngle b.rotateAngle {- TODO -}))
        |> Drawable.translateIn Direction3d.x (Length.meters (curve vectorA.x vectorB.x))
        |> Drawable.translateIn Direction3d.y (Length.meters (curve vectorA.y vectorB.y))
        |> Drawable.translateIn Direction3d.z (Length.meters (curve vectorA.z vectorB.z))


cubicBezier : Float -> Float -> Float -> Float
cubicBezier t p0 p3 =
    let
        controlPoint1 =
            0.85

        controlPoint2 =
            1.15

        p1 =
            p0 + controlPoint1 * (p3 - p0)

        p2 =
            p0 + controlPoint2 * (p3 - p0)
    in
    (p0 * ((1 - t) ^ 3))
        + (p1 * 3 * ((1 - t) ^ 2) * t)
        + (p2 * 3 * (1 - t) * (t ^ 2))
        + (p3 * (t ^ 3))
