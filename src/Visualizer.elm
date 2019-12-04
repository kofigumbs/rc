port module Visualizer exposing (main)

import Angle exposing (Angle)
import Array exposing (Array)
import Axis3d exposing (Axis3d)
import Base64
import Bitwise
import Browser
import Browser.Events
import Camera3d
import Char
import Color
import Dict exposing (Dict)
import Direction3d exposing (Direction3d)
import Html exposing (Html)
import Html.Attributes
import Html.Events
import Illuminance
import Json.Encode as E
import Length exposing (Length)
import Luminance
import Parser as P exposing ((|.), (|=))
import Pixels
import Point3d
import Quantity exposing (Quantity(..))
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
    { width : Float
    , height : Float
    , dance : Dance
    , clock : Clock
    , lastNote : String
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
                    Err reason ->
                        defaultDance

                    Ok (Ok loadedDance) ->
                        loadedDance

                    Ok (Err code) ->
                        { defaultDance | error = True, code = code }
    in
    ( { width = flags.width
      , height = flags.height
      , dance = dance
      , clock = initialClock
      , lastNote = ""
      }
    , Cmd.none
    )


type Msg
    = Resize Int Int
    | GotMidiMessage ( Float, List Int )
    | SetCode String


update : Msg -> Model -> ( Model, Cmd Msg )
update msg ({ dance } as model) =
    case msg of
        Resize width height ->
            pure { model | width = toFloat width, height = toFloat height }

        GotMidiMessage ( time, data ) ->
            pure (applyMidi time data model)

        SetCode code ->
            case P.run planParser code of
                Err reason ->
                    pure { model | dance = { dance | error = True, code = code } }

                Ok plan ->
                    ( { model | dance = { dance | error = False, code = code, plan = plan } }
                    , setHash ("#!" ++ Base64.encode code)
                    )


nextPose : String -> Dance -> Pose
nextPose note dance =
    Dict.get note dance.plan |> Maybe.withDefault dance.next


port setHash : String -> Cmd msg


pure : a -> ( a, Cmd msg )
pure a =
    ( a, Cmd.none )


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.batch
        [ midiMessage GotMidiMessage
        , Browser.Events.onResize Resize
        ]



-- DANCE-LANG


type alias Dance =
    { error : Bool
    , code : String
    , plan : Dict String Pose
    , prev : Pose
    , next : Pose
    }


type alias Pose =
    { head : Move
    , torso : Move
    , leftArm : Move
    , rightArm : Move
    , leftLeg : Move
    , rightLeg : Move
    }


type alias Move =
    { rotate : Vector3d Length.Meters ()
    , translate : Vector3d Length.Meters ()
    }


defaultDance : Dance
defaultDance =
    let
        code =
            String.trimLeft """
C
leftarm   down 0.15  left 0.15   back 1
rightarm  forward 1  pitch -375
rightleg  pitch 30
head      left 0.2
torso     left 0.2   roll 5

G
rightarm  down 0.15  right 0.15  back 1
leftarm   forward 1  pitch -375
leftleg   pitch 30
head      right 0.2
torso     right 0.2  roll -5
"""
    in
    P.run danceParser code
        |> Result.withDefault
            { error = True
            , code = code
            , prev = neutralPose
            , next = neutralPose
            , plan = Dict.empty
            }


neutralPose : Pose
neutralPose =
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
                    Err reason ->
                        P.problem reason

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
                ( prev, next ) =
                    case Dict.values plan of
                        a :: b :: _ ->
                            ( a, b )

                        [ one ] ->
                            ( one, one )

                        [] ->
                            ( neutralPose, neutralPose )
            in
            { error = False, code = code, prev = prev, next = next, plan = plan }
        )
        |= P.getSource
        |= planParser


planParser : P.Parser (Dict String Pose)
planParser =
    P.succeed identity
        |. P.spaces
        |= P.loop Dict.empty poseParser


poseParser : Dict String Pose -> P.Parser (P.Step (Dict String Pose) (Dict String Pose))
poseParser poses =
    P.succeed (\note pose loop -> loop (Dict.insert note pose poses))
        |= noteParser
        |. P.spaces
        |= P.loop neutralPose partParser
        |. P.spaces
        |= P.oneOf [ P.succeed P.Done |. P.end, P.succeed P.Loop ]


noteParser : P.Parser String
noteParser =
    let
        isNote code =
            65 <= code && code <= 71
    in
    P.chompIf (isNote << Char.toCode)
        |. P.oneOf [ P.symbol "#", P.succeed () ]
        |> P.getChompedString


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
        [ P.succeed (\dir x -> { noMove | rotate = vectorIn dir x })
            |= P.oneOf
                [ P.succeed Direction3d.x |. P.keyword "pitch"
                , P.succeed Direction3d.y |. P.keyword "yaw"
                , P.succeed Direction3d.z |. P.keyword "roll"
                ]
        , P.succeed (\dir x -> { noMove | translate = vectorIn dir x })
            |= P.oneOf
                [ P.succeed Direction3d.positiveY |. P.keyword "up"
                , P.succeed Direction3d.negativeY |. P.keyword "down"
                , P.succeed Direction3d.positiveX |. P.keyword "right"
                , P.succeed Direction3d.negativeX |. P.keyword "left"
                , P.succeed Direction3d.positiveZ |. P.keyword "forward"
                , P.succeed Direction3d.negativeZ |. P.keyword "back"
                ]
        ]
        |. P.spaces
        |= P.oneOf [ P.succeed negate |. P.symbol "-" |= P.float, P.float ]


break : P.Parser ()
break =
    P.chompIf (\x -> x == '-')


merge : Move -> Move -> Move
merge a b =
    { rotate = Vector3d.plus a.rotate b.rotate
    , translate = Vector3d.plus a.translate b.translate
    }


vectorIn : Direction3d () -> Float -> Vector3d Length.Meters ()
vectorIn dir length =
    Vector3d.withLength (Length.meters length) dir


noMove : Move
noMove =
    Move Vector3d.zero Vector3d.zero



-- MIDI STUFF


type alias Clock =
    { index : Int
    , last : Float
    , samples : Array Float
    , averageMillis : Float
    }


initialClock : Clock
initialClock =
    clockFromSamples 0 0 Array.empty


clockFromSamples : Float -> Int -> Array Float -> Clock
clockFromSamples last index samples =
    Clock index last samples <|
        case Array.length samples of
            0 ->
                1000

            n ->
                24 * Array.foldl (+) 0 samples / toFloat n


samplesToKeep : Int
samplesToKeep =
    24 * 4 * 4


applyMidi : Float -> List Int -> Model -> Model
applyMidi time data ({ dance } as model) =
    case data of
        -- CLOCK
        [ 248 ] ->
            let
                newClock =
                    updateClock time model.clock

                newDance =
                    if
                        progress time newClock.averageMillis
                            > progress model.clock.last newClock.averageMillis
                    then
                        dance
                    else
                        { dance | prev = dance.next, next = nextPose model.lastNote dance }
            in
            { model | dance = newDance, clock = newClock }

        -- NOTE ON CH1
        [ 128, midinote, velocity ] ->
            case Array.get (modBy 12 midinote) notes of
                Nothing ->
                    model

                Just note ->
                    -- TODO
                    -- Try to change dance.{prev,next} if time is close enough to clock.last
                    -- How do I base timing off of notes _and_ clock? (should stay synced after hitting stop/play)
                    { model | lastNote = note }

        _ ->
            model


updateClock : Float -> Clock -> Clock
updateClock time clock =
    let
        diff =
            time - clock.last
    in
    if Array.length clock.samples < samplesToKeep then
        clockFromSamples time clock.index (Array.push diff clock.samples)
    else
        clockFromSamples time (clock.index + 1) <|
            Array.set (modBy samplesToKeep clock.index) diff clock.samples


notes : Array String
notes =
    Array.fromList [ "C", "C#", "D", "D#", "E", "F", "F#", "G", "G#", "A", "A#", "B" ]


port midiMessage : (( Float, List Int ) -> msg) -> Sub msg



-- RENDER


view : Model -> Html Msg
view model =
    Html.main_ []
        [ viewSubject model
        , Html.textarea
            [ Html.Events.onInput SetCode
            , Html.Attributes.autofocus True
            , Html.Attributes.spellcheck False
            , Html.Attributes.classList [ ( "error", model.dance.error ) ]
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
        , width = Pixels.pixels model.width
        , height = Pixels.pixels model.height
        , ambientLighting = Just ambientLighting
        , lights = Scene3d.oneLight sunlight { castsShadows = False }
        , exposure = Scene3d.Exposure.fromMaxLuminance (Luminance.nits 10000)
        , whiteBalance = Scene3d.Chromaticity.daylight
        }
        [ animate model head .head
        , animate model torso.drawable .torso
            |> torso.translation
        , animate model arm.drawable .leftArm
            |> arm.translation
            |> Drawable.translateIn Direction3d.negativeX armOffset
        , animate model arm.drawable .rightArm
            |> arm.translation
            |> Drawable.translateIn Direction3d.positiveX armOffset
        , animate model leg.drawable .leftLeg
            |> leg.translation
            |> Drawable.translateIn Direction3d.negativeX legOffset
        , animate model leg.drawable .rightLeg
            |> leg.translation
            |> Drawable.translateIn Direction3d.positiveX legOffset
        ]


head : Drawable a
head =
    bodyPart <| Shape.sphere { radius = var.headRadius, subdivisions = 72 }


torso : { drawable : Drawable a, translation : Drawable a -> Drawable a }
torso =
    pill torsoRadius var.torsoLength <| Quantity.plus var.spacing var.headRadius


arm : { drawable : Drawable a, translation : Drawable a -> Drawable a }
arm =
    pill var.limbRadius var.armLength <| Quantity.plus var.spacing var.headRadius


leg : { drawable : Drawable a, translation : Drawable a -> Drawable a }
leg =
    pill var.limbRadius var.legLength <|
        Quantity.sum [ var.headRadius, var.spacing, var.torsoLength, var.spacing ]


pill : Length -> Length -> Length -> { drawable : Drawable a, translation : Drawable a -> Drawable a }
pill radius length offset =
    let
        height =
            Quantity.minus (Quantity.twice radius) length

        halfHeight =
            Quantity.half height

        end =
            bodyPart <| Shape.sphere { radius = radius, subdivisions = 72 }

        trunk =
            bodyPart <| Shape.cylinder { radius = radius, height = height, subdivisions = 72 }
    in
    { drawable =
        Drawable.group
            [ Drawable.translateIn Direction3d.negativeZ halfHeight end
            , Drawable.translateIn Direction3d.positiveZ halfHeight end
            , Drawable.translateIn Direction3d.negativeZ halfHeight trunk
            ]
            |> Drawable.rotateAround Axis3d.x (Angle.degrees 90)
    , translation =
        Quantity.sum [ offset, radius, halfHeight ]
            |> Drawable.translateIn Direction3d.negativeY
    }


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

        rotation =
            curveVector model a.rotate b.rotate

        translation =
            curveVector model a.translate b.translate
    in
    drawable
        |> Drawable.rotateAround
            (Vector3d.direction rotation
                |> Maybe.withDefault Direction3d.x
                |> Axis3d.through Point3d.origin
            )
            (Angle.degrees (unQuantity (Vector3d.length rotation)))
        |> Drawable.translateBy translation


curveVector : Model -> Vector3d Length.Meters () -> Vector3d Length.Meters () -> Vector3d Length.Meters ()
curveVector model a b =
    Vector3d.meters
        (curve model Vector3d.xComponent a b)
        (curve model Vector3d.yComponent a b)
        (curve model Vector3d.zComponent a b)


curve : Model -> (a -> Quantity Float b) -> a -> a -> Float
curve model unwrap a b =
    cubicBezier (progress model.clock.last model.clock.averageMillis)
        (unQuantity (unwrap a))
        (unQuantity (unwrap b))


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


progress : Float -> Float -> Float
progress time step =
    let
        ratio =
            time / step
    in
    ratio - toFloat (floor ratio)


unQuantity : Quantity number a -> number
unQuantity (Quantity x) =
    x
