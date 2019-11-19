port module Visualizer exposing (main)

import Angle exposing (Angle)
import Array exposing (Array)
import Axis3d exposing (Axis3d)
import Bitwise
import Browser
import Browser.Events
import Camera3d
import Color exposing (Color)
import Direction3d
import Html exposing (Html)
import Illuminance as Illuminance
import Length exposing (Length)
import Luminance as Luminance
import Pixels
import Point3d
import Quantity
import Scene3d
import Scene3d.Chromaticity as Chromaticity
import Scene3d.Drawable as Drawable exposing (Drawable, Material)
import Scene3d.Exposure as Exposure
import Scene3d.Light as Light
import Scene3d.Mesh as Mesh exposing (Mesh)
import Scene3d.Shape as Shape
import SketchPlane3d
import Triangle3d
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
    { step : Float
    , time : Float
    , width : Float
    , height : Float
    , channels : Array Int
    }


type alias Flags =
    { width : Float, height : Float }


init : Flags -> ( Model, Cmd Msg )
init flags =
    ( { step = 750
      , time = 0
      , width = flags.width
      , height = flags.height
      , channels = Array.repeat 8 0
      }
    , Cmd.none
    )


type Msg
    = Diff Float
    | Resize Int Int
    | GotMidiMessage (List Int)


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        Diff diff ->
            pure { model | time = model.time + diff }

        Resize width height ->
            pure { model | width = toFloat width, height = toFloat height }

        GotMidiMessage data ->
            case data of
                [ status, note, velocity ] ->
                    let
                        command =
                            Bitwise.shiftRightBy 4 status

                        channel =
                            Bitwise.and 0x07 status
                    in
                    case ( command, Array.get channel model.channels ) of
                        ( 8, Just (oldValue as old) ) ->
                            pure { model | channels = Array.set channel (oldValue + 1) model.channels }

                        _ ->
                            pure model

                _ ->
                    pure model


pure : a -> ( a, Cmd msg )
pure a =
    ( a, Cmd.none )


port midiMessage : (List Int -> msg) -> Sub msg


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.batch
        [ midiMessage GotMidiMessage
        , Browser.Events.onAnimationFrameDelta Diff
        , Browser.Events.onResize Resize
        ]


view : Model -> Html msg
view model =
    -- TODO add parameters that vary with channel data
    viewSubject model


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
            Light.directional Chromaticity.daylight
                (Illuminance.lux 10000)
                (Direction3d.zxY (Angle.degrees 45) (Angle.degrees 195))

        ambientLighting =
            Light.overcast
                { zenithDirection = Direction3d.z
                , chromaticity = Chromaticity.daylight
                , zenithLuminance = Luminance.nits 5000
                }
    in
    Scene3d.render [ Scene3d.clearColor Color.black ]
        { camera = camera
        , width = Pixels.pixels model.width
        , height = Pixels.pixels model.height
        , ambientLighting = Just ambientLighting
        , lights = Scene3d.oneLight sunlight { castsShadows = False }
        , exposure = Exposure.fromMaxLuminance (Luminance.nits 10000)
        , whiteBalance = Chromaticity.daylight
        }
    <|
        [ head
            |> Drawable.rotateAround
                (jointAtY <| Quantity.plus var.headRadius var.spacing)
                (Angle.degrees <| curve model.time [ 15, -10, 10, -15, 10, -10 ])
        , torso
            |> Drawable.translateIn Direction3d.negativeY
                (Quantity.divideBy 2 var.torsoLength
                    |> Quantity.plus var.headRadius
                    |> Quantity.plus var.spacing
                )
            |> Drawable.rotateAround
                (jointAtY <| Quantity.sum [ var.headRadius, var.spacing, var.torsoLength ])
                (Angle.degrees <| curve model.time [ -5, 5 ])
        , arm
            |> Drawable.rotateAround Axis3d.z (Angle.degrees <| curve model.time [ 10, -10 ])
            |> Drawable.rotateAround Axis3d.x (Angle.degrees <| curve model.time [ -15, -15, 15 ])
            |> Drawable.translateIn Direction3d.x armOffset
        , arm
            |> Drawable.rotateAround Axis3d.z (Angle.degrees <| curve model.time [ 10, -10 ])
            |> Drawable.rotateAround Axis3d.x (Angle.degrees <| curve model.time [ -15, -15, 15 ])
            |> Drawable.translateIn Direction3d.negativeX armOffset
        , leg
            |> Drawable.rotateAround
                (jointAtY (Quantity.multiplyBy 2 var.legLength))
                (Angle.degrees <| curve model.time [ 5, -5 ])
            |> Drawable.translateIn Direction3d.x legOffset
        , leg
            |> Drawable.rotateAround
                (jointAtY (Quantity.multiplyBy 2 var.legLength))
                (Angle.degrees <| curve model.time [ 5, -5 ])
            |> Drawable.translateIn Direction3d.negativeX legOffset
        ]


head : Drawable a
head =
    bodyPart <|
        Shape.sphere
            { radius = var.headRadius
            , subdivisions = 72
            }


leg : Drawable a
leg =
    limb var.legLength
        |> Drawable.translateIn Direction3d.negativeY
            (Quantity.plus var.spacing var.headRadius
                |> Quantity.plus var.torsoLength
                |> Quantity.plus var.legLength
            )


arm : Drawable a
arm =
    limb var.armLength
        |> Drawable.translateIn Direction3d.negativeY
            (Quantity.plus var.spacing var.headRadius
                |> Quantity.plus var.armLength
            )


limb : Length -> Drawable a
limb length =
    let
        height =
            Quantity.minus limbRadius length

        trunk =
            Shape.cylinder
                { radius = limbRadius
                , height = height
                , subdivisions = 72
                }
    in
    Drawable.group
        [ bodyPart trunk
        , extremity
        , Drawable.translateIn Direction3d.z height extremity
        ]
        |> Drawable.rotateAround Axis3d.x (Angle.degrees 270)


extremity : Drawable a
extremity =
    bodyPart <|
        Shape.sphere
            { radius = Quantity.divideBy 2 var.limbWidth
            , subdivisions = 72
            }


torso : Drawable a
torso =
    bodyPart <|
        Shape.block (Quantity.sum [ var.limbWidth, var.spacing, var.limbWidth ])
            var.torsoLength
            var.limbWidth


bodyPart : Mesh a (Mesh.Triangles Mesh.WithNormals uv tangents shadows) -> Drawable a
bodyPart =
    Drawable.physical
        { baseColor = Color.white
        , roughness = 0.25
        , metallic = False
        }


jointAtY : Length -> Axis3d Length.Meters c
jointAtY y =
    Axis3d.withDirection Direction3d.z <|
        Point3d.xyz Quantity.zero (Quantity.negate y) Quantity.zero


limbRadius =
    Quantity.divideBy 2 var.limbWidth


armOffset =
    Quantity.plus var.limbWidth limbRadius
        |> Quantity.plus (Quantity.multiplyBy 2 var.spacing)


legOffset =
    Quantity.plus var.spacing limbRadius


var =
    { spacing = Length.meters 0.045
    , headRadius = Length.meters 0.85
    , limbWidth = Length.meters 0.75
    , armLength = Length.meters 2.0
    , legLength = Length.meters 3.0
    , torsoLength = Length.meters 2.5
    , movement = Length.meters 0.85
    }



-- CUBIC BEZIER
-- from earlier experiments


stepDuration =
    350


curve : Float -> List Float -> Float
curve delta steps =
    let
        cycles =
            delta / stepDuration

        cyclesCompleted =
            truncate cycles
    in
    case
        List.drop
            (modBy (List.length steps) cyclesCompleted)
            (steps ++ List.take 1 steps)
    of
        first :: second :: _ ->
            cubicBezier (cycles - toFloat cyclesCompleted) first second

        _ ->
            0


cubicBezier : Float -> Float -> Float -> Float
cubicBezier t p0 p3 =
    let
        controlPoint1 =
            1.05

        controlPoint2 =
            0.75

        p1 =
            p0 + controlPoint1 * (p3 - p0)

        p2 =
            p0 + controlPoint2 * (p3 - p0)
    in
    (p0 * ((1 - t) ^ 3))
        + (p1 * 3 * ((1 - t) ^ 2) * t)
        + (p2 * 3 * (1 - t) * (t ^ 2))
        + (p3 * (t ^ 3))
