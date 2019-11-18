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
        let
            arm_ dir =
                bodyPart arm
                    |> Drawable.rotateAround Axis3d.x (Angle.degrees 270)
                    |> Drawable.translateIn Direction3d.negativeY
                        (Quantity.plus var.spacing var.headRadius
                            |> Quantity.plus var.armLength
                        )
                    |> Drawable.translateIn dir
                        (Quantity.divideBy 2 var.limbWidth
                            |> Quantity.plus var.limbWidth
                            |> Quantity.plus (Quantity.multiplyBy 2 var.spacing)
                        )

            leg_ dir =
                bodyPart leg
                    |> Drawable.rotateAround Axis3d.x (Angle.degrees 270)
                    |> Drawable.translateIn Direction3d.negativeY
                        (Quantity.plus var.spacing var.headRadius
                            |> Quantity.plus var.torsoLength
                            |> Quantity.plus var.legLength
                        )
                    |> Drawable.translateIn dir
                        (Quantity.divideBy 2 var.limbWidth
                            |> Quantity.plus var.spacing
                        )

            swing cadence =
                Angle.degrees (sin (model.time / cadence) * 5)
        in
        [ bodyPart head
            |> Drawable.rotateAround hips (swing 180)
        , bodyPart torso
            |> Drawable.translateIn Direction3d.negativeY
                (Quantity.divideBy 2 var.torsoLength
                    |> Quantity.plus var.headRadius
                    |> Quantity.plus var.spacing
                )
            |> Drawable.rotateAround hips (swing 120)
        , arm_ Direction3d.x
            |> Drawable.rotateAround hips (Quantity.negate (swing 180))
        , arm_ Direction3d.negativeX
            |> Drawable.rotateAround hips (Quantity.negate (swing 180))
        , leg_ Direction3d.x
            |> Drawable.rotateAround hips (Quantity.negate (swing 120))
        , leg_ Direction3d.negativeX
            |> Drawable.rotateAround hips (Quantity.negate (swing 120))
        ]


head : Mesh a (Mesh.Triangles Mesh.WithNormals Mesh.NoUV Mesh.NoTangents Mesh.ShadowsDisabled)
head =
    Shape.sphere
        { radius = var.headRadius
        , subdivisions = 72
        }


leg : Mesh a (Mesh.Triangles Mesh.WithNormals Mesh.NoUV Mesh.NoTangents Mesh.ShadowsDisabled)
leg =
    Shape.cylinder
        { radius = Quantity.divideBy 2 var.limbWidth
        , height = var.legLength
        , subdivisions = 72
        }


arm : Mesh a (Mesh.Triangles Mesh.WithNormals Mesh.NoUV Mesh.NoTangents Mesh.ShadowsDisabled)
arm =
    Shape.cylinder
        { radius = Quantity.divideBy 2 var.limbWidth
        , height = var.armLength
        , subdivisions = 72
        }


extremity : Mesh a (Mesh.Triangles Mesh.WithNormals Mesh.NoUV Mesh.NoTangents Mesh.ShadowsDisabled)
extremity =
    Shape.sphere
        { radius = Quantity.divideBy 2 var.limbWidth
        , subdivisions = 72
        }


torso : Mesh a (Mesh.Triangles Mesh.WithNormals Mesh.NoUV Mesh.NoTangents Mesh.ShadowsDisabled)
torso =
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


hips : Axis3d Length.Meters c
hips =
    let
        y =
            Quantity.negate <|
                Quantity.sum [ var.headRadius, var.spacing, var.torsoLength ]
    in
    Axis3d.withDirection Direction3d.z <|
        Point3d.xyz Quantity.zero y Quantity.zero


var =
    { spacing = Length.meters 0.025
    , headRadius = Length.meters 0.85
    , limbWidth = Length.meters 0.85
    , armLength = Length.meters 2.0
    , legLength = Length.meters 3.0
    , torsoLength = Length.meters 2.5
    , movement = Length.meters 0.85
    }
