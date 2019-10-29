port module Visualizer exposing (main)

import Angle exposing (Angle)
import Array exposing (Array)
import Axis3d
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
    viewSubject model
        (cycle (Array.get 0 model.channels) vary.colors)
        (cycle (Array.get 1 model.channels) vary.colors)
        (cycle (Array.get 2 model.channels) vary.colors)
        (cycle (Array.get 3 model.channels) vary.colors)
        (cycle (Array.get 4 model.channels) vary.scales)
        (cycle (Array.get 5 model.channels) vary.rotations)
        (cycle (Array.get 6 model.channels) vary.rotations)
        (cycle (Array.get 7 model.channels) vary.rotations)


viewSubject : Model -> Color -> Color -> Color -> Color -> Float -> Angle -> Angle -> Angle -> Html msg
viewSubject model color1 color2 color3 color4 scale xAngle yAngle zAngle =
    let
        viewpoint =
            Viewpoint3d.orbit
                { groundPlane = SketchPlane3d.xy
                , azimuth = Angle.degrees 0
                , elevation = Angle.degrees 0
                , focalPoint = Point3d.meters 0 0 0
                , distance = Length.meters 8
                }

        camera =
            Camera3d.perspective
                { viewpoint = viewpoint
                , verticalFieldOfView = Angle.degrees 30
                , clipDepth = Length.meters 0.1
                }

        light1 =
            Light.directional
                (Chromaticity.fromColor color1)
                (Illuminance.lux 10000)
                (Direction3d.yz (Angle.degrees 45))

        light2 =
            Light.directional
                (Chromaticity.fromColor color2)
                (Illuminance.lux 10000)
                (Direction3d.yz (Angle.degrees 135))

        light3 =
            Light.directional
                (Chromaticity.fromColor color3)
                (Illuminance.lux 10000)
                (Direction3d.yz (Angle.degrees 225))

        light4 =
            Light.directional
                (Chromaticity.fromColor color4)
                (Illuminance.lux 10000)
                (Direction3d.yz (Angle.degrees 315))

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
        , lights = Scene3d.fourLights ( light1, { castsShadows = False } ) light2 light3 light4
        , exposure = Exposure.fromMaxLuminance (Luminance.nits 10000)
        , whiteBalance = Chromaticity.daylight
        }
        [ Drawable.physical aluminum subject
            |> Drawable.scaleAbout Point3d.origin scale
            |> Drawable.rotateAround Axis3d.x xAngle
            |> Drawable.rotateAround Axis3d.y yAngle
            |> Drawable.rotateAround Axis3d.z zAngle
        ]


subject : Mesh a (Mesh.Triangles Mesh.WithNormals Mesh.NoUV Mesh.NoTangents Mesh.ShadowsDisabled)
subject =
    let
        side =
            Length.meters 0.45
    in
    -- Shape.block side side side
    Shape.sphere { radius = side, subdivisions = 72 }


cycle : Maybe Int -> Array b -> b
cycle channel values =
    let
        index =
            modBy (Array.length values) (Maybe.withDefault 0 channel)
    in
    case Array.get index values of
        Just value ->
            value

        _ ->
            -- UNTO INFINITY
            cycle Nothing values


vary =
    { colors =
        Array.fromList
            [ Color.red
            , Color.orange
            , Color.yellow
            , Color.green
            , Color.blue
            , Color.purple
            , Color.grey
            ]
    , scales =
        Array.fromList
            [ 1.0
            , 1.1
            , 1.25
            , 1.45
            , 1.7
            , 2.0
            , 2.35
            , 2.75
            , 3.2
            , 3.7
            ]
    , rotations =
        Array.fromList
            [ Angle.degrees 30
            , Angle.degrees 15
            , Angle.degrees 30
            , Angle.degrees 45
            , Angle.degrees 60
            , Angle.degrees 45
            , Angle.degrees 60
            , Angle.degrees 75
            , Angle.degrees 60
            , Angle.degrees 75
            , Angle.degrees 90
            , Angle.degrees 75
            , Angle.degrees 60
            , Angle.degrees 75
            , Angle.degrees 60
            , Angle.degrees 45
            , Angle.degrees 60
            , Angle.degrees 45
            , Angle.degrees 30
            ]
    }


{-| <https://github.com/ianmackenzie/elm-3d-scene/blob/85ee07e1454a0270ead17c0319688ccf091ade2f/examples/Common/Materials.elm#L19-L21>
-}
aluminum : Material
aluminum =
    { baseColor = Color.rgb255 233 235 236, roughness = 0.6, metallic = True }
