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
import Html.Attributes
import Html.Events
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
    { time : Float
    , width : Float
    , height : Float
    , sync : Sync
    }


type alias Flags =
    { width : Float, height : Float }


init : Flags -> ( Model, Cmd Msg )
init flags =
    ( { time = 0
      , width = flags.width
      , height = flags.height
      , sync = Time
      }
    , Cmd.none
    )


type Msg
    = Diff Float
    | Resize Int Int
    | SetToMusic Bool
    | GotMidiMessage (List Int)


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        Diff diff ->
            pure { model | time = model.time + diff }

        Resize width height ->
            pure { model | width = toFloat width, height = toFloat height }

        SetToMusic True ->
            pure { model | sync = Midi <| Array.repeat 8 ( 0, 0 ) }

        SetToMusic False ->
            pure { model | sync = Time }

        GotMidiMessage data ->
            case ( model.sync, data ) of
                ( Midi channels, [ status, note, velocity ] ) ->
                    let
                        command =
                            Bitwise.shiftRightBy 4 status

                        channel =
                            Bitwise.and 0x07 status
                    in
                    case ( command, Array.get channel channels ) of
                        ( 8, Just ( old, _ ) ) ->
                            pure { model | sync = Midi <| Array.set channel ( old + 1, model.time ) channels }

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


view : Model -> Html Msg
view model =
    Html.div [ Html.Attributes.style "position" "relative" ]
        [ viewSubject model
        , Html.label
            [ Html.Attributes.style "position" "absolute"
            , Html.Attributes.style "top" "0"
            , Html.Attributes.style "right" "0"
            , Html.Attributes.style "background" "white"
            , Html.Attributes.style "padding" "16px"
            , Html.Attributes.style "border-bottom-left-radius" "4px"
            , Html.Attributes.style "cursor" "pointer"
            , Html.Attributes.style "font-family" "monospace"
            ]
            [ Html.text "to music: "
            , Html.input
                [ Html.Attributes.type_ "checkbox"
                , Html.Attributes.checked (model.sync /= Time)
                , Html.Events.onCheck SetToMusic
                ]
                []
            ]
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
        [ head
            |> Drawable.rotateAround shoulders
                (Angle.degrees <| sync model 5 [ 15, -10, 10, -15, 10, -10 ])
        , torso
            |> Drawable.rotateAround hips (Angle.degrees <| sync model 1 [ -5, 5 ])
        , arm
            |> Drawable.rotateAround Axis3d.z (Angle.degrees <| sync model 0 [ 10, -10 ])
            |> Drawable.rotateAround Axis3d.x (Angle.degrees <| sync model 0 [ -15, -15, 15 ])
            |> Drawable.translateIn Direction3d.x armOffset
        , arm
            |> Drawable.rotateAround Axis3d.z (Angle.degrees <| sync model 0 [ 10, -10 ])
            |> Drawable.rotateAround Axis3d.x (Angle.degrees <| sync model 0 [ -15, -15, 15 ])
            |> Drawable.translateIn Direction3d.negativeX armOffset
        , leg
            |> Drawable.rotateAround feet (Angle.degrees <| sync model 1 [ 5, -5 ])
            |> Drawable.translateIn Direction3d.x legOffset
        , leg
            |> Drawable.rotateAround feet (Angle.degrees <| sync model 1 [ 5, -5 ])
            |> Drawable.translateIn Direction3d.negativeX legOffset
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
    let
        radius =
            Quantity.plus var.headRadius var.limbRadius
                |> Quantity.half
    in
    pill radius var.torsoLength
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


shoulders : Axis3d Length.Meters c
shoulders =
    jointAtY <| Quantity.plus var.headRadius var.spacing


hips : Axis3d Length.Meters c
hips =
    jointAtY <| Quantity.sum [ var.headRadius, var.spacing, var.torsoLength ]


feet : Axis3d Length.Meters c
feet =
    jointAtY <| Quantity.sum [ var.headRadius, var.spacing, var.torsoLength, var.spacing, var.legLength ]


jointAtY : Length -> Axis3d Length.Meters c
jointAtY y =
    Axis3d.withDirection Direction3d.z <|
        Point3d.xyz Quantity.zero (Quantity.negate y) Quantity.zero


armOffset : Length
armOffset =
    Quantity.plus (Quantity.twice var.limbRadius) var.limbRadius
        |> Quantity.plus (Quantity.twice var.spacing)


legOffset : Length
legOffset =
    Quantity.plus var.spacing var.limbRadius


var =
    { spacing = Length.meters 0.045
    , headRadius = Length.meters 0.85
    , limbRadius = Length.meters 0.375
    , armLength = Length.meters 2.0
    , legLength = Length.meters 3.0
    , torsoLength = Length.meters 2.5
    , movement = Length.meters 0.85
    }



-- TIMING


type Sync
    = Time
    | Midi (Array ( Int, Float ))


sync : { a | sync : Sync, time : Float } -> Int -> List Float -> Float
sync model channel steps =
    case model.sync of
        Time ->
            let
                cycles =
                    model.time / stepDuration

                cyclesCompleted =
                    truncate cycles
            in
            curve cyclesCompleted (cycles - toFloat cyclesCompleted) steps

        Midi channels ->
            let
                ( cyclesCompleted, start ) =
                    Array.get channel channels
                        |> Maybe.withDefault ( 0, 0 )
            in
            curve cyclesCompleted (min 1 ((model.time - start) / stepDuration)) steps



-- CUBIC BEZIER
-- from earlier experiments


stepDuration =
    350


curve : Int -> Float -> List Float -> Float
curve completed t steps =
    case
        List.drop
            (modBy (List.length steps) completed)
            (steps ++ List.take 1 steps)
    of
        first :: second :: _ ->
            cubicBezier t first second

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
