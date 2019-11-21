port module Visualizer exposing (main)

import Angle exposing (Angle)
import Array exposing (Array)
import Axis3d exposing (Axis3d)
import Bitwise
import Browser
import Browser.Events
import Camera3d
import Color exposing (Color)
import Direction3d exposing (Direction3d)
import Html exposing (Html)
import Html.Attributes
import Html.Events
import Illuminance as Illuminance
import Json.Decode as D
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
    , channels : Array ( Int, Float )
    }


type alias Flags =
    { width : Float, height : Float }


init : Flags -> ( Model, Cmd Msg )
init flags =
    ( { time = 0
      , width = flags.width
      , height = flags.height
      , sync = Hardcoded floss
      , channels = Array.repeat 8 ( 0, 0 )
      }
    , Cmd.none
    )


type Msg
    = Diff Float
    | Resize Int Int
    | SetSync Sync
    | GotMidiMessage (List Int)


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        Diff diff ->
            pure { model | time = model.time + diff }

        Resize width height ->
            pure { model | width = toFloat width, height = toFloat height }

        SetSync sync_ ->
            pure { model | sync = sync_ }

        GotMidiMessage data ->
            case ( model.sync, data ) of
                ( Midi, [ status, note, velocity ] ) ->
                    let
                        command =
                            Bitwise.shiftRightBy 4 status

                        channel =
                            Bitwise.and 0x07 status
                    in
                    case ( command, Array.get channel model.channels ) of
                        ( 8, Just ( old, _ ) ) ->
                            pure { model | channels = Array.set channel ( old + 1, model.time ) model.channels }

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
        , Html.div [ Html.Attributes.class "controls" ] <|
            radio SetSync model.sync <|
                [ ( Hardcoded floss, Html.text "floss" )
                , ( Hardcoded macarena, Html.text "macarena" )
                , ( Midi, Html.b [] [ Html.text "midi" ] )
                ]
        ]


radio : (a -> msg) -> a -> List ( a, Html msg ) -> List (Html msg)
radio toMsg value =
    List.indexedMap <|
        \index ( thisValue, thisLabel ) ->
            Html.label []
                [ Html.input
                    [ Html.Attributes.name "radio"
                    , Html.Attributes.type_ "radio"
                    , Html.Attributes.value (String.fromInt index)
                    , Html.Attributes.checked (thisValue == value)
                    , onChange (String.fromInt index) (toMsg thisValue)
                    ]
                    []
                , Html.text " "
                , thisLabel
                ]


onChange : String -> msg -> Html.Attribute msg
onChange this msg =
    Html.Events.on "change" <|
        D.andThen
            (\x ->
                if x == this then
                    D.succeed msg
                else
                    D.fail ""
            )
            Html.Events.targetValue


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
    Scene3d.render [ Scene3d.clearColor (Color.rgb255 72 72 72) ]
        { camera = camera
        , width = Pixels.pixels model.width
        , height = Pixels.pixels model.height
        , ambientLighting = Just ambientLighting
        , lights = Scene3d.oneLight sunlight { castsShadows = False }
        , exposure = Exposure.fromMaxLuminance (Luminance.nits 10000)
        , whiteBalance = Chromaticity.daylight
        }
        [ dance model head .head
        , dance model torso .torso
        , dance model arm .leftArm
            |> Drawable.translateIn Direction3d.negativeX armOffset
        , dance model arm .rightArm
            |> Drawable.translateIn Direction3d.x armOffset
        , dance model leg .leftLeg
            |> Drawable.translateIn Direction3d.negativeX legOffset
        , dance model leg .rightLeg
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
    Quantity.plus var.headRadius var.limbRadius
        |> Quantity.half


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



-- SYNC + DANCE


type Sync
    = Hardcoded Dance
    | Midi


dance : Model -> Drawable () -> (Dance -> List Move) -> Drawable ()
dance model initial getter =
    case model.sync of
        Hardcoded dance_ ->
            List.foldl
                (\move ->
                    Drawable.rotateAround move.axis <|
                        Angle.degrees (sync model move.steps)
                )
                initial
                (getter dance_)

        Midi ->
            -- TODO
            initial


sync : Model -> List Float -> Float
sync model steps =
    case model.sync of
        Hardcoded dance_ ->
            let
                progress =
                    model.time / stepDuration

                step =
                    truncate progress
            in
            curve step (progress - toFloat step) steps

        Midi ->
            let
                ( step, start ) =
                    -- TODO
                    -- Array.get channel model.channels
                    --     |> Maybe.withDefault ( 0, 0 )
                    ( 0, 0 )
            in
            curve step (min 1 ((model.time - start) / stepDuration)) steps


type alias Dance =
    { head : List Move
    , torso : List Move
    , leftArm : List Move
    , rightArm : List Move
    , leftLeg : List Move
    , rightLeg : List Move
    }


type alias Move =
    { steps : List Float
    , axis : Axis3d Length.Meters ()
    }


floss : Dance
floss =
    { head =
        [ { steps = [ 15, -10, 10, -15, 10, -10 ], axis = shouldersZ } ]
    , torso =
        [ { steps = [ -5, 5 ], axis = hips } ]
    , leftArm =
        [ { steps = [ 10, -10 ], axis = Axis3d.z }
        , { steps = [ -15, -15, 15 ], axis = Axis3d.x }
        ]
    , rightArm =
        [ { steps = [ 10, -10 ], axis = Axis3d.z }
        , { steps = [ -15, -15, 15 ], axis = Axis3d.x }
        ]
    , leftLeg =
        [ { steps = [ 5, -5 ], axis = feet } ]
    , rightLeg =
        [ { steps = [ 5, -5 ], axis = feet } ]
    }


macarena : Dance
macarena =
    let
        shake vigor =
            List.map ((*) vigor) <|
                List.concat
                    [ [ 0, 0, 0, 0, 0, 0, 0, 0 ]
                    , [ 0, 0, 0, 0, 0, 0, 0, 0 ]
                    , [ 0, 0, 0, 0, 0, 0, 0, 0 ]
                    , [ 0, 0, 0, 0, 1, -1, 1, -1 ]
                    ]
    in
    { head =
        [ { steps = shake 5, axis = shouldersZ } ]
    , torso =
        [ { steps = shake 10, axis = hips } ]
    , leftArm =
        [ { steps =
                List.concat
                    [ [ 0, 0, 0, 0, -5, 0, 0, 0 ]
                    , [ -15, -15, -15, -15, 30, 30, 30, 30 ]
                    , [ 0, 0, 0, 0, 30, 30, 30, 30 ]
                    , [ 0, 0, 0, 0, 0, 0, 0, 0 ]
                    ]
          , axis = shouldersZ
          }
        , { steps =
                List.concat
                    [ [ 0, 0, 0, 0, -90, -90, -90, -90 ]
                    , [ -95, -90, -90, -90, -105, -105, -105, -105 ]
                    , [ -150, -150, -150, -150, -60, -60, -60, -60 ]
                    , [ 0, 0, 0, 0, 0, 0, 0, 0 ]
                    ]
          , axis = shouldersX
          }
        , { steps =
                List.concat
                    [ [ 0, 0, 0, 0, 0, 0, 0, 0 ]
                    , [ 0, 0, 0, 0, 0, 0, 0, 0 ]
                    , [ 0, 0, 0, 0, 0, 0, 0, 0 ]
                    , [ 10, 10, 10, 10, 10, -10, 10, -10 ]
                    ]
          , axis = hips
          }
        ]
    , rightArm =
        [ { steps =
                List.concat
                    [ [ 0, 0, 0, 0, 0, 0, 5, 0, 0, 0 ]
                    , [ 15, 15, 15, 15, -30, -30, -30, -30 ]
                    , [ 0, 0, 0, 0, -30, -30, -30, -30 ]
                    , [ 0, 0, 0, 0, 0, 0 ]
                    ]
          , axis = shouldersZ
          }
        , { steps =
                List.concat
                    [ [ 0, 0, 0, 0, 0, 0, -90, -90, -90, -90 ]
                    , [ -95, -90, -90, -90, -105, -105, -105, -105 ]
                    , [ -150, -150, -150, -150, -60, -60, -60, -60 ]
                    , [ 0, 0, 0, 0, 0, 0 ]
                    ]
          , axis = shouldersX
          }
        , { steps =
                List.concat
                    [ [ 0, 0, 0, 0, 0, 0, 0, 0, 0, 0 ]
                    , [ 0, 0, 0, 0, 0, 0, 0, 0 ]
                    , [ 0, 0, 0, 0, 0, 0, 0, 0 ]
                    , [ -10, -10, -10, -10, 10, -10 ]
                    ]
          , axis = hips
          }
        ]
    , leftLeg =
        [ { steps = shake -5, axis = feet } ]
    , rightLeg =
        [ { steps = shake -5, axis = feet } ]
    }


shouldersX : Axis3d Length.Meters c
shouldersX =
    shoulders Direction3d.x


shouldersZ : Axis3d Length.Meters c
shouldersZ =
    shoulders Direction3d.z


shoulders : Direction3d c -> Axis3d Length.Meters c
shoulders direction =
    joint direction <| Quantity.plus var.headRadius var.spacing


hips : Axis3d Length.Meters c
hips =
    joint Direction3d.z <| Quantity.sum [ var.headRadius, var.spacing, var.torsoLength ]


feet : Axis3d Length.Meters c
feet =
    joint Direction3d.z <| Quantity.sum [ var.headRadius, var.spacing, var.torsoLength, var.spacing, var.legLength ]


joint : Direction3d c -> Length -> Axis3d Length.Meters c
joint dir y =
    Axis3d.withDirection dir <|
        Point3d.xyz Quantity.zero (Quantity.negate y) Quantity.zero



-- CUBIC BEZIER
-- from earlier experiments


stepDuration =
    280


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
