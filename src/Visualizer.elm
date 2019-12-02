port module Visualizer exposing (main)

import Angle exposing (Angle)
import Array exposing (Array)
import Axis3d exposing (Axis3d)
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
    , code : String
    , prev : Body Move
    , next : Body Move
    , plan : List (Body Move)
    , clock : Clock
    , channels : Array ( Int, Int, Float )
    }


type alias Clock =
    { diffPerBeat : Float
    , index : Int
    , history : Array Float
    }


type alias Body a =
    { head : a
    , torso : a
    , leftArm : a
    , rightArm : a
    , leftLeg : a
    , rightLeg : a
    }


type Move
    = Vertical Float
    | Horizontal Float


type alias Flags =
    { width : Float, height : Float }


init : Flags -> ( Model, Cmd Msg )
init flags =
    let
        code =
            String.join "\n"
                [ "rightarm up    leftarm down"
                , "rightarm down  leftarm up"
                ]

        ( prev, next, plan ) =
            parseDance code
    in
    ( { time = 0
      , width = flags.width
      , height = flags.height
      , code = code
      , prev = prev
      , next = next
      , plan = plan
      , clock = Clock 0 0 (Array.repeat 24 0.0)
      , channels = Array.repeat 8 ( 0, 0, 0 )
      }
    , Cmd.none
    )


parseDance : String -> ( Body Move, Body Move, List (Body Move) )
parseDance code =
    let
        parse body line =
            case line of
                "leftarm" :: "up" :: rest ->
                    parse { body | leftArm = Vertical 0.15 } rest

                "leftarm" :: "down" :: rest ->
                    parse { body | leftArm = Vertical -0.15 } rest

                "rightarm" :: "up" :: rest ->
                    parse { body | rightArm = Vertical 0.15 } rest

                "rightarm" :: "down" :: rest ->
                    parse { body | rightArm = Vertical -0.15 } rest

                "head" :: "left" :: rest ->
                    parse { body | head = Horizontal -0.15 } rest

                "head" :: "right" :: rest ->
                    parse { body | head = Horizontal 0.15 } rest

                _ ->
                    body
    in
    case
        String.lines code
            |> List.map
                (String.split " "
                    >> List.filter (String.isEmpty >> not)
                    >> parse neutral
                )
    of
        (first :: second :: _) as all ->
            ( first, second, all )

        _ ->
            ( neutral, neutral, [] )


neutral : Body Move
neutral =
    { head = Vertical 0
    , torso = Vertical 0
    , leftArm = Vertical 0
    , rightArm = Vertical 0
    , leftLeg = Vertical 0
    , rightLeg = Vertical 0
    }


type Msg
    = Diff Float
    | Resize Int Int
    | SetCode String
    | GotMidiMessage (List Int)


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        Diff diff ->
            let
                newTime =
                    model.time + diff
            in
            if newTime <= stepDuration then
                pure { model | time = newTime }
            else
                let
                    ( next, plan ) =
                        case model.plan of
                            [] ->
                                ( model.next, model.plan )

                            first :: rest ->
                                ( first, rest ++ [ first ] )
                in
                pure { model | time = newTime - stepDuration, prev = model.next, next = next, plan = plan }

        Resize width height ->
            pure { model | width = toFloat width, height = toFloat height }

        SetCode code ->
            let
                ( _, _, plan ) =
                    parseDance code
            in
            pure { model | code = code, plan = plan }

        GotMidiMessage data ->
            pure (applyMidi data model)


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
            , Html.Attributes.style "width" (String.fromInt (floor model.width // 2) ++ "px")
            , Html.Events.onInput SetCode
            ]
            [ Html.text model.code ]
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
        [ dance model head Head
        , dance model torso Torso
        , dance model arm LeftArm
            |> Drawable.translateIn Direction3d.negativeX armOffset
        , dance model arm RightArm
            |> Drawable.translateIn Direction3d.x armOffset
        , dance model leg LeftLeg
            |> Drawable.translateIn Direction3d.negativeX legOffset
        , dance model leg RightLeg
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



-- DANCE


type Part
    = Head
    | Torso
    | LeftArm
    | RightArm
    | LeftLeg
    | RightLeg


part : Part -> Body a -> a
part part_ body =
    case part_ of
        Head ->
            body.head

        Torso ->
            body.torso

        LeftArm ->
            body.leftArm

        RightArm ->
            body.rightArm

        LeftLeg ->
            body.leftLeg

        RightLeg ->
            body.rightLeg


dance : Model -> Drawable () -> Part -> Drawable ()
dance model drawable part_ =
    let
        ( verticalA, horizontalA ) =
            case part part_ model.prev of
                Vertical a ->
                    ( a, 0 )

                Horizontal a ->
                    ( 0, a )

        ( verticalB, horizontalB ) =
            case part part_ model.next of
                Vertical b ->
                    ( b, 0 )

                Horizontal b ->
                    ( 0, b )

        distance a b =
            Length.meters (cubicBezier (model.time / stepDuration) a b)
    in
    drawable
        |> Drawable.translateIn Direction3d.y (distance verticalA verticalB)
        |> Drawable.translateIn Direction3d.x (distance horizontalA horizontalB)


stepDuration : Float
stepDuration =
    350


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
