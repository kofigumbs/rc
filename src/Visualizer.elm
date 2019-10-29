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
import Length exposing (Length)
import Pixels
import Point3d
import Quantity
import Scene3d
import Scene3d.Drawable as Drawable exposing (Drawable)
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
    , channels : Array Envelope
    }


type Envelope
    = Unset
    | Sustained
    | Released Float


type alias Flags =
    { width : Float, height : Float }


init : Flags -> ( Model, Cmd Msg )
init flags =
    ( { time = 0
      , width = flags.width
      , height = flags.height
      , channels = Array.repeat trackCount Unset
      }
    , Cmd.none
    )


type Msg
    = Diff Float
    | Resize Int Int
    | GotMidiMessage (Array Int)


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        Diff diff ->
            ( { model | time = model.time + diff }
            , Cmd.none
            )

        Resize width height ->
            ( { model | width = toFloat width, height = toFloat height }, Cmd.none )

        GotMidiMessage data ->
            case Maybe.map3 dataToNote (Array.get 0 data) (Array.get 1 data) (Array.get 2 data) of
                Just (Note 8 channel note velocity) ->
                    ( { model | channels = Array.set channel (Released model.time) model.channels }, Cmd.none )

                Just (Note 9 channel note velocity) ->
                    ( { model | channels = Array.set channel Sustained model.channels }, Cmd.none )

                _ ->
                    ( model, Cmd.none )


type Note
    = Note Int Int Int Int


dataToNote : Int -> Int -> Int -> Note
dataToNote byte0 byte1 byte2 =
    Note (Bitwise.shiftRightBy 4 byte0) (Bitwise.and 0x07 byte0) byte1 byte2


trackIndexes : List Int
trackIndexes =
    List.range 0 (trackCount - 1)


trackCount : Int
trackCount =
    List.length trackColors


trackColors : List Color
trackColors =
    [ Color.red
    , Color.orange
    , Color.yellow
    , Color.green
    , Color.blue
    , Color.purple
    , Color.grey
    , Color.black
    ]


port midiMessage : (Array Int -> msg) -> Sub msg


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.batch
        [ midiMessage GotMidiMessage
        , Browser.Events.onAnimationFrameDelta Diff
        , Browser.Events.onResize Resize
        ]


view : Model -> Html Msg
view model =
    let
        viewpoint =
            Viewpoint3d.orbit
                { groundPlane = SketchPlane3d.xy
                , azimuth = Angle.degrees (model.time / 64)
                , elevation = Angle.degrees (model.time / 128 + 15)
                , focalPoint = Point3d.meters (cos (model.time / 720)) 0 0
                , distance = Length.meters (8 + sin (model.time / 720) * 2)
                }

        camera =
            Camera3d.perspective
                { viewpoint = viewpoint
                , verticalFieldOfView = Angle.degrees 30
                , clipDepth = Length.meters 0.1
                }
    in
    Scene3d.unlit [ Scene3d.clearColor Color.black ]
        { camera = camera
        , width = Pixels.pixels model.width
        , height = Pixels.pixels model.height
        }
        (List.map3 (drawTrack model.time) trackIndexes trackColors (Array.toList model.channels))


drawTrack : Float -> Int -> Color -> Envelope -> Drawable a
drawTrack time index base envelope =
    let
        ratio =
            case envelope of
                Unset ->
                    0

                Sustained ->
                    1

                Released at ->
                    max 0 (1 - (time - at) / 750)

        { hue, saturation } =
            Color.toHsla base

        color =
            Color.hsl hue saturation (ratio / 2)

        offset =
            Length.meters (toFloat index - ((toFloat trackCount - 1) / 2))
    in
    Drawable.colored color sphere
        |> Drawable.scaleAbout Point3d.origin ratio
        |> Drawable.translateIn Direction3d.x offset


sphere : Mesh a (Mesh.Triangles Mesh.WithNormals Mesh.NoUV Mesh.NoTangents Mesh.ShadowsDisabled)
sphere =
    Shape.sphere { radius = Length.meters 0.45, subdivisions = 72 }
