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
import Length
import Parameter1d
import Pixels
import Point3d
import Quantity
import Scene3d
import Scene3d.Drawable as Drawable exposing (Drawable)
import Scene3d.Mesh as Mesh
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
    { width : Float
    , height : Float
    , channels : Array Float
    }


type Envelope
    = Sustained
    | Releasing Float


type alias Flags =
    { width : Float, height : Float }


init : Flags -> ( Model, Cmd Msg )
init flags =
    ( { width = flags.width
      , height = flags.height
      , channels = Array.repeat trackCount 0
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
        Diff time ->
            ( { model | channels = Array.map ((+) -time) model.channels }, Cmd.none )

        Resize width height ->
            ( { model | width = toFloat width, height = toFloat height }, Cmd.none )

        GotMidiMessage data ->
            case Maybe.map3 dataToNote (Array.get 0 data) (Array.get 1 data) (Array.get 2 data) of
                Just (Note 9 channel note velocity) ->
                    ( { model | channels = Array.set channel holdTime model.channels }, Cmd.none )

                _ ->
                    ( model, Cmd.none )


type Note
    = Note Int Int Int Int


dataToNote : Int -> Int -> Int -> Note
dataToNote byte0 byte1 byte2 =
    Note (Bitwise.shiftRightBy 4 byte0) (Bitwise.and 0x07 byte0) byte1 byte2


holdTime : number
holdTime =
    750


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
    , Color.brown
    , Color.grey
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
            Viewpoint3d.lookAt
                { focalPoint = Point3d.meters 0 2 0
                , eyePoint = Point3d.meters 10 3 5
                , upDirection = Direction3d.y
                }

        camera =
            Camera3d.perspective
                { viewpoint = viewpoint
                , verticalFieldOfView = Angle.degrees 30
                , clipDepth = Length.meters 0.1
                }

        angles =
            Parameter1d.leading trackCount <|
                Quantity.interpolateFrom
                    (Angle.degrees 0)
                    (Angle.degrees 360)
    in
    Scene3d.unlit []
        { camera = camera
        , width = Pixels.pixels model.width
        , height = Pixels.pixels model.height
        }
        (List.map3 drawTrack angles trackColors (Array.toList model.channels))


drawTrack : Angle -> Color -> Float -> Drawable a
drawTrack angle base countdown =
    let
        { hue, saturation } =
            Color.toHsla base

        color =
            Color.hsl hue saturation (1 - (countdown / holdTime) / 2)
    in
    Drawable.group
        [ Drawable.colored color mesh1
        , Drawable.colored color mesh2
        ]
        |> Drawable.rotateAround rotationAxis angle


rotationAxis =
    Axis3d.through (Point3d.meters 0 2 0) Direction3d.x


triangle1 =
    Triangle3d.from
        (Point3d.meters 0 0 0)
        (Point3d.meters 1 0 0)
        (Point3d.meters 1 1 0)


triangle2 =
    Triangle3d.from
        (Point3d.meters 0 0 0)
        (Point3d.meters 1 1 0)
        (Point3d.meters 0 1 0)


mesh1 =
    Mesh.triangles [] [ triangle1 ]


mesh2 =
    Mesh.triangles [] [ triangle2 ]
