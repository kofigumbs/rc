module Vizualizer exposing (main)

import Angle exposing (Angle)
import Axis3d
import Browser
import Browser.Events
import Camera3d
import Color
import Dict exposing (Dict)
import Direction3d
import Html exposing (Html)
import Json.Decode as D
import Length
import Parameter1d
import Pixels
import Point3d
import Quantity
import Scene3d
import Scene3d.Drawable as Drawable
import Scene3d.Mesh as Mesh
import Triangle3d
import Viewpoint3d


main : Program () Model Msg
main =
    Browser.element
        { init = init
        , update = update
        , subscriptions = subscriptions
        , view = view
        }


type alias Model =
    { activeKeys : Dict Int Envelope
    }


type Envelope
    = Sustained
    | Releasing Float


init : () -> ( Model, Cmd Msg )
init () =
    ( { activeKeys = Dict.empty }, Cmd.none )


type Msg
    = Diff Float
    | GotKeyDown Int
    | GotKeyUp Int


upper =
    [ 81, 50, 87, 51, 69, 82, 53, 84, 54, 89, 55, 85, 73, 57, 79, 48, 80, 219, 187, 221 ]


indexOf : a -> List a -> Int
indexOf value =
    let
        recurse index list =
            case list of
                [] ->
                    -1

                first :: rest ->
                    if first == value then
                        index
                    else
                        recurse (index + 1) rest
    in
    recurse 0


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        Diff time ->
            ( { model
                | activeKeys =
                    Dict.map
                        (\k v ->
                            case v of
                                Sustained ->
                                    Sustained

                                Releasing t ->
                                    Releasing (max 0 (t - time))
                        )
                        model.activeKeys
              }
            , Cmd.none
            )

        GotKeyDown code ->
            ( { model | activeKeys = Dict.insert code Sustained model.activeKeys }, Cmd.none )

        GotKeyUp code ->
            ( { model | activeKeys = Dict.insert code (Releasing 750) model.activeKeys }, Cmd.none )


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.batch
        [ Browser.Events.onKeyDown (D.map GotKeyDown (D.field "which" D.int))
        , Browser.Events.onKeyUp (D.map GotKeyUp (D.field "which" D.int))
        , Browser.Events.onAnimationFrameDelta Diff
        ]


view : Model -> Html Msg
view model =
    let
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

        -- z = left right
        -- y = up down
        -- x = in out
        viewpoint =
            Viewpoint3d.lookAt
                { focalPoint = Point3d.meters 0 2 0
                , eyePoint = Point3d.meters 10 5 3
                , upDirection = Direction3d.y
                }

        camera =
            Camera3d.perspective
                { viewpoint = viewpoint
                , verticalFieldOfView = Angle.degrees 30
                , clipDepth = Length.meters 0.1
                }

        square alpha =
            Drawable.group
                [ Drawable.colored (Color.hsl 0 1 (1 - alpha / 2)) mesh1
                , Drawable.colored (Color.hsl 0.94 1 (1 - alpha / 2)) mesh2
                ]

        rotationAxis =
            Axis3d.through (Point3d.meters 0 2 0) Direction3d.x

        angles =
            Parameter1d.leading 12 <|
                Quantity.interpolateFrom
                    (Angle.degrees 0)
                    (Angle.degrees 360)

        rotatedSquare activeKeys index angle =
            case List.filter (\( key, value ) -> modBy 12 (indexOf key upper) == index) (Dict.toList activeKeys) of
                [] ->
                    Drawable.empty

                ( _, Sustained ) :: _ ->
                    square 1 |> Drawable.rotateAround rotationAxis angle

                ( _, Releasing env ) :: _ ->
                    square (env / 750) |> Drawable.rotateAround rotationAxis angle
    in
    Scene3d.unlit []
        { camera = camera
        , width = Pixels.pixels 800
        , height = Pixels.pixels 600
        }
        (List.indexedMap (rotatedSquare model.activeKeys) angles)
