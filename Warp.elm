port module Main exposing (main)

import Bitmoji
import Browser
import Browser.Events
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (onClick)
import Json.Decode as D
import Math.Vector2 exposing (Vec2, vec2)
import Math.Vector3 exposing (Vec3, vec3)
import Task
import WebGL exposing (Mesh, Shader)
import WebGL.Texture exposing (Texture)


type Msg
    = Diff Float
    | GotBitmoji (Result WebGL.Texture.Error Texture)
    | NewBitmoji (Result D.Error String)
    | SetPoseId Bitmoji.PoseId


type alias Model =
    { time : Float
    , error : Bool
    , bitmoji : Maybe Texture
    , options : Bitmoji.Options
    }


main : Program () Model Msg
main =
    Browser.element
        { init = init
        , view = view
        , subscriptions = subscriptions
        , update = update
        }


init : () -> ( Model, Cmd Msg )
init () =
    load
        { time = 0
        , error = False
        , bitmoji = Nothing
        , options = Bitmoji.default
        }


load : Model -> ( Model, Cmd Msg )
load model =
    Bitmoji.url model.options
        |> WebGL.Texture.loadWith WebGL.Texture.nonPowerOfTwoOptions
        |> Task.attempt GotBitmoji
        |> Tuple.pair model


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        Diff time ->
            pure { model | time = model.time + time }

        GotBitmoji (Err _) ->
            pure { model | error = True }

        GotBitmoji (Ok bitmoji) ->
            pure { model | error = False, bitmoji = Just bitmoji }

        NewBitmoji (Err _) ->
            pure { model | error = True }

        NewBitmoji (Ok userId) ->
            load { model | options = { userId = userId, poseId = model.options.poseId } }

        SetPoseId poseId ->
            load { model | options = { userId = model.options.userId, poseId = poseId } }


pure : a -> ( a, Cmd msg )
pure a =
    ( a, Cmd.none )


port imageDrop : (D.Value -> msg) -> Sub msg


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.batch
        [ imageDrop (NewBitmoji << D.decodeValue Bitmoji.parseUserId)
        , Browser.Events.onAnimationFrameDelta Diff
        ]


view : Model -> Html Msg
view model =
    main_
        [ style "display" "flex"
        , style "flex-direction" "column"
        , style "justify-content" "center"
        , style "align-items" "center"
        , style "min-height" "100vh"
        ]
        [ WebGL.toHtml
            [ width 256
            , height 256
            , style "display" "block"
            ]
          <|
            case model.bitmoji of
                Nothing ->
                    []

                Just bitmoji ->
                    [ WebGL.entity vertexShader fragmentShader mesh <|
                        toPose (model.time / 1000) bitmoji model.options.poseId
                    ]
        , div
            [ class "box"
            , style "color" "white"
            , style "background" "#21cc8c"
            ]
            [ p []
                [ text "1. Install the "
                , a
                    [ href Bitmoji.chromeExtensionUrl ]
                    [ text "official Bitmoji Chrome extension" ]
                ]
            , p [] [ text "2. Drag-and-drop your Bitmoji here" ]
            ]
        , div [ class "box" ]
            [ button [ onClick (SetPoseId Bitmoji.Lean) ] [ text "The Lean" ]
            , button [ onClick (SetPoseId Bitmoji.Disco) ] [ text "Disco Wave" ]
            ]
        , if not model.error then
            text ""
          else
            div
                [ class "box"
                , style "color" "red"
                ]
                [ Html.text "That doesn't seem like a Bitmoji..." ]
        ]



-- Mesh


mesh : Mesh { position : Vec3 }
mesh =
    WebGL.triangles
        [ ( { position = vec3 -1 1 0 }
          , { position = vec3 1 1 0 }
          , { position = vec3 -1 -1 0 }
          )
        , ( { position = vec3 -1 -1 0 }
          , { position = vec3 1 1 0 }
          , { position = vec3 1 -1 0 }
          )
        ]



-- Shaders


type alias Uniforms =
    { time : Float
    , bitmoji : Texture
    , aTimeMultiplier : Float
    , aPhase : Float
    , aTarget : Vec2
    , aMovement : Vec2
    , bTimeMultiplier : Float
    , bPhase : Float
    , bTarget : Vec2
    , bMovement : Vec2
    , cTimeMultiplier : Float
    , cPhase : Float
    , cTarget : Vec2
    , cMovement : Vec2
    , dTimeMultiplier : Float
    , dPhase : Float
    , dTarget : Vec2
    , dMovement : Vec2
    }


toPose : Float -> Texture -> Bitmoji.PoseId -> Uniforms
toPose time bitmoji poseId =
    case poseId of
        Bitmoji.Lean ->
            { time = time
            , bitmoji = bitmoji

            -- HEAD
            , aTimeMultiplier = 16
            , aPhase = -pi / 2
            , aTarget = vec2 0.5 0.7
            , aMovement = vec2 0 0.2

            -- HIPS
            , bTimeMultiplier = 8
            , bPhase = 0
            , bTarget = vec2 0.5 0.3
            , bMovement = vec2 0.6 0

            -- FEET
            , cTimeMultiplier = 16
            , cPhase = -pi / 2
            , cTarget = vec2 0.5 0
            , cMovement = vec2 0 0.1

            -- ???
            , dTimeMultiplier = 0
            , dPhase = 0
            , dTarget = vec2 0 0
            , dMovement = vec2 0 0
            }

        Bitmoji.Disco ->
            { time = time
            , bitmoji = bitmoji

            -- LEFT ARM
            , aTimeMultiplier = 8
            , aPhase = 0
            , aTarget = vec2 0.1 0.6
            , aMovement = vec2 0.2 0.3

            -- HIPS
            , bTimeMultiplier = 8
            , bPhase = -pi / 2
            , bTarget = vec2 0.5 0.3
            , bMovement = vec2 0.5 -0.2

            -- RIGHT ARM
            , cTimeMultiplier = 8
            , cPhase = -pi
            , cTarget = vec2 0.7 0.4
            , cMovement = vec2 0.3 0.5

            -- HEAD
            , dTimeMultiplier = 8
            , dPhase = -pi / 2
            , dTarget = vec2 0.45 0.7
            , dMovement = vec2 0.05 -0.15
            }


vertexShader : Shader { position : Vec3 } Uniforms { vFragCoord : Vec2 }
vertexShader =
    [glsl|
        precision mediump float;
        attribute vec3 position;
        varying vec2 vFragCoord;
        void main () {
            gl_Position = vec4(position, 1.0);
            vFragCoord = (position.xy + 1.0) / 2.0;
        }
    |]


fragmentShader : Shader {} Uniforms { vFragCoord : Vec2 }
fragmentShader =
    [glsl|
        precision mediump float;
        varying vec2      vFragCoord;
        uniform float     time;
        uniform sampler2D bitmoji;

        const float pi      = 3.14159265359;
        const int animSteps = 60;
        const vec2 animDist = vec2(0.003, 0.003);

        uniform float aTimeMultiplier; // rate
        uniform float aPhase;          // radians
        uniform vec2  aTarget;         // coordinates [0, 1]
        uniform vec2  aMovement;       // distance in target coordinate system [0, 1]

        uniform float bTimeMultiplier;
        uniform float bPhase;
        uniform vec2  bTarget;
        uniform vec2  bMovement;

        uniform float cTimeMultiplier;
        uniform float cPhase;
        uniform vec2  cTarget;
        uniform vec2  cMovement;

        uniform float dTimeMultiplier;
        uniform float dPhase;
        uniform vec2  dTarget;
        uniform vec2  dMovement;

        vec2 dance(float time, vec2 uv, vec2 target, vec2 movement) {
            vec2 diff = abs(uv - target);
            vec2 value = vec2(0.);
            for(int step = 0; step < animSteps; step++) {
                if (length(diff) < length(float(step)*animDist)) {
                    value += (movement/1000.)*time;
                }
            }
            return value;
        }

        void main(void) {
            vec2 uv = vFragCoord;
            vec2 img = vec2(uv);

            img += dance(sin(time*aTimeMultiplier + aPhase), uv, aTarget, aMovement);
            img += dance(sin(time*bTimeMultiplier + bPhase), uv, bTarget, bMovement);
            img += dance(sin(time*cTimeMultiplier + cPhase), uv, cTarget, cMovement);
            img += dance(sin(time*dTimeMultiplier + dPhase), uv, dTarget, dMovement);

            gl_FragColor = texture2D(bitmoji, img);
        }
    |]
