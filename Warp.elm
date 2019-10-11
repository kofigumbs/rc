port module Main exposing (main)

import Bitmoji
import Browser
import Browser.Events
import Html exposing (Html)
import Html.Attributes
import Html.Events
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
    Html.main_
        [ Html.Attributes.style "display" "flex"
        , Html.Attributes.style "flex-direction" "column"
        , Html.Attributes.style "justify-content" "center"
        , Html.Attributes.style "align-items" "center"
        , Html.Attributes.style "min-height" "100vh"
        ]
        [ WebGL.toHtml
            [ Html.Attributes.width 256
            , Html.Attributes.height 256
            , Html.Attributes.style "display" "block"
            ]
          <|
            case model.bitmoji of
                Nothing ->
                    []

                Just bitmoji ->
                    [ WebGL.entity vertexShader fragmentShader mesh <|
                        toPose (model.time / 1000) bitmoji model.options.poseId
                    ]
        , Html.div
            [ Html.Attributes.class "box"
            , Html.Attributes.style "color" "white"
            , Html.Attributes.style "background" "#21cc8c"
            ]
            [ Html.ol []
                [ Html.li []
                    [ Html.text "Download the "
                    , Html.a
                        [ Html.Attributes.href Bitmoji.chromeExtensionUrl ]
                        [ Html.text "official Bitmoji Chrome extension" ]
                    ]
                , Html.li [] [ Html.text "Drag-and-drop your Bitmoji here" ]
                ]
            ]
        , Html.div
            [ Html.Attributes.class "box"
            , Html.Attributes.style "color" "red"
            ]
            [ if model.error then
                Html.text "That doesn't seem like a Bitmoji..."
              else
                Html.text ""
            ]
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

    --
    , aTimeMultiplier : Float
    , aPhase : Float
    , aTarget : Vec2
    , aMovement : Vec2

    --
    , bTimeMultiplier : Float
    , bPhase : Float
    , bTarget : Vec2
    , bMovement : Vec2

    --
    , cTimeMultiplier : Float
    , cPhase : Float
    , cTarget : Vec2
    , cMovement : Vec2
    }


toPose : Float -> Texture -> Bitmoji.PoseId -> Uniforms
toPose time bitmoji poseId =
    case poseId of
        Bitmoji.Standing ->
            { time = time
            , bitmoji = bitmoji
            , aTimeMultiplier = 16
            , aPhase = -pi / 2
            , aTarget = vec2 0.5 0.7
            , aMovement = vec2 0 0.0002
            , bTimeMultiplier = 8
            , bPhase = 0
            , bTarget = vec2 0.5 0.3
            , bMovement = vec2 0.0006 0
            , cTimeMultiplier = 16
            , cPhase = -pi / 2
            , cTarget = vec2 0.5 0
            , cMovement = vec2 0 0.0001
            }

        Bitmoji.Pointing ->
            { time = time
            , bitmoji = bitmoji
            , aTimeMultiplier = 16
            , aPhase = 0
            , aTarget = vec2 0.35 0.6
            , aMovement = vec2 0 0
            , bTimeMultiplier = 8
            , bPhase = 0
            , bTarget = vec2 0.5 0.3
            , bMovement = vec2 0.0003 0
            , cTimeMultiplier = 16
            , cPhase = pi / 2
            , cTarget = vec2 0.5 0
            , cMovement = vec2 0 0.0001
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

        uniform float bTimeMultiplier; // rate
        uniform float bPhase;          // radians
        uniform vec2  bTarget;         // coordinates [0, 1]
        uniform vec2  bMovement;       // distance in target coordinate system [0, 1]

        uniform float cTimeMultiplier; // rate
        uniform float cPhase;          // radians
        uniform vec2  cTarget;         // coordinates [0, 1]
        uniform vec2  cMovement;       // distance in target coordinate system [0, 1]

        vec2 dance(float time, vec2 uv, vec2 target, vec2 movement) {
            vec2 diff = abs(uv - target);
            vec2 value = vec2(0.);
            for(int step = 0; step < animSteps; step++) {
                if (length(diff) < length(float(step)*animDist)) {
                    value += movement*time;
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

            gl_FragColor = texture2D(bitmoji, img);
        }
    |]
