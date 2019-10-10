module Main exposing (main)

import Browser
import Browser.Events
import Html exposing (Html)
import Html.Attributes
import Math.Vector2 exposing (Vec2)
import Math.Vector3 exposing (Vec3, vec3)
import Task
import WebGL exposing (Mesh, Shader)
import WebGL.Texture exposing (Texture)


type Msg
    = Diff Float
    | GotBitmoji (Result WebGL.Texture.Error Texture)


type alias Model =
    { time : Float
    , bitmoji : Maybe Texture
    }


main : Program () Model Msg
main =
    Browser.element
        { init =
            always
                ( Model 0 Nothing
                , Task.attempt GotBitmoji <| WebGL.Texture.load "/bitmoji.png"
                )
        , view = view
        , subscriptions = subscriptions
        , update = update
        }


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        Diff time ->
            ( { model | time = model.time + time }
            , Cmd.none
            )

        GotBitmoji bitmoji ->
            ( { model | bitmoji = Result.toMaybe bitmoji }, Cmd.none )


subscriptions : Model -> Sub Msg
subscriptions model =
    Browser.Events.onAnimationFrameDelta Diff


view : Model -> Html msg
view model =
    WebGL.toHtml
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
                    { time = model.time / 1000
                    , bitmoji = bitmoji
                    }
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

        vec2 dance(float time, vec2 uv, vec2 point, vec2 offset) {
            const int animSteps = 60;
            const float animDist = 0.003;
            vec2 diff = abs(uv - point);
            vec2 value = vec2(0.);
            for(int i = 0; i < animSteps; i++) {
                if (diff.y < float(i)*animDist) {
                    value.x += offset.x*time;
                }
                if (diff.x < float(i)*animDist) {
                    value.y += offset.y*time;
                }
            }
            return value;
        }

        void main(void) {
            vec2 uv = vFragCoord;
            vec2 img = vec2(uv);

            img += dance(sin(time*4.)/4., uv, vec2(0.5, 0.7), vec2(0., 0.0002));
            img += dance(sin(time*8.),    uv, vec2(0.5, 0.3), vec2(0.0003, 0.));
            img += dance(cos(time*16.),   uv, vec2(0.5, 0.),  vec2(0., 0.0001));

            gl_FragColor = texture2D(bitmoji, img);
        }
    |]
