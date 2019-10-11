port module Main exposing (main)

import Bitmoji
import Browser
import Browser.Events
import Html exposing (Html)
import Html.Attributes
import Html.Events
import Json.Decode as D
import Math.Vector2 exposing (Vec2)
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
                        { time = model.time / 1000
                        , bitmoji = bitmoji
                        }
                    ]
        , Html.div
            [ Html.Attributes.class "box"
            , Html.Attributes.style "color" "white"
            , Html.Attributes.style "background" "#21cc8c"
            ]
            [ Html.text "Use the "
            , Html.a
                [ Html.Attributes.href Bitmoji.chromeExtensionUrl ]
                [ Html.text "Chrome extension" ]
            , Html.text " to drag-and-drop your Bitmoji here!"
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

        const float pi       = 3.14159265359;
        const float animDist = 0.003;
        const int animSteps  = 60;

        vec2 dance(float time, vec2 uv, vec2 target, vec2 movement) {
            vec2 diff = abs(uv - target);
            vec2 value = vec2(0.);
            for(int step = 0; step < animSteps; step++) {
                if (diff.y < float(step)*animDist) {
                    value.x += movement.x*time;
                }
                if (diff.x < float(step)*animDist) {
                    value.y += movement.y*time;
                }
            }
            return value;
        }

        void main(void) {
            vec2 uv = vFragCoord;
            vec2 img = vec2(uv);

            img += dance(sin(time*4.)/4.,         uv, vec2(0.5, 0.7), vec2(0., 0.0002));
            img += dance(sin(time*8.),            uv, vec2(0.5, 0.3), vec2(0.0003, 0.));
            img += dance(sin(time*16. + pi/2.),   uv, vec2(0.5, 0.),  vec2(0., 0.0001));

            gl_FragColor = texture2D(bitmoji, img);
        }
    |]
