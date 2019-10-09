module Main exposing (..)

import Browser
import Browser.Events
import Html exposing (Html)
import Html.Attributes exposing (height, width)
import Html.Events
import Math.Vector3 exposing (Vec3, vec3)
import Task
import Time
import WebGL
import WebGL.Texture exposing (Texture)


vertexShader : WebGL.Shader { a | position : Vec3 } u {}
vertexShader =
    [glsl|
attribute vec3 position;
void main () {
    gl_Position = vec4(position, 1.0);
}
|]


fragmentShader : WebGL.Shader {} { u | time : Float, bitmoji : Texture } {}
fragmentShader =
    [glsl|
uniform float time;
uniform sampler2D bitmoji;

in VertexData
{
    vec4 v_position;
    vec3 v_normal;
    vec2 v_texcoord;
} inData;

out vec4 fragColor;

vec2 dance(float time, vec2 uv, vec2 point, vec2 offset) {
    int animSteps = 60;
    float animDist = 0.003;
    vec2 diff = abs(uv - point);
    vec2 value = vec2(0.);
    for(int i = 0; i < animSteps; i++) {
        if (diff.y < i*animDist) {
            value.x += offset.x*time;
        }
        if (diff.x < i*animDist) {
            value.y += offset.y*time;
        }
    }
    return value;
}

void main(void) {
    vec2 uv = inData.v_texcoord;
    vec2 img = vec2(uv.x, 1-uv.y);

    img += dance(sin(time*4)/4, uv, vec2(0.5, 0.7), vec2(0., 0.0002));
    img += dance(sin(time*8),   uv, vec2(0.5, 0.3), vec2(0.0003, 0.));
    img += dance(cos(time*16),  uv, vec2(0.5, 0.),  vec2(0., 0.0001));

    fragColor = texture(bitmoji, img);
}
|]


mesh : WebGL.Mesh { position : Vec3 }
mesh =
    let
        topLeft =
            { position = vec3 -1 1 1 }

        topRight =
            { position = vec3 1 1 1 }

        bottomLeft =
            { position = vec3 -1 -1 1 }

        bottomRight =
            { position = vec3 1 -1 1 }
    in
    WebGL.triangles
        [ ( topLeft, topRight, bottomLeft )
        , ( bottomLeft, topRight, bottomRight )
        ]


type alias Model =
    { time : Float
    , bitmoji : Maybe Texture
    }


init : () -> ( Model, Cmd Msg )
init () =
    ( { time = 0
      , bitmoji = Nothing
      }
    , WebGL.Texture.loadWith WebGL.Texture.nonPowerOfTwoOptions "/bitmoji.png"
        |> Task.attempt GotBitmoji
    )


view : Model -> Html Msg
view model =
    case model.bitmoji of
        Nothing ->
            Html.div [] [ Html.text "Loading …" ]

        Just bitmoji ->
            WebGL.toHtml [ width 398, height 398 ]
                [ WebGL.entity vertexShader fragmentShader mesh <|
                    { time = model.time, bitmoji = bitmoji }
                ]


type Msg
    = NewAnimationFrameDelta Float
    | GotBitmoji (Result WebGL.Texture.Error Texture)


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        NewAnimationFrameDelta value ->
            ( { model | time = value + model.time }, Cmd.none )

        GotBitmoji result ->
            ( { model | bitmoji = Result.toMaybe result }, Cmd.none )



-- PROGRAM


subscriptions model =
    Browser.Events.onAnimationFrameDelta NewAnimationFrameDelta


main =
    Browser.element
        { init = init
        , update = update
        , subscriptions = subscriptions
        , view = view
        }
