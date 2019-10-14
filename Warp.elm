port module Main exposing (main)

import Browser
import Browser.Events
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (on, onClick)
import Json.Decode as D
import Math.Vector2 exposing (Vec2, vec2)
import Math.Vector3 exposing (Vec3, vec3)
import Task
import WebGL exposing (Mesh, Shader)
import WebGL.Texture exposing (Texture)


-- ELM ARCHITECTURE


type alias Model =
    { time : Float
    , error : Bool
    , bitmoji : Maybe Texture
    , userId : String
    , dance : Dance { comicId : String }
    , custom : Dance { comicId : String }
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
    let
        kofi =
            "4b014b97-f9a9-480e-8e7f-3c74def6e9f6"
    in
    load
        { time = 0
        , error = False
        , bitmoji = Nothing
        , userId = kofi
        , dance = lean
        , custom = customDefaults
        }


load : Model -> ( Model, Cmd Msg )
load model =
    let
        url =
            baseUrl
                ++ model.dance.comicId
                ++ "-"
                ++ model.userId
                ++ "-v1.png?transparent=1&palette=1"
    in
    WebGL.Texture.loadWith WebGL.Texture.nonPowerOfTwoOptions url
        |> Task.attempt GotBitmoji
        |> Tuple.pair model


type Msg
    = Diff Float
    | GotBitmoji (Result WebGL.Texture.Error Texture)
    | NewBitmoji (Result D.Error ( String, String ))
    | SetDance (Dance { comicId : String })


update : Msg -> Model -> ( Model, Cmd Msg )
update msg ({ custom } as model) =
    case msg of
        Diff time ->
            pure { model | time = model.time + time }

        GotBitmoji (Err _) ->
            pure { model | error = True }

        GotBitmoji (Ok bitmoji) ->
            pure { model | error = False, bitmoji = Just bitmoji }

        NewBitmoji (Err _) ->
            pure { model | error = True }

        NewBitmoji (Ok ( userId, comicId )) ->
            load { model | userId = userId, custom = { custom | comicId = comicId } }

        SetDance dance ->
            load { model | dance = dance }


pure : a -> ( a, Cmd msg )
pure a =
    ( a, Cmd.none )


port imageDrop : (D.Value -> msg) -> Sub msg


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.batch
        [ imageDrop (NewBitmoji << D.decodeValue parseIds)
        , Browser.Events.onAnimationFrameDelta Diff
        ]


view : Model -> Html Msg
view model =
    main_ []
        [ WebGL.toHtml [ width 398, height 398 ] (viewCanvas model)
        , div
            [ class "box warning"
            , classList [ ( "hide", not model.error ) ]
            ]
            [ Html.text "That doesn't seem like a Bitmoji..." ]
        , div
            [ class "box instructions" ]
            [ p []
                [ text "1. Install the "
                , a [ href chromeExtensionUrl, target "_blank" ]
                    [ text "official Bitmoji Chrome extension" ]
                ]
            , p [] [ text "2. Drag-and-drop your Bitmoji here" ]
            ]
        , div [ class "box" ] <|
            let
                radio this disabledValue labelText =
                    label []
                        [ input
                            [ name "radio"
                            , type_ "radio"
                            , value this.comicId
                            , checked (this.comicId == model.dance.comicId)
                            , onChange this.comicId (SetDance this)
                            , disabled disabledValue
                            ]
                            []
                        , labelText
                        ]
            in
            [ radio lean False (text "The Lean")
            , radio disco False (text "Disco Wave")
            , radio model.custom
                (String.isEmpty model.custom.comicId)
                (code [ style "font-family" "monospace" ] [ text "Custom" ])
            ]
        ]


viewCanvas : Model -> List WebGL.Entity
viewCanvas model =
    case model.bitmoji of
        Nothing ->
            []

        Just bitmoji ->
            [ WebGL.entity vertexShader fragmentShader mesh <|
                { time = model.time / 1000
                , bitmoji = bitmoji
                , aTimeMultiplier = model.dance.aTimeMultiplier
                , aPhase = model.dance.aPhase
                , aTarget = model.dance.aTarget
                , aMovement = model.dance.aMovement
                , bTimeMultiplier = model.dance.bTimeMultiplier
                , bPhase = model.dance.bPhase
                , bTarget = model.dance.bTarget
                , bMovement = model.dance.bMovement
                , cTimeMultiplier = model.dance.cTimeMultiplier
                , cPhase = model.dance.cPhase
                , cTarget = model.dance.cTarget
                , cMovement = model.dance.cMovement
                , dTimeMultiplier = model.dance.dTimeMultiplier
                , dPhase = model.dance.dPhase
                , dTarget = model.dance.dTarget
                , dMovement = model.dance.dMovement
                }
            ]


onChange : String -> msg -> Attribute msg
onChange this msg =
    on "change" <|
        D.andThen
            (\x ->
                if x == this then
                    D.succeed msg
                else
                    D.fail ""
            )
            Html.Events.targetValue



-- MESH


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



-- SHADERS


type alias Uniforms =
    Dance { time : Float, bitmoji : Texture }


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

        uniform float aTimeMultiplier;
        uniform float aPhase;
        uniform vec2  aTarget;
        uniform vec2  aMovement;

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



-- ANIMATION
--
-- Choose which 4 points (a, b, c, d) to animate in the Bitmoji image!
--
--   Multiplier: rate of movement. Bigger means faster.
--   Phase: radians by which to offset movement. Bigger means more delayed.
--   Target: coordinates [0, 1]. Point to target.
--   Movement: distance in target coordinate system [0, 1]. How far to animate.


type alias Dance a =
    { a
        | aTimeMultiplier : Float
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


lean : Dance { comicId : String }
lean =
    { comicId = "49490f4e-eabb-4cab-bcb6-69f361d66706"

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


disco : Dance { comicId : String }
disco =
    { comicId = "5ee3832d-7743-43c8-b6d7-ea47f11a1798"

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


customDefaults : Dance { comicId : String }
customDefaults =
    { comicId = ""
    , aTimeMultiplier = 1
    , aPhase = 0
    , aTarget = vec2 0 0
    , aMovement = vec2 0.1 0.1
    , bTimeMultiplier = 1
    , bPhase = 0
    , bTarget = vec2 0 1
    , bMovement = vec2 0.1 0.1
    , cTimeMultiplier = 1
    , cPhase = 0
    , cTarget = vec2 1 0
    , cMovement = vec2 0.1 0.1
    , dTimeMultiplier = 1
    , dPhase = 0
    , dTarget = vec2 1 1
    , dMovement = vec2 0.1 0.1
    }



-- BITMOJI "API"


parseIds : D.Decoder ( String, String )
parseIds =
    D.andThen parseIds_ D.string


parseIds_ : String -> D.Decoder ( String, String )
parseIds_ raw =
    if not <| String.startsWith baseUrl raw then
        D.fail ""
    else
        case dropUntilUserId (String.split "-" raw) of
            Nothing ->
                D.fail ""

            Just userId ->
                String.dropLeft (String.length baseUrl) raw
                    |> String.split ("-" ++ userId)
                    |> List.head
                    |> Maybe.map (D.succeed << Tuple.pair userId)
                    |> Maybe.withDefault (D.fail "")


dropUntilUserId : List String -> Maybe String
dropUntilUserId segments =
    case segments of
        [] ->
            Nothing

        [ a, b, c, d, e, _ ] ->
            Just (String.join "-" [ a, b, c, d, e ])

        _ :: rest ->
            dropUntilUserId rest


baseUrl : String
baseUrl =
    "https://render.bitstrips.com/v2/cpanel/"


chromeExtensionUrl : String
chromeExtensionUrl =
    "https://chrome.google.com/webstore/detail/bitmoji/bfgdeiadkckfbkeigkoncpdieiiefpig?hl=en"
