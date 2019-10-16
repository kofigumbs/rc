port module Main exposing (main)

import Browser
import Browser.Events
import Dance exposing (Dance)
import Html exposing (..)
import Html.Attributes
import Html.Events
import Json.Decode as D
import Math.Vector2 exposing (Vec2, vec2)
import Math.Vector3 exposing (Vec3, vec3)
import Svg exposing (Svg)
import Svg.Attributes exposing (viewBox)
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
    , showAnchors : Bool
    , toCustomization : D.Decoder Dance.Customization
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
        , dance = Dance.lean
        , showAnchors = False
        , toCustomization = D.fail ""
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
    | SetShowAnchors Bool
    | StartCustomization (D.Decoder Dance.Customization)
    | Customize Dance.Customization
    | StopCustomization


update : Msg -> Model -> ( Model, Cmd Msg )
update msg ({ dance } as model) =
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
            load { model | userId = userId, dance = { dance | comicId = comicId } }

        SetDance dance_ ->
            load { model | dance = dance_ }

        SetShowAnchors showAnchors ->
            pure { model | showAnchors = showAnchors }

        StartCustomization toCustomization ->
            pure { model | toCustomization = toCustomization }

        Customize customization ->
            pure { model | dance = Dance.customize customization dance }

        StopCustomization ->
            pure { model | toCustomization = D.fail "" }


pure : a -> ( a, Cmd msg )
pure a =
    ( a, Cmd.none )


port imageDrop : (D.Value -> msg) -> Sub msg


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.batch
        [ imageDrop (NewBitmoji << D.decodeValue parseIds)
        , Browser.Events.onAnimationFrameDelta Diff
        , Browser.Events.onMouseUp (D.succeed StopCustomization)
        ]


view : Model -> Html Msg
view model =
    main_ [ Html.Attributes.style "max-width" (px bitmojiSize) ]
        [ div [ Html.Attributes.style "position" "relative" ]
            [ WebGL.toHtml
                [ Html.Attributes.width bitmojiSize
                , Html.Attributes.height bitmojiSize
                ]
                (viewCanvas model)
            , showIf model.showAnchors <|
                Svg.svg
                    [ Html.Attributes.style "position" "absolute"
                    , Html.Attributes.style "top" "0"
                    , Html.Attributes.style "left" "0"
                    , Html.Attributes.width bitmojiSize
                    , Html.Attributes.height bitmojiSize
                    , Svg.Attributes.viewBox "0 0 1 1"
                    , Html.Events.on "mousemove" (D.map Customize model.toCustomization)
                    ]
                    [ viewAnchor Dance.A
                        model.dance.aTimeMultiplier
                        model.dance.aPhase
                        model.dance.aTarget
                        model.dance.aMovement
                    , viewAnchor Dance.B
                        model.dance.bTimeMultiplier
                        model.dance.bPhase
                        model.dance.bTarget
                        model.dance.bMovement
                    , viewAnchor Dance.C
                        model.dance.cTimeMultiplier
                        model.dance.cPhase
                        model.dance.cTarget
                        model.dance.cMovement
                    , viewAnchor Dance.D
                        model.dance.dTimeMultiplier
                        model.dance.dPhase
                        model.dance.dTarget
                        model.dance.dMovement
                    ]
            ]
        , fieldset []
            [ radio model Dance.lean (text "The Lean")
            , radio model Dance.disco (text "Disco Wave")
            , hr [] []
            , strong [] [ text "Customize" ]
            , showIf model.error <|
                div [ Html.Attributes.class "warning" ]
                    [ b [] [ Html.text "That doesn't seem like a Bitmoji..." ] ]
            , ol []
                [ li []
                    [ text "Install the "
                    , a [ Html.Attributes.href chromeExtensionUrl, Html.Attributes.target "_blank" ]
                        [ text "official Bitmoji Chrome extension" ]
                    ]
                , li [] [ text "Drag-and-drop your Bitmoji here" ]
                ]
            , checkbox SetShowAnchors model.showAnchors (text "Show Anchors")
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


viewAnchor : Dance.Anchor -> Float -> Float -> Vec2 -> Vec2 -> Svg Msg
viewAnchor anchor timeMultiplier phase target movement =
    let
        x =
            Math.Vector2.getX target

        y =
            1 - Math.Vector2.getY target

        width =
            Math.Vector2.getX movement

        height =
            Math.Vector2.getY movement
    in
    Svg.g []
        [ Svg.path
            [ Svg.Attributes.style "cursor:move"
            , Html.Events.onMouseDown <|
                StartCustomization <|
                    D.map (Dance.Target anchor) (D.map2 vec2 offsetX offsetYInverse)
            , Svg.Attributes.d <|
                String.join " " <|
                    List.map (String.join " ") <|
                        -- cross centered at target
                        [ [ "M", String.fromFloat x, String.fromFloat y ]
                        , [ "v", String.fromFloat (-height / 2) ]
                        , [ "v", String.fromFloat height ]
                        , [ "v", String.fromFloat (-height / 2) ]
                        , [ "h", String.fromFloat (-width / 2) ]
                        , [ "h", String.fromFloat width ]
                        ]
            ]
            []
        , Svg.circle
            [ Svg.Attributes.style "cursor:ns-resize"
            , Svg.Attributes.r "0.01"
            , Svg.Attributes.cx <| String.fromFloat x
            , Svg.Attributes.cy <| String.fromFloat (y + height / 2)
            , Html.Events.onMouseDown <|
                StartCustomization <|
                    D.map (Dance.MovementY anchor << distance y) offsetY
            ]
            []
        , Svg.circle
            [ Svg.Attributes.style "cursor:ew-resize"
            , Svg.Attributes.r "0.01"
            , Svg.Attributes.cx <| String.fromFloat (x + width / 2)
            , Svg.Attributes.cy <| String.fromFloat y
            , Html.Events.onMouseDown <|
                StartCustomization <|
                    D.map (Dance.MovementX anchor << distance x) offsetX
            ]
            []
        ]


radio : Model -> Dance { comicId : String } -> Html Msg -> Html Msg
radio model this labelText =
    label []
        [ input
            [ Html.Attributes.name "radio"
            , Html.Attributes.type_ "radio"
            , Html.Attributes.value this.comicId
            , Html.Attributes.checked (this.comicId == model.dance.comicId)
            , onChange this.comicId (SetDance this)
            ]
            []
        , labelText
        ]


checkbox : (Bool -> msg) -> Bool -> Html msg -> Html msg
checkbox toMsg value labelText =
    label []
        [ input
            [ Html.Attributes.type_ "checkbox"
            , Html.Attributes.checked value
            , Html.Events.onCheck toMsg
            ]
            []
        , labelText
        ]


onChange : String -> msg -> Attribute msg
onChange this msg =
    Html.Events.on "change" <|
        D.andThen
            (\x ->
                if x == this then
                    D.succeed msg
                else
                    D.fail ""
            )
            Html.Events.targetValue


px : Float -> String
px value =
    String.fromFloat value ++ "px"


showIf : Bool -> Html msg -> Html msg
showIf check el =
    if check then
        el
    else
        text ""


offsetX : D.Decoder Float
offsetX =
    D.map (\x -> x / bitmojiSize) (D.field "offsetX" D.float)


offsetY : D.Decoder Float
offsetY =
    D.map (\y -> y / bitmojiSize) (D.field "offsetY" D.float)


offsetYInverse : D.Decoder Float
offsetYInverse =
    D.map (\y -> 1 - y) offsetY


distance : Float -> Float -> Float
distance center offset =
    (offset - center) * 2



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

        const int animSteps  = 60;
        const float animDist = 0.003;

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



-- BITMOJI "API"


bitmojiSize : number
bitmojiSize =
    398


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
