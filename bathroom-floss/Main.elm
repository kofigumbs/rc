module Main exposing (..)

import Browser
import Browser.Events
import Html
import Html.Attributes
import Svg exposing (..)
import Svg.Attributes exposing (..)
import Time


type alias Model =
    { delta : Float
    , stepDuration : Float
    , forward : Bool
    }


init : Model
init =
    Model 0 450 True


view : Model -> Svg msg
view model =
    let
        spacing =
            0.025

        startX =
            5.0

        startY =
            0.05

        headRadius =
            0.85

        body =
            { limbWidth = 0.85
            , armLength = 2.0
            , legLength = 3.0
            , torsoLength = 3.0
            }

        extremity =
            "a"
                ++ p (body.limbWidth / 2)
                ++ p (body.limbWidth / 2)
                ++ "0 1 1"
                ++ p -body.limbWidth
                ++ "0"

        rightHalf =
            [ H (body.limbWidth + (spacing * 3 / 2))
            , Q body.limbWidth 0 body.limbWidth body.limbWidth
            , L (curve model 0 body.limbWidth) body.armLength
            , Raw extremity
            , L (curve model -spacing -(body.limbWidth + spacing)) -body.armLength
            , V (body.torsoLength + body.legLength)
            , Raw extremity
            , V -body.legLength
            , H -(spacing / 2)
            ]
    in
    Html.main_ []
        [ Html.node "style" [] [ text styles ]
        , svg [ Svg.Attributes.viewBox "0 0 10 10" ]
            [ circle
                [ fill "black"
                , cx (p startX)
                , cy (p (startY + headRadius))
                , r (p headRadius)
                ]
                []
            , Svg.path
                [ d <|
                    "M"
                        ++ p startX
                        ++ p (startY + 2 * headRadius + spacing)
                        ++ List.foldr ((++) << dString) "" rightHalf
                        ++ List.foldl ((++) << dString << reverseY) "" rightHalf
                ]
                []
            ]
        ]


type D
    = H Float
    | V Float
    | L Float Float
    | Q Float Float Float Float
    | Raw String


reverseY : D -> D
reverseY d =
    case d of
        V y ->
            V -y

        L x y ->
            L x -y

        Q x0 y0 x1 y1 ->
            Q y0 -x0 x1 -y1

        _ ->
            d


dString : D -> String
dString d =
    let
        value =
            case d of
                H x ->
                    "h" ++ p x

                V y ->
                    "v" ++ p y

                L x y ->
                    "l" ++ p x ++ p y

                Q x0 y0 x1 y1 ->
                    "q" ++ p x0 ++ p y0 ++ p x1 ++ p y1

                Raw string ->
                    string
    in
    " " ++ value ++ " "


curve : Model -> Float -> Float -> Float
curve model from to =
    let
        multiplier =
            clamp 0 1 (model.delta / model.stepDuration)

        dx =
            if model.forward then
                multiplier
            else
                1 - multiplier
    in
    from + (dx * (to - from))


p : Float -> String
p float =
    " " ++ String.fromFloat float ++ " "


styles : String
styles =
    """
    html, body {
        margin:0;
        padding:0;
    }
    main {
        height:100vh;
        width:100%;
        display:flex;
        display:flex;
        flex-direction:column;
    }
    svg {
        flex: 1;
    }
    * {
        transform-style: preserve-3d;
    }
    """


type Msg
    = NewAnimationFrameDelta Float
    | Next


update : Msg -> Model -> Model
update msg model =
    case msg of
        NewAnimationFrameDelta value ->
            { model | delta = value + model.delta }

        Next ->
            { model | delta = 0, forward = not model.forward }



-- PROGRAM


subscriptions model =
    Sub.batch
        [ Time.every model.stepDuration (\_ -> Next)
        , Browser.Events.onAnimationFrameDelta NewAnimationFrameDelta
        ]


main =
    Browser.element
        { init = \() -> ( init, Cmd.none )
        , update = \msg model -> ( update msg model, Cmd.none )
        , subscriptions = subscriptions
        , view = view
        }
