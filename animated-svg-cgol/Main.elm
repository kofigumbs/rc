module Main exposing (..)

import Browser
import Browser.Events
import Curve
import Glyph
import Html
import Html.Attributes
import Html.Events
import Set exposing (Set)
import Svg exposing (..)
import Svg.Attributes exposing (..)
import Time


blinker : List Cell
blinker =
    parse [ "OOO" ]


smallSpaceship : List Cell
smallSpaceship =
    parse
        [ "OOO"
        , ".O."
        , "..O"
        ]


middleSpaceship : List Cell
middleSpaceship =
    parse
        [ "..O"
        , "O...O"
        , ".....O"
        , "O....O"
        , ".OOOOO"
        ]


gun : List Cell
gun =
    parse
        [ "........................O"
        , "......................O.O"
        , "............OO......OO............OO"
        , "...........O...O....OO............OO"
        , "OO........O.....O...OO"
        , "OO........O...O.OO....O.O"
        , "..........O.....O.......O"
        , "...........O...O"
        , "............OO"
        ]


parse : List String -> List Cell
parse raw =
    let
        middle =
            List.length raw // 2

        dotIsDead x y char =
            if char == '.' then
                Nothing
            else
                Just ( -middle + y, x )

        unRaw y line =
            String.toList line
                |> List.indexedMap (\x char -> dotIsDead x y char)
                |> List.filterMap identity
    in
    List.concat (List.indexedMap unRaw raw)


type alias Model =
    { previous : World
    , current : World
    , delta : Float
    , stepDuration : Float
    , shape : Glyph.Shape
    }


type alias World =
    Set Cell


type alias Cell =
    ( Int, Int )


init : Model
init =
    { previous = Set.empty
    , current = Set.fromList middleSpaceship
    , delta = 0
    , stepDuration = 450
    , shape = Glyph.Dot
    }


view : Model -> Svg Msg
view model =
    let
        ( ( oldFirstX, oldFirstY ), ( oldLastX, oldLastY ) ) =
            northwestSourtheastCorners 5 model.previous

        ( ( newFirstX, newFirstY ), ( newLastX, newLastY ) ) =
            northwestSourtheastCorners 5 model.current
    in
    Html.main_ []
        [ Html.input
            [ Html.Attributes.type_ "range"
            , Html.Attributes.min "16"
            , Html.Attributes.max "1000"
            , Html.Attributes.style "direction" "rtl"
            , Html.Events.onInput SetStepDuration
            , Html.Attributes.value (String.fromFloat model.stepDuration)
            ]
            []
        , Html.div []
            [ Html.button [ Html.Events.onClick (SetShape Glyph.Dot) ] [ text "Dot" ]
            , Html.button [ Html.Events.onClick (SetShape Glyph.Star) ] [ text "Star" ]
            , Html.button [ Html.Events.onClick (SetShape Glyph.Box) ] [ text "Box" ]
            ]
        , Html.div []
            [ Html.button [ Html.Events.onClick (SetPattern blinker) ] [ text "Blinker" ]
            , Html.button [ Html.Events.onClick (SetPattern gun) ] [ text "Gun" ]
            , Html.button [ Html.Events.onClick (SetPattern middleSpaceship) ] [ text "Middle Spaceship" ]
            , Html.button [ Html.Events.onClick (SetPattern smallSpaceship) ] [ text "Small Spaceship?" ]
            ]
        , svg
            [ viewBox
                (cubicBezier model oldFirstX newFirstX)
                (cubicBezier model oldFirstY newFirstY)
                (cubicBezier model (oldLastX - oldFirstX) (newLastX - newFirstX))
                (cubicBezier model (oldLastY - oldFirstY) (newLastY - newFirstY))
            ]
            (grid (space model) (List.range newFirstX newLastX) (List.range newFirstY newLastY))
        ]


space : Model -> Cell -> Maybe (Svg msg)
space model cell =
    let
        nowAlive =
            alive model.current cell

        wasAlive =
            alive model.previous cell
    in
    if nowAlive && wasAlive {- STAYING ALIIIIVE -} then
        Just <| Glyph.view model.shape 1 cell
    else if nowAlive {- REVIVING -} then
        Just <| Glyph.view model.shape (cubicBezier model 0 1) cell
    else if wasAlive {- DYING -} then
        Just <| Glyph.view model.shape (cubicBezier model 1 0) cell
    else
        Nothing


type Msg
    = NewAnimationFrameDelta Float
    | SetPattern (List Cell)
    | SetShape Glyph.Shape
    | SetStepDuration String
    | Next


update : Msg -> Model -> Model
update msg model =
    case msg of
        NewAnimationFrameDelta value ->
            { model | delta = value + model.delta }

        SetPattern cells ->
            { model | previous = Set.empty, current = Set.fromList cells }

        SetShape shape ->
            { model | shape = shape }

        SetStepDuration raw ->
            case String.toFloat raw of
                Nothing ->
                    model

                Just value ->
                    { model | stepDuration = value }

        Next ->
            let
                ( ( firstX, firstY ), ( lastX, lastY ) ) =
                    northwestSourtheastCorners 1 model.current
            in
            { current =
                Set.fromList <|
                    grid (next model.current)
                        (List.range firstX lastX)
                        (List.range firstY lastY)
            , previous = model.current
            , delta = 0
            , stepDuration = model.stepDuration
            , shape = model.shape
            }


next : World -> Cell -> Maybe Cell
next world cell =
    let
        count =
            List.length <| List.filter (alive world) (neighbors cell)
    in
    if count == 3 || count == 2 && alive world cell then
        Just cell
    else
        Nothing


alive : World -> Cell -> Bool
alive world cell =
    Set.member cell world


neighbors : Cell -> List Cell
neighbors ( x, y ) =
    -- ABOVE
    [ ( x - 1, y - 1 )
    , ( x + 0, y - 1 )
    , ( x + 1, y - 1 )

    -- BESIDE
    , ( x - 1, y + 0 )
    , ( x + 1, y + 0 )

    -- BELOW
    , ( x - 1, y + 1 )
    , ( x + 0, y + 1 )
    , ( x + 1, y + 1 )
    ]



-- BOX STUFF


northwestSourtheastCorners : Int -> World -> ( Cell, Cell )
northwestSourtheastCorners padding world =
    let
        ( xs, ys ) =
            Set.foldl (\( x, y ) -> Tuple.mapBoth ((::) x) ((::) y)) ( [], [] ) world

        zeroOr =
            Maybe.withDefault 0
    in
    ( ( zeroOr (List.minimum xs) - padding, zeroOr (List.minimum ys) - padding )
    , ( zeroOr (List.maximum xs) + padding, zeroOr (List.maximum ys) + padding )
    )


cubicBezier : Model -> Int -> Int -> Float
cubicBezier model from to =
    Curve.cubicBezier (clamp 0 1 (model.delta / model.stepDuration)) (toFloat from) (toFloat to)


grid : (Cell -> Maybe a) -> List Int -> List Int -> List a
grid f rangeX rangeY =
    List.concatMap (\x -> List.filterMap (\y -> f ( x, y )) rangeY) rangeX


viewBox : Float -> Float -> Float -> Float -> Svg.Attribute msg
viewBox x1 x2 x3 x4 =
    Svg.Attributes.viewBox <|
        String.fromFloat x1
            ++ " "
            ++ String.fromFloat x2
            ++ " "
            ++ String.fromFloat x3
            ++ " "
            ++ String.fromFloat x4


last : List a -> Maybe a
last list =
    case list of
        [] ->
            Nothing

        _ :: x :: [] ->
            Just x

        _ :: rest ->
            last rest



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
