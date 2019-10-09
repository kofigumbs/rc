module Main exposing (..)

import Browser
import Browser.Events
import Html exposing (Html)
import Html.Attributes
import Html.Events
import Time


type alias Model =
    { delta : Float
    }


init : () -> ( Model, Cmd Msg )
init () =
    ( { delta = 0
      }
    , Cmd.none
    )


view : Model -> Html Msg
view model =
    Html.div [] [ Html.text "TODO" ]


type Msg
    = NewAnimationFrameDelta Float


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        NewAnimationFrameDelta value ->
            ( { model | delta = value + model.delta }, Cmd.none )



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
