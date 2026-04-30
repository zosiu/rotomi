module Main exposing (Model, Msg(..), init, main, update)

import Browser
import Html exposing (Html, button, div, h1, p, text)
import Html.Attributes exposing (class)
import Html.Events exposing (onClick)


main : Program () Model Msg
main =
    Browser.sandbox
        { init = init
        , update = update
        , view = view
        }



-- MODEL


type alias Model =
    { count : Int
    }


init : Model
init =
    { count = 0 }



-- UPDATE


type Msg
    = Increment
    | Decrement
    | Reset


update : Msg -> Model -> Model
update msg model =
    case msg of
        Increment ->
            { model | count = model.count + 1 }

        Decrement ->
            { model | count = model.count - 1 }

        Reset ->
            init



-- VIEW


view : Model -> Html Msg
view model =
    div [ class "app" ]
        [ h1 [] [ text "Rotomi" ]
        , p [] [ text ("Count: " ++ String.fromInt model.count) ]
        , div [ class "controls" ]
            [ button [ onClick Decrement ] [ text "-" ]
            , button [ onClick Reset ] [ text "Reset" ]
            , button [ onClick Increment ] [ text "+" ]
            ]
        ]
