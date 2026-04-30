port module Main exposing (Model(..), Msg(..), init, main, update)

import Browser
import Html exposing (Html, button, div, h1, input, span, text)
import Html.Attributes exposing (placeholder, style, type_, value)
import Html.Events exposing (onClick, onInput)
import Http
import Json.Decode as Decode
import Replay
import Url


port pushUrl : String -> Cmd msg


init : String -> ( Model, Cmd Msg )
init flags =
    let
        url =
            String.trim flags
    in
    if String.isEmpty url then
        ( EnteringUrl "", Cmd.none )

    else
        ( Loading url
        , Http.get { url = url, expect = Http.expectString GotReplay }
        )


main : Program String Model Msg
main =
    Browser.element
        { init = init
        , update = update
        , view = view
        , subscriptions = \_ -> Sub.none
        }



-- MODEL


type Model
    = EnteringUrl String
    | Loading String
    | Retrying String
    | Loaded String Replay.Replay
    | Failed String String


currentUrl : Model -> String
currentUrl model =
    case model of
        EnteringUrl url ->
            url

        Loading url ->
            url

        Retrying url ->
            url

        Loaded url _ ->
            url

        Failed url _ ->
            url



-- UPDATE


type Msg
    = UrlChanged String
    | LoadClicked
    | GotReplay (Result Http.Error String)


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        UrlChanged url ->
            ( EnteringUrl url, Cmd.none )

        LoadClicked ->
            let
                url =
                    currentUrl model |> String.trim
            in
            if String.isEmpty url then
                ( model, Cmd.none )

            else
                ( Loading url
                , Http.get { url = url, expect = Http.expectString GotReplay }
                )

        GotReplay result ->
            case model of
                Loading url ->
                    case result of
                        Ok content ->
                            loadReplay url content

                        Err Http.NetworkError ->
                            ( Retrying url
                            , Http.get { url = proxyUrl url, expect = Http.expectString GotReplay }
                            )

                        Err err ->
                            ( Failed url (httpErrorToString err), Cmd.none )

                Retrying url ->
                    case result of
                        Ok content ->
                            loadReplay url content

                        Err err ->
                            ( Failed url (httpErrorToString err), Cmd.none )

                _ ->
                    ( model, Cmd.none )


loadReplay : String -> String -> ( Model, Cmd Msg )
loadReplay url content =
    let
        replay =
            Replay.parse content
    in
    if List.isEmpty replay.sections then
        ( Failed url "No replay content found — check the URL", Cmd.none )

    else
        ( Loaded url replay, pushUrl url )


proxyUrl : String -> String
proxyUrl url =
    "https://api.allorigins.win/raw?url=" ++ Url.percentEncode url


httpErrorToString : Http.Error -> String
httpErrorToString err =
    case err of
        Http.BadUrl _ ->
            "Invalid URL"

        Http.Timeout ->
            "Request timed out"

        Http.NetworkError ->
            "Network error — check your connection"

        Http.BadStatus 404 ->
            "No replay content found — check the URL"

        Http.BadStatus status ->
            "Server error (" ++ String.fromInt status ++ ")"

        Http.BadBody _ ->
            "Unexpected response format"



-- VIEW


view : Model -> Html Msg
view model =
    div
        [ style "font-family" "system-ui, -apple-system, sans-serif"
        , style "max-width" "820px"
        , style "margin" "0 auto"
        , style "padding" "2rem 1rem"
        , style "color" "#1a202c"
        ]
        [ h1
            [ style "font-size" "1.75rem"
            , style "font-weight" "700"
            , style "margin" "0 0 1.5rem"
            , style "color" "#2d3748"
            ]
            [ text "Rotomi" ]
        , viewUrlBar model
        , viewContent model
        ]


viewUrlBar : Model -> Html Msg
viewUrlBar model =
    let
        url =
            currentUrl model

        isLoading =
            case model of
                Loading _ ->
                    True

                Retrying _ ->
                    True

                _ ->
                    False

        hasError =
            case model of
                Failed _ _ ->
                    True

                _ ->
                    False
    in
    div [ style "margin-bottom" "2rem" ]
        [ div
            [ style "display" "flex"
            , style "gap" "0.5rem"
            ]
            [ input
                [ type_ "url"
                , placeholder "Paste a raw replay file URL…"
                , value url
                , onInput UrlChanged
                , onEnter LoadClicked
                , style "flex" "1"
                , style "min-width" "0"
                , style "padding" "0.6rem 0.75rem"
                , style "font-size" "0.95rem"
                , style "border"
                    (if hasError then
                        "1px solid #fc8181"

                     else
                        "1px solid #cbd5e0"
                    )
                , style "border-radius" "6px"
                , style "outline" "none"
                ]
                []
            , button
                [ onClick LoadClicked
                , style "padding" "0.6rem 1.5rem"
                , style "background"
                    (if isLoading then
                        "#a0aec0"

                     else
                        "#4a5568"
                    )
                , style "color" "white"
                , style "border" "none"
                , style "border-radius" "6px"
                , style "cursor"
                    (if isLoading then
                        "default"

                     else
                        "pointer"
                    )
                , style "font-size" "0.95rem"
                , style "font-weight" "600"
                , style "white-space" "nowrap"
                ]
                [ text
                    (if isLoading then
                        "Loading…"

                     else
                        "Load"
                    )
                ]
            ]
        , case model of
            Failed _ err ->
                div
                    [ style "margin-top" "0.5rem"
                    , style "color" "#e53e3e"
                    , style "font-size" "0.875rem"
                    ]
                    [ text err ]

            _ ->
                text ""
        ]


onEnter : msg -> Html.Attribute msg
onEnter msg =
    Html.Events.on "keydown"
        (Decode.field "key" Decode.string
            |> Decode.andThen
                (\key ->
                    if key == "Enter" then
                        Decode.succeed msg

                    else
                        Decode.fail "not enter"
                )
        )


viewContent : Model -> Html Msg
viewContent model =
    case model of
        EnteringUrl _ ->
            text ""

        Loading _ ->
            div
                [ style "color" "#718096"
                , style "font-style" "italic"
                ]
                [ text "Loading replay…" ]

        Retrying _ ->
            div
                [ style "color" "#718096"
                , style "font-style" "italic"
                ]
                [ text "Loading replay…" ]

        Loaded _ replay ->
            viewReplay replay

        Failed _ _ ->
            text ""


viewReplay : Replay.Replay -> Html Msg
viewReplay replay =
    div [] (List.map (viewSection replay.players) replay.sections)


playerColor : Maybe Replay.Players -> String -> String
playerColor players name =
    case players of
        Just p ->
            if name == p.red then
                "#c53030"

            else if name == p.blue then
                "#2c5282"

            else
                "#2d3748"

        Nothing ->
            "#2d3748"


viewSection : Maybe Replay.Players -> Replay.Section -> Html Msg
viewSection players section =
    case section of
        Replay.SetupSection lines ->
            viewBoxSection { label = "Setup", labelColor = "#718096" } lines

        Replay.TurnSection turn lines ->
            let
                badgeColor =
                    playerColor players turn.player

                borderColor =
                    badgeColor ++ "40"
            in
            div [ style "margin-bottom" "1.5rem" ]
                [ div
                    [ style "display" "flex"
                    , style "align-items" "center"
                    , style "gap" "0.6rem"
                    , style "margin-bottom" "0.5rem"
                    ]
                    [ span
                        [ style "background" badgeColor
                        , style "color" "white"
                        , style "font-size" "0.7rem"
                        , style "font-weight" "700"
                        , style "letter-spacing" "0.08em"
                        , style "padding" "0.2rem 0.55rem"
                        , style "border-radius" "4px"
                        ]
                        [ text ("TURN " ++ String.fromInt turn.number) ]
                    , span
                        [ style "font-weight" "600"
                        , style "color" "#4a5568"
                        , style "font-size" "0.95rem"
                        ]
                        [ text turn.player ]
                    ]
                , div
                    [ style "border-left" ("3px solid " ++ borderColor)
                    , style "padding-left" "0.75rem"
                    ]
                    (List.map viewLine lines)
                ]

        Replay.CheckupSection lines ->
            viewBoxSection { label = "Pokémon Checkup", labelColor = "#b7791f" } lines

        Replay.ResultSection result ->
            viewResult players result


viewSectionBadge : String -> String -> Html Msg
viewSectionBadge color label =
    span
        [ style "background" color
        , style "color" "white"
        , style "font-size" "0.7rem"
        , style "font-weight" "700"
        , style "letter-spacing" "0.08em"
        , style "text-transform" "uppercase"
        , style "padding" "0.2rem 0.55rem"
        , style "border-radius" "4px"
        ]
        [ text label ]


viewBoxSection :
    { label : String, labelColor : String }
    -> List Replay.ReplayLine
    -> Html Msg
viewBoxSection { label, labelColor } lines =
    div [ style "margin-bottom" "1.5rem" ]
        [ div [ style "margin-bottom" "0.5rem" ]
            [ viewSectionBadge labelColor label ]
        , div
            [ style "border-left" ("3px solid " ++ labelColor ++ "40")
            , style "padding-left" "0.75rem"
            ]
            (List.map viewLine lines)
        ]


viewResult : Maybe Replay.Players -> Replay.MatchResult -> Html Msg
viewResult players result =
    let
        winnerColor =
            playerColor players result.winner
    in
    div [ style "margin-bottom" "1.5rem" ]
        [ div [ style "margin-bottom" "0.5rem" ]
            [ viewSectionBadge "#718096" "Result" ]
        , div
            [ style "border-left" "3px solid #71809640"
            , style "padding-left" "0.75rem"
            ]
            [ div
                [ style "font-size" "0.9rem"
                , style "color" "#4a5568"
                , style "padding" "0.2rem 0"
                , style "line-height" "1.5"
                ]
                [ text result.reason ]
            , div
                [ style "font-size" "0.95rem"
                , style "font-weight" "700"
                , style "color" winnerColor
                , style "padding" "0.2rem 0"
                ]
                [ text (result.winner ++ " wins.") ]
            ]
        ]


viewLine : Replay.ReplayLine -> Html Msg
viewLine line =
    case line of
        Replay.TopLine content ->
            div
                [ style "padding" "0.2rem 0"
                , style "font-size" "0.9rem"
                , style "color" "#2d3748"
                , style "line-height" "1.5"
                ]
                (viewInlineText content)

        Replay.DetailLine content ->
            div
                [ style "padding" "0.15rem 0 0.15rem 1.25rem"
                , style "font-size" "0.875rem"
                , style "color" "#4a5568"
                , style "line-height" "1.5"
                ]
                (viewInlineText content)

        Replay.BulletLine content ->
            div
                [ style "padding" "0.1rem 0 0.1rem 2.5rem"
                , style "font-size" "0.85rem"
                , style "color" "#718096"
                , style "line-height" "1.4"
                ]
                (viewInlineText content)



-- INLINE CARD REFERENCE PARSING


type TextSegment
    = PlainText String
    | CardId String


viewInlineText : String -> List (Html Msg)
viewInlineText str =
    List.map viewSegment (segmentText str)


viewSegment : TextSegment -> Html Msg
viewSegment seg =
    case seg of
        PlainText str ->
            text str

        CardId id ->
            span
                [ style "font-family" "'Courier New', monospace"
                , style "font-size" "0.78em"
                , style "background" "#edf2f7"
                , style "color" "#553c9a"
                , style "padding" "0.1em 0.35em"
                , style "border-radius" "3px"
                , style "white-space" "nowrap"
                ]
                [ text id ]


segmentText : String -> List TextSegment
segmentText str =
    case String.split "(" str of
        [] ->
            []

        first :: rest ->
            PlainText first :: List.concatMap parseParen rest


parseParen : String -> List TextSegment
parseParen str =
    case String.split ")" str of
        id :: remainderParts ->
            if isCardId id then
                [ CardId id
                , PlainText (String.join ")" remainderParts)
                ]

            else
                [ PlainText ("(" ++ str) ]

        [] ->
            [ PlainText ("(" ++ str) ]


isCardId : String -> Bool
isCardId s =
    not (String.isEmpty s)
        && String.all (\c -> Char.isAlpha c || Char.isDigit c || c == '_' || c == '-') s
