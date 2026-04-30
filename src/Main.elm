port module Main exposing (CardPopup(..), Model(..), Msg(..), init, main, update)

import Browser
import Html exposing (Html, button, div, h1, img, input, span, text)
import Html.Attributes exposing (placeholder, src, style, type_, value)
import Html.Events exposing (onClick, onInput)
import Http
import Json.Decode as Decode
import Replay
import Url


port pushUrl : { url : String, index : Int } -> Cmd msg


port onSwipe : (String -> msg) -> Sub msg


init : { replayUrl : String, sectionIndex : Int } -> ( Model, Cmd Msg )
init flags =
    let
        url =
            String.trim flags.replayUrl
    in
    if String.isEmpty url then
        ( EnteringUrl "", Cmd.none )

    else
        ( Loading url flags.sectionIndex
        , Http.get { url = url, expect = Http.expectString GotReplay }
        )


main : Program { replayUrl : String, sectionIndex : Int } Model Msg
main =
    Browser.element
        { init = init
        , update = update
        , view = view
        , subscriptions = \_ -> onSwipe GotSwipe
        }



-- MODEL


type CardPopup
    = FetchingCard String
    | ShowingCard String String
    | CardNotFound String


type Model
    = EnteringUrl String
    | Loading String Int
    | Retrying String Int
    | Loaded String Replay.Replay Int (Maybe CardPopup)
    | Failed String String


currentUrl : Model -> String
currentUrl model =
    case model of
        EnteringUrl url ->
            url

        Loading url _ ->
            url

        Retrying url _ ->
            url

        Loaded url _ _ _ ->
            url

        Failed url _ ->
            url



-- UPDATE


type Msg
    = UrlChanged String
    | LoadClicked
    | GotReplay (Result Http.Error String)
    | FirstSection
    | PrevSection
    | NextSection
    | LastSection
    | GotSwipe String
    | CardClicked String
    | GotCardImage String (Result Http.Error String)
    | CloseCard


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
                ( Loading url 0
                , Http.get { url = url, expect = Http.expectString GotReplay }
                )

        GotReplay result ->
            case model of
                Loading url idx ->
                    case result of
                        Ok content ->
                            loadReplay url idx content

                        Err Http.NetworkError ->
                            ( Retrying url idx
                            , Http.get { url = proxyUrl url, expect = Http.expectString GotReplay }
                            )

                        Err err ->
                            ( Failed url (httpErrorToString err), Cmd.none )

                Retrying url idx ->
                    case result of
                        Ok content ->
                            loadReplay url idx content

                        Err err ->
                            ( Failed url (httpErrorToString err), Cmd.none )

                _ ->
                    ( model, Cmd.none )

        FirstSection ->
            case model of
                Loaded url replay _ _ ->
                    ( Loaded url replay 0 Nothing
                    , pushUrl { url = url, index = 0 }
                    )

                _ ->
                    ( model, Cmd.none )

        PrevSection ->
            case model of
                Loaded url replay i _ ->
                    let
                        newIndex =
                            max 0 (i - 1)
                    in
                    ( Loaded url replay newIndex Nothing
                    , pushUrl { url = url, index = newIndex }
                    )

                _ ->
                    ( model, Cmd.none )

        NextSection ->
            case model of
                Loaded url replay i _ ->
                    let
                        newIndex =
                            min (List.length replay.sections - 1) (i + 1)
                    in
                    ( Loaded url replay newIndex Nothing
                    , pushUrl { url = url, index = newIndex }
                    )

                _ ->
                    ( model, Cmd.none )

        LastSection ->
            case model of
                Loaded url replay _ _ ->
                    let
                        newIndex =
                            List.length replay.sections - 1
                    in
                    ( Loaded url replay newIndex Nothing
                    , pushUrl { url = url, index = newIndex }
                    )

                _ ->
                    ( model, Cmd.none )

        GotSwipe direction ->
            case direction of
                "left" ->
                    update NextSection model

                "right" ->
                    update PrevSection model

                _ ->
                    ( model, Cmd.none )

        CardClicked id ->
            case model of
                Loaded url replay i _ ->
                    case cardApiUrl id of
                        Just apiUrl ->
                            ( Loaded url replay i (Just (FetchingCard id))
                            , Http.get
                                { url = apiUrl
                                , expect = Http.expectString (GotCardImage id)
                                }
                            )

                        Nothing ->
                            ( Loaded url replay i (Just (CardNotFound id)), Cmd.none )

                _ ->
                    ( model, Cmd.none )

        GotCardImage id result ->
            case model of
                Loaded url replay i _ ->
                    let
                        popup =
                            case result of
                                Ok body ->
                                    case Decode.decodeString (Decode.field "image" Decode.string) body of
                                        Ok imageUrl ->
                                            ShowingCard id imageUrl

                                        Err _ ->
                                            CardNotFound id

                                Err _ ->
                                    CardNotFound id
                    in
                    ( Loaded url replay i (Just popup), Cmd.none )

                _ ->
                    ( model, Cmd.none )

        CloseCard ->
            case model of
                Loaded url replay i _ ->
                    ( Loaded url replay i Nothing, Cmd.none )

                _ ->
                    ( model, Cmd.none )


loadReplay : String -> Int -> String -> ( Model, Cmd Msg )
loadReplay url requestedIndex content =
    let
        replay =
            Replay.parse content

        index =
            min (max 0 requestedIndex) (max 0 (List.length replay.sections - 1))
    in
    if List.isEmpty replay.sections then
        ( Failed url "No replay content found — check the URL", Cmd.none )

    else
        ( Loaded url replay index Nothing, pushUrl { url = url, index = index } )


proxyUrl : String -> String
proxyUrl url =
    "https://api.allorigins.win/raw?url=" ++ Url.percentEncode url


{-| Build the TCGdex set-card API URL for a replay card ID like "sv4_160_ph".
Returns Nothing if the ID has no underscore (can't be split into set + local ID).
-}
cardApiUrl : String -> Maybe String
cardApiUrl id =
    case String.split "_" id of
        setCode :: localId :: _ ->
            Just
                ("https://api.tcgdex.net/v2/en/sets/"
                    ++ replaySetIdToTcgDex setCode
                    ++ "/"
                    ++ localId
                )

        _ ->
            Nothing


{-| Convert a replay set code to the TCGdex set ID.

Replay format uses hyphens for fractional sets and no leading zeros:
  sv4, sv4-5, me1, swsh12-5
TCGdex format uses dots and leading zeros for sv/me series:
  sv04, sv04.5, me01, swsh12.5
-}
replaySetIdToTcgDex : String -> String
replaySetIdToTcgDex code =
    case code of
        "mebsp" ->
            "mep"

        "svbsp" ->
            "svp"

        "zsv10-5" ->
            "sv10.5b"

        "rsv10-5" ->
            "sv10.5w"

        _ ->
            code
                |> dotifyFractional
                |> zeroPadSetPrefix


dotifyFractional : String -> String
dotifyFractional s =
    case String.split "-5" s of
        [ prefix, suffix ] ->
            prefix ++ ".5" ++ suffix

        _ ->
            s


zeroPadSetPrefix : String -> String
zeroPadSetPrefix s =
    if String.startsWith "sv" s then
        zeroPadAfterPrefix "sv" s

    else if String.startsWith "me" s then
        zeroPadAfterPrefix "me" s

    else
        s


zeroPadAfterPrefix : String -> String -> String
zeroPadAfterPrefix prefix s =
    let
        rest =
            String.dropLeft (String.length prefix) s
    in
    case String.toList rest of
        [] ->
            s

        [ d ] ->
            if Char.isDigit d then
                prefix ++ "0" ++ rest

            else
                s

        d :: next :: _ ->
            if Char.isDigit d && not (Char.isDigit next) then
                prefix ++ "0" ++ rest

            else
                s


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
        , case model of
            Loaded _ _ _ (Just popup) ->
                viewCardPopup popup

            _ ->
                text ""
        ]


viewUrlBar : Model -> Html Msg
viewUrlBar model =
    let
        url =
            currentUrl model

        isLoading =
            case model of
                Loading _ _ ->
                    True

                Retrying _ _ ->
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

        Loading _ _ ->
            div
                [ style "color" "#718096"
                , style "font-style" "italic"
                ]
                [ text "Loading replay…" ]

        Retrying _ _ ->
            div
                [ style "color" "#718096"
                , style "font-style" "italic"
                ]
                [ text "Loading replay…" ]

        Loaded _ replay index _ ->
            viewReplay replay index

        Failed _ _ ->
            text ""


viewReplay : Replay.Replay -> Int -> Html Msg
viewReplay replay index =
    let
        total =
            List.length replay.sections

        section =
            replay.sections |> List.drop index |> List.head
    in
    case section of
        Nothing ->
            text ""

        Just s ->
            viewSectionWithNav replay.players (index > 0) (index < total - 1) s


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


viewSectionWithNav : Maybe Replay.Players -> Bool -> Bool -> Replay.Section -> Html Msg
viewSectionWithNav players hasPrev hasNext section =
    case section of
        Replay.SetupSection lines ->
            viewNavSection
                { badge = viewSectionBadge "#718096" "Setup"
                , extra = []
                , borderColor = "#71809640"
                , content = List.map viewLine lines
                , hasPrev = hasPrev
                , hasNext = hasNext
                }

        Replay.TurnSection turn lines ->
            let
                badgeColor =
                    playerColor players turn.player
            in
            viewNavSection
                { badge = viewSectionBadge badgeColor ("Turn " ++ String.fromInt turn.number)
                , extra =
                    [ span
                        [ style "font-weight" "600"
                        , style "color" "#4a5568"
                        , style "font-size" "0.95rem"
                        ]
                        [ text turn.player ]
                    ]
                , borderColor = badgeColor ++ "40"
                , content = List.map viewLine lines
                , hasPrev = hasPrev
                , hasNext = hasNext
                }

        Replay.CheckupSection lines ->
            viewNavSection
                { badge = viewSectionBadge "#b7791f" "Pokémon Checkup"
                , extra = []
                , borderColor = "#b7791f40"
                , content = List.map viewLine lines
                , hasPrev = hasPrev
                , hasNext = hasNext
                }

        Replay.ResultSection result ->
            let
                winnerColor =
                    playerColor players result.winner
            in
            viewNavSection
                { badge = viewSectionBadge "#718096" "Result"
                , extra = []
                , borderColor = "#71809640"
                , content =
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
                , hasPrev = hasPrev
                , hasNext = hasNext
                }


viewNavSection :
    { badge : Html Msg
    , extra : List (Html Msg)
    , borderColor : String
    , content : List (Html Msg)
    , hasPrev : Bool
    , hasNext : Bool
    }
    -> Html Msg
viewNavSection { badge, extra, borderColor, content, hasPrev, hasNext } =
    div []
        [ div
            [ style "display" "flex"
            , style "justify-content" "space-between"
            , style "align-items" "center"
            , style "margin-bottom" "0.5rem"
            ]
            [ div
                [ style "display" "flex"
                , style "gap" "0.35rem"
                ]
                [ navArrow hasPrev FirstSection "«"
                , navArrow hasPrev PrevSection "‹"
                ]
            , div
                [ style "display" "flex"
                , style "align-items" "center"
                , style "gap" "0.6rem"
                ]
                (badge :: extra)
            , div
                [ style "display" "flex"
                , style "gap" "0.35rem"
                ]
                [ navArrow hasNext NextSection "›"
                , navArrow hasNext LastSection "»"
                ]
            ]
        , div
            [ style "border-left" ("3px solid " ++ borderColor)
            , style "padding-left" "0.75rem"
            ]
            content
        ]


navArrow : Bool -> Msg -> String -> Html Msg
navArrow visible msg symbol =
    button
        [ onClick msg
        , style "visibility"
            (if visible then
                "visible"

             else
                "hidden"
            )
        , style "background" "none"
        , style "border" "none"
        , style "cursor" "pointer"
        , style "font-size" "1rem"
        , style "color" "#718096"
        , style "padding" "0"
        , style "line-height" "1"
        ]
        [ text symbol ]


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
    | CardRef String String


viewInlineText : String -> List (Html Msg)
viewInlineText str =
    List.map viewSegment (segmentText str)


viewSegment : TextSegment -> Html Msg
viewSegment seg =
    case seg of
        PlainText str ->
            text str

        CardRef id name ->
            span
                [ onClick (CardClicked id)
                , style "font-family" "'Courier New', monospace"
                , style "font-size" "0.78em"
                , style "background" "#edf2f7"
                , style "color" "#553c9a"
                , style "padding" "0.1em 0.35em"
                , style "border-radius" "3px"
                , style "white-space" "nowrap"
                , style "cursor" "pointer"
                ]
                [ text
                    (if String.isEmpty name then
                        id

                     else
                        name
                    )
                ]


viewCardPopup : CardPopup -> Html Msg
viewCardPopup popup =
    div
        [ onClick CloseCard
        , style "position" "fixed"
        , style "inset" "0"
        , style "background" "rgba(0,0,0,0.6)"
        , style "display" "flex"
        , style "align-items" "center"
        , style "justify-content" "center"
        , style "z-index" "1000"
        , style "cursor" "pointer"
        ]
        [ case popup of
            FetchingCard _ ->
                div
                    [ style "color" "white"
                    , style "font-style" "italic"
                    , style "font-size" "1rem"
                    ]
                    [ text "Loading…" ]

            CardNotFound id ->
                div
                    [ style "background" "white"
                    , style "border-radius" "8px"
                    , style "padding" "1.5rem 2rem"
                    , style "color" "#4a5568"
                    , style "font-size" "0.95rem"
                    ]
                    [ text ("Card not found: " ++ id) ]

            ShowingCard _ imageUrl ->
                img
                    [ src (imageUrl ++ "/high.webp")
                    , style "max-height" "80vh"
                    , style "max-width" "90vw"
                    , style "border-radius" "8px"
                    , style "box-shadow" "0 8px 32px rgba(0,0,0,0.5)"
                    , style "display" "block"
                    ]
                    []
        ]


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
                let
                    remainder =
                        String.join ")" remainderParts

                    ( name, rest ) =
                        extractCardName remainder
                in
                [ CardRef id name
                , PlainText rest
                ]

            else
                [ PlainText ("(" ++ str) ]

        [] ->
            [ PlainText ("(" ++ str) ]


{-| Extract a human-readable card name from the text that follows a card ID.

The content is the remainder after `)`, e.g. `" Yveltal to the Active Spot."`.
Returns `(name, rest)` where name is the Title-Case card name (e.g. `"Yveltal"`)
and rest is everything after the name in the original content.

Name tokens are words that start with an uppercase letter, or exactly "ex".
Collection stops at the first word that is neither, or when a word ends with
"," or "." (terminal punctuation – the stripped word becomes the final name token).
A trailing ":" on the assembled name is stripped (handles "Binding Mochi:").

-}
extractCardName : String -> ( String, String )
extractCardName content =
    let
        leadingSpace =
            if String.startsWith " " content then
                1

            else
                0

        body =
            String.dropLeft leadingSpace content

        words =
            String.words body

        ( nameWords, hadTerminalPunct ) =
            collectName words

        rawName =
            String.join " " nameWords

        name =
            trimTrailingColon rawName

        offset =
            leadingSpace
                + String.length rawName
                + (if hadTerminalPunct then
                    1

                   else
                    0
                  )

        rest =
            String.dropLeft offset content
    in
    ( name, rest )


{-| Collect name-token words, returning (words, hadTerminalPunct).

Normal stop: first word that is neither a name token nor a connector.
Connectors ("of", "at") are included only when the immediately following word
is a name token — this handles "Forest of Vitality", "Academy at Night".

Terminal punctuation ("," or ".") on the last name token is stripped and
hadTerminalPunct is True, so the caller can account for the extra character
in the offset.  Exception: if the next word is also a name token, the period
is treated as an abbreviation dot (e.g. "Exp. Share") and kept as-is.

-}
collectName : List String -> ( List String, Bool )
collectName words =
    case words of
        [] ->
            ( [], False )

        word :: rest ->
            let
                ( stripped, hadPunct ) =
                    stripTerminalPunct word
            in
            if isNameToken stripped then
                if hadPunct then
                    -- Decide whether "." is an abbreviation dot or a sentence-ending period.
                    --
                    -- Abbreviation rule: apply only when ALL of:
                    --   • stripped word is ≤ 3 chars (short abbreviation like "Exp")
                    --   • stripped word is not "ex" ("ex." is always the Pokémon-type
                    --     suffix at the end of a name, never a mid-name abbreviation)
                    --   • the next word is a name token
                    --
                    -- This lets "Exp. Share" through while keeping "Lunatone." and
                    -- "ex." (e.g. "Bloodmoon Ursaluna ex.") as sentence-ending periods.
                    case rest of
                        nextWord :: _ ->
                            let
                                ( nextStripped, _ ) =
                                    stripTerminalPunct nextWord
                            in
                            if
                                stripped /= "ex"
                                    && String.length stripped <= 3
                                    && isNameToken nextStripped
                            then
                                let
                                    ( moreWords, finalHadPunct ) =
                                        collectName rest
                                in
                                ( word :: moreWords, finalHadPunct )

                            else
                                ( [ stripped ], True )

                        [] ->
                            ( [ stripped ], True )

                else
                    let
                        ( moreWords, finalHadPunct ) =
                            collectName rest
                    in
                    ( stripped :: moreWords, finalHadPunct )

            else if isVersionToken stripped then
                -- Version-number suffix (e.g. "3.0" in "Pokégear 3.0").
                -- Use the stripped form so a sentence-ending "." is not included
                -- in the name; propagate hadPunct so the offset is adjusted.
                ( [ stripped ], hadPunct )

            else if isConnector word then
                -- Include this connector only if the next word is a name token
                case rest of
                    nextWord :: _ ->
                        let
                            ( nextStripped, _ ) =
                                stripTerminalPunct nextWord
                        in
                        if isNameToken nextStripped then
                            let
                                ( moreWords, finalHadPunct ) =
                                    collectName rest
                            in
                            ( word :: moreWords, finalHadPunct )

                        else
                            ( [], False )

                    [] ->
                        ( [], False )

            else
                ( [], False )


{-| A word is a name token if it can be part of a Pokemon card name.

Rules:
  - "ex" (the lowercase card-type suffix) is always a name token.
  - Otherwise the word must start with an uppercase letter AND pass two checks:
      1. safeDigits: at most one digit, and only as the very last character
         ("Porygon2" ✓, "Alannvs86" ✗, "Mom3nt" ✗).
      2. not hasCamelCase: no uppercase letter immediately following a lowercase
         letter.  A hyphen resets the "previous was lower" flag, so hyphenated
         names like "Buddy-Buddy", "Ting-Lu", "Roto-Stick" are fine.
         Mixed-CamelCase player names like "NoxFoxEX" are rejected.

-}
isNameToken : String -> Bool
isNameToken word =
    word == "ex"
        || (case String.uncons word of
                Nothing ->
                    False

                Just ( c, _ ) ->
                    Char.isUpper c
                        && safeDigits word
                        && not (hasCamelCase word)
           )


{-| True when the word's digits (if any) consist of exactly one digit at the end.

"Porygon2" → True   "Alannvs86" → False (two digits)   "Mom3nt" → False (mid-word)

-}
safeDigits : String -> Bool
safeDigits word =
    let
        digits =
            String.filter Char.isDigit word
    in
    String.isEmpty digits
        || (String.length digits == 1 && String.endsWith digits word)


{-| True when the word contains an uppercase letter immediately following a
lowercase letter — the defining pattern of CamelCase player names like "NoxFoxEX".
A hyphen is treated as a separator (resets the "previous was lower" flag), so
"Buddy-Buddy" and "Ting-Lu" are NOT considered CamelCase.
-}
hasCamelCase : String -> Bool
hasCamelCase word =
    let
        ( _, found ) =
            List.foldl
                (\c ( prevWasLower, acc ) ->
                    ( Char.isLower c, acc || (Char.isUpper c && prevWasLower) )
                )
                ( False, False )
                (String.toList word)
    in
    found


{-| Short lowercase connector words that can appear inside card names.

Used by collectName to allow "Forest of Vitality", "Academy at Night" etc.
A connector is included only when the word immediately following it is a name token.

-}
isConnector : String -> Bool
isConnector word =
    word == "of" || word == "at"


{-| True for version-number tokens like "3.0" in "Pokégear 3.0".

Must start with a digit, contain at least one ".", and consist only of digits
and dots.  The dot requirement excludes bare numbers (turn counts, damage
values etc.) from being mistakenly absorbed into the card name.

-}
isVersionToken : String -> Bool
isVersionToken word =
    case String.uncons word of
        Nothing ->
            False

        Just ( c, _ ) ->
            Char.isDigit c
                && String.contains "." word
                && String.all (\ch -> Char.isDigit ch || ch == '.') word


{-| Strip a trailing "," or "." from a word, returning (strippedWord, True) if found. -}
stripTerminalPunct : String -> ( String, Bool )
stripTerminalPunct word =
    if String.endsWith "," word || String.endsWith "." word then
        ( String.dropRight 1 word, True )

    else
        ( word, False )


{-| Strip a trailing ":" from an assembled name (e.g. "Binding Mochi:" → "Binding Mochi"). -}
trimTrailingColon : String -> String
trimTrailingColon s =
    if String.endsWith ":" s then
        String.dropRight 1 s

    else
        s


isCardId : String -> Bool
isCardId s =
    not (String.isEmpty s)
        && String.contains "_" s
        && String.all (\c -> Char.isAlpha c || Char.isDigit c || c == '_' || c == '-') s
