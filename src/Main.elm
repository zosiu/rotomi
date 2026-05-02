port module Main exposing (CardAttack, CardAbility, CardData, MoveKind(..), MoveHighlight, CardPopup(..), Model(..), Msg(..), HandState, emptyHand, applyGroupToHand, BenchState, emptyBench, applyGroupToBench, PileState, emptyPiles, applyGroupToPiles, StadiumState, applyGroupToStadium, CurrentPlay, currentPlayFromGroup, init, main, update)

import Browser
import Browser.Dom
import Dict exposing (Dict)
import Html exposing (Html, button, div, img, input, p, span, text)
import Html.Attributes exposing (id, placeholder, src, style, type_, value)
import Html.Events exposing (onClick, onInput)
import Http
import Json.Decode as Decode
import Task
import Action
import Replay
import Url


port pushUrl : { url : String, index : Int, groupIndex : Int, flipOpponent : Bool } -> Cmd msg


port onSwipe : (String -> msg) -> Sub msg


init : { replayUrl : String, sectionIndex : Int, groupIndex : Int, flipOpponent : Bool } -> ( Model, Cmd Msg )
init flags =
    let
        url =
            String.trim flags.replayUrl
    in
    if String.isEmpty url then
        ( EnteringUrl "", Cmd.none )

    else
        ( Loading url flags.sectionIndex flags.groupIndex flags.flipOpponent
        , Http.get { url = url, expect = Http.expectString GotReplay }
        )


main : Program { replayUrl : String, sectionIndex : Int, groupIndex : Int, flipOpponent : Bool } Model Msg
main =
    Browser.element
        { init = init
        , update = update
        , view = view
        , subscriptions = \_ -> onSwipe GotSwipe
        }



-- MODEL


type alias CardAttack =
    { name : String
    , cost : List String
    , damage : String
    , effect : String
    }


type alias CardAbility =
    { abilityType : String
    , name : String
    , effect : String
    }


type alias CardData =
    { imageUrl : Maybe String
    , attacks : List CardAttack
    , abilities : List CardAbility
    }


type MoveKind
    = IsAbility
    | IsAttack


type alias MoveHighlight =
    { phrase : String
    , kind : Maybe MoveKind
    , cardId : String
    }


type alias DamageInfo =
    { breakdownLines : List String
    }


type alias HandState =
    { red : List (Maybe Action.CardRef)
    , blue : List (Maybe Action.CardRef)
    }


emptyHand : HandState
emptyHand =
    { red = [], blue = [] }


type alias BenchState =
    { red : List Action.CardRef
    , blue : List Action.CardRef
    }


emptyBench : BenchState
emptyBench =
    { red = [], blue = [] }


{-| The card that was played in the current action group, plus cards
discarded as part of the effect and cards drawn as a result of it.
-}
type alias PlayerCards =
    { discarded : List (Maybe Action.CardRef)
    , shuffled : List (Maybe Action.CardRef)
    , drawn : List (Maybe Action.CardRef)
    }


emptyPlayerCards : PlayerCards
emptyPlayerCards =
    { discarded = [], shuffled = [], drawn = [] }


type alias CurrentPlay =
    { player : String
    -- Nothing = prize-taking action (no single card played)
    , card : Maybe Action.CardRef
    -- per-player card buckets (red = local recorder, blue = opponent)
    , red : PlayerCards
    , blue : PlayerCards
    }


type CardPopup
    = FetchingCard String
    | FetchingMove String String
    | ShowingCard String CardData
    | ShowingMove CardData String
    | ShowingDamageInfo DamageInfo
    | CardNotFound String


type Model
    = EnteringUrl String
    | Loading String Int Int Bool
    | Retrying String Int Int Bool
    | Loaded String Replay.Replay Int Int (Maybe CardPopup) (Dict String CardData) Bool
    | Failed String String


currentUrl : Model -> String
currentUrl model =
    case model of
        EnteringUrl url ->
            url

        Loading url _ _ _ ->
            url

        Retrying url _ _ _ ->
            url

        Loaded url _ _ _ _ _ _ ->
            url

        Failed url _ ->
            url


currentFlipOpponent : Model -> Bool
currentFlipOpponent model =
    case model of
        Loading _ _ _ flip ->
            flip

        Retrying _ _ _ flip ->
            flip

        Loaded _ _ _ _ _ _ flip ->
            flip

        _ ->
            True



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
    | MoveClicked String String
    | DamageClicked DamageInfo
    | GotCardImage String (Result Http.Error String)
    | FlipOpponentToggled
    | CloseCard
    | NoOp


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
                ( Loading url 0 0 (currentFlipOpponent model)
                , Http.get { url = url, expect = Http.expectString GotReplay }
                )

        GotReplay result ->
            case model of
                Loading url idx gIdx flip ->
                    case result of
                        Ok content ->
                            loadReplay url idx gIdx flip content

                        Err Http.NetworkError ->
                            ( Retrying url idx gIdx flip
                            , Http.get { url = proxyUrl url, expect = Http.expectString GotReplay }
                            )

                        Err err ->
                            ( Failed url (httpErrorToString err), Cmd.none )

                Retrying url idx gIdx flip ->
                    case result of
                        Ok content ->
                            loadReplay url idx gIdx flip content

                        Err err ->
                            ( Failed url (httpErrorToString err), Cmd.none )

                _ ->
                    ( model, Cmd.none )

        FirstSection ->
            case model of
                Loaded url replay _ _ _ cache flip ->
                    ( Loaded url replay 0 0 Nothing cache flip
                    , Cmd.batch
                        [ pushUrl { url = url, index = 0, groupIndex = 0, flipOpponent = flip }
                        , scrollToTop
                        , fetchHandCards replay.players replay 0 0 cache
                        ]
                    )

                _ ->
                    ( model, Cmd.none )

        PrevSection ->
            case model of
                Loaded url replay i g _ cache flip ->
                    if g > 0 then
                        ( Loaded url replay i (g - 1) Nothing cache flip
                        , Cmd.batch
                            [ pushUrl { url = url, index = i, groupIndex = g - 1, flipOpponent = flip }
                            , scrollToTop
                            , fetchHandCards replay.players replay i (g - 1) cache
                            ]
                        )

                    else if i > 0 then
                        let
                            newI =
                                i - 1

                            prevSection =
                                replay.sections |> List.drop newI |> List.head

                            prevCount =
                                prevSection |> Maybe.map sectionGroupCount |> Maybe.withDefault 1

                            newG =
                                max 0 (prevCount - 1)
                        in
                        ( Loaded url replay newI newG Nothing cache flip
                        , Cmd.batch
                            [ pushUrl { url = url, index = newI, groupIndex = newG, flipOpponent = flip }
                            , scrollToTop
                            , fetchHandCards replay.players replay newI newG cache
                            ]
                        )

                    else
                        ( model, Cmd.none )

                _ ->
                    ( model, Cmd.none )

        NextSection ->
            case model of
                Loaded url replay i g _ cache flip ->
                    let
                        currentSection =
                            replay.sections |> List.drop i |> List.head

                        totalGroups =
                            currentSection |> Maybe.map sectionGroupCount |> Maybe.withDefault 1

                        totalSections =
                            List.length replay.sections
                    in
                    if g < totalGroups - 1 then
                        ( Loaded url replay i (g + 1) Nothing cache flip
                        , Cmd.batch
                            [ pushUrl { url = url, index = i, groupIndex = g + 1, flipOpponent = flip }
                            , scrollToTop
                            , fetchHandCards replay.players replay i (g + 1) cache
                            ]
                        )

                    else if i < totalSections - 1 then
                        ( Loaded url replay (i + 1) 0 Nothing cache flip
                        , Cmd.batch
                            [ pushUrl { url = url, index = i + 1, groupIndex = 0, flipOpponent = flip }
                            , scrollToTop
                            , fetchHandCards replay.players replay (i + 1) 0 cache
                            ]
                        )

                    else
                        ( model, Cmd.none )

                _ ->
                    ( model, Cmd.none )

        LastSection ->
            case model of
                Loaded url replay _ _ _ cache flip ->
                    let
                        lastI =
                            List.length replay.sections - 1

                        lastSection =
                            replay.sections |> List.drop lastI |> List.head

                        lastCount =
                            lastSection |> Maybe.map sectionGroupCount |> Maybe.withDefault 1

                        lastG =
                            max 0 (lastCount - 1)
                    in
                    ( Loaded url replay lastI lastG Nothing cache flip
                    , Cmd.batch
                        [ pushUrl { url = url, index = lastI, groupIndex = lastG, flipOpponent = flip }
                        , scrollToTop
                        , fetchHandCards replay.players replay lastI lastG cache
                        ]
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
                Loaded url replay i g _ cache flip ->
                    case Dict.get id cache of
                        Just cardData ->
                            ( Loaded url replay i g (Just (ShowingCard id cardData)) cache flip, Cmd.none )

                        Nothing ->
                            case cardApiUrl id of
                                Just apiUrl ->
                                    ( Loaded url replay i g (Just (FetchingCard id)) cache flip
                                    , Http.get
                                        { url = apiUrl
                                        , expect = Http.expectString (GotCardImage id)
                                        }
                                    )

                                Nothing ->
                                    ( Loaded url replay i g (Just (CardNotFound id)) cache flip, Cmd.none )

                _ ->
                    ( model, Cmd.none )

        MoveClicked cardId moveName ->
            case model of
                Loaded url replay i g _ cache flip ->
                    case Dict.get cardId cache of
                        Just cardData ->
                            ( Loaded url replay i g (Just (ShowingMove cardData moveName)) cache flip, Cmd.none )

                        Nothing ->
                            case cardApiUrl cardId of
                                Just apiUrl ->
                                    ( Loaded url replay i g (Just (FetchingMove cardId moveName)) cache flip
                                    , Http.get { url = apiUrl, expect = Http.expectString (GotCardImage cardId) }
                                    )

                                Nothing ->
                                    ( Loaded url replay i g (Just (CardNotFound cardId)) cache flip, Cmd.none )

                _ ->
                    ( model, Cmd.none )

        DamageClicked info ->
            case model of
                Loaded url replay i g _ cache flip ->
                    ( Loaded url replay i g (Just (ShowingDamageInfo info)) cache flip, Cmd.none )

                _ ->
                    ( model, Cmd.none )

        GotCardImage id result ->
            case model of
                Loaded url replay i g currentPopup cache flip ->
                    let
                        -- True only when the user explicitly requested this card
                        -- (by clicking a pill / card thumbnail), so we should open a popup.
                        -- Background hand-prefetch requests must NOT disturb the popup state.
                        isUserFetch =
                            case currentPopup of
                                Just (FetchingCard fetchId) ->
                                    fetchId == id

                                Just (FetchingMove _ _) ->
                                    True

                                _ ->
                                    False

                        ( nextPopup, newCache ) =
                            case result of
                                Ok body ->
                                    case decodeCardData body of
                                        Just cardData ->
                                            let
                                                resolvedData =
                                                    case cardData.imageUrl of
                                                        Just _ ->
                                                            cardData

                                                        Nothing ->
                                                            case Decode.decodeString (Decode.field "name" Decode.string) body of
                                                                Ok apiName ->
                                                                    case basicEnergyImageUrl apiName of
                                                                        Just fallbackUrl ->
                                                                            { cardData | imageUrl = Just fallbackUrl }

                                                                        Nothing ->
                                                                            cardData

                                                                Err _ ->
                                                                    cardData

                                                popup =
                                                    if isUserFetch then
                                                        case currentPopup of
                                                            Just (FetchingMove _ moveName) ->
                                                                Just (ShowingMove resolvedData moveName)

                                                            _ ->
                                                                case resolvedData.imageUrl of
                                                                    Just _ ->
                                                                        Just (ShowingCard id resolvedData)

                                                                    Nothing ->
                                                                        Just (CardNotFound id)

                                                    else
                                                        currentPopup
                                            in
                                            ( popup, Dict.insert id resolvedData cache )

                                        Nothing ->
                                            ( if isUserFetch then Just (CardNotFound id) else currentPopup, cache )

                                Err _ ->
                                    ( if isUserFetch then Just (CardNotFound id) else currentPopup, cache )
                    in
                    ( Loaded url replay i g nextPopup newCache flip, Cmd.none )

                _ ->
                    ( model, Cmd.none )

        CloseCard ->
            case model of
                Loaded url replay i g _ cache flip ->
                    ( Loaded url replay i g Nothing cache flip, Cmd.none )

                _ ->
                    ( model, Cmd.none )

        FlipOpponentToggled ->
            case model of
                Loaded url replay i g popup cache flip ->
                    let
                        newFlip =
                            not flip
                    in
                    ( Loaded url replay i g popup cache newFlip
                    , pushUrl { url = url, index = i, groupIndex = g, flipOpponent = newFlip }
                    )

                _ ->
                    ( model, Cmd.none )

        NoOp ->
            ( model, Cmd.none )


scrollToBottom : Cmd Msg
scrollToBottom =
    Browser.Dom.setViewportOf "action-content" 0 999999
        |> Task.attempt (\_ -> NoOp)


scrollToTop : Cmd Msg
scrollToTop =
    Browser.Dom.setViewportOf "action-content" 0 0
        |> Task.attempt (\_ -> NoOp)


loadReplay : String -> Int -> Int -> Bool -> String -> ( Model, Cmd Msg )
loadReplay url requestedIndex requestedGroupIndex flip content =
    let
        -- Normalize Unicode curly apostrophes (U+2019 ' RIGHT SINGLE QUOTATION MARK)
        -- to plain ASCII apostrophes. Some replay sources use smart quotes in player
        -- possessives ("takeshi516's") which breaks the parsePokemonRef splitter.
        normalized =
            String.replace "\u{2019}" "'" content

        replay =
            Replay.parse normalized

        index =
            min (max 0 requestedIndex) (max 0 (List.length replay.sections - 1))

        groupIndex =
            let
                section =
                    replay.sections |> List.drop index |> List.head

                maxGroup =
                    section |> Maybe.map sectionGroupCount |> Maybe.withDefault 0 |> (\n -> max 0 (n - 1))
            in
            min (max 0 requestedGroupIndex) maxGroup
    in
    if List.isEmpty replay.sections then
        ( Failed url "No replay content found — check the URL", Cmd.none )

    else
        ( Loaded url replay index groupIndex Nothing Dict.empty flip
        , Cmd.batch
            [ pushUrl { url = url, index = index, groupIndex = groupIndex, flipOpponent = flip }
            , fetchHandCards replay.players replay index groupIndex Dict.empty
            ]
        )


{-| Issue HTTP fetches for any known hand cards that are not yet in the cache.
Safe to call on every navigation — already-cached cards are skipped.
-}
fetchHandCards : Maybe Replay.Players -> Replay.Replay -> Int -> Int -> Dict String CardData -> Cmd Msg
fetchHandCards maybePlayers replay si gi cache =
    case maybePlayers of
        Nothing ->
            Cmd.none

        Just players ->
            let
                hand =
                    computeHand players replay si gi

                handRefs =
                    List.filterMap identity hand.red ++ List.filterMap identity hand.blue

                bench =
                    computeBench players replay si gi

                activeSpots =
                    computeActive players replay si gi

                benchRefs =
                    bench.red ++ bench.blue

                activeRefs =
                    List.filterMap identity [ activeSpots.red, activeSpots.blue ]

                stadiumRef =
                    computeStadium players replay si gi
                        |> Maybe.map .card
                        |> Maybe.map List.singleton
                        |> Maybe.withDefault []

                -- Also fetch the played card + all known panel cards so images load without a click
                playRefs =
                    getCurrentGroup replay si gi
                        |> Maybe.andThen (currentPlayFromGroup players)
                        |> Maybe.map
                            (\play ->
                                let
                                    allCards pc =
                                        List.filterMap identity pc.discarded
                                            ++ List.filterMap identity pc.shuffled
                                            ++ List.filterMap identity pc.drawn
                                in
                                (case play.card of
                                    Just c ->
                                        [ c ]

                                    Nothing ->
                                        []
                                )
                                    ++ allCards play.red
                                    ++ allCards play.blue
                            )
                        |> Maybe.withDefault []

                -- Unique IDs not already in cache
                knownIds =
                    (handRefs ++ benchRefs ++ activeRefs ++ stadiumRef ++ playRefs)
                        |> List.map .id
                        |> List.foldl
                            (\id acc ->
                                if List.member id acc then
                                    acc

                                else
                                    id :: acc
                            )
                            []
                        |> List.filter (\id -> not (Dict.member id cache))
            in
            Cmd.batch
                (List.filterMap
                    (\id ->
                        cardApiUrl id
                            |> Maybe.map
                                (\apiUrl ->
                                    Http.get
                                        { url = apiUrl
                                        , expect = Http.expectString (GotCardImage id)
                                        }
                                )
                    )
                    knownIds
                )


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


{-| Fallback image base URLs for Basic Energy cards that exist in TCGdex but
have no image field in their API response.  The lookup strips an optional
"Basic " prefix so it works whether the API returns "Grass Energy" or
"Basic Grass Energy".
-}
basicEnergyImageUrl : String -> Maybe String
basicEnergyImageUrl apiName =
    let
        key =
            if String.startsWith "Basic " apiName then
                String.dropLeft 6 apiName

            else
                apiName
    in
    case key of
        "Grass Energy" ->
            Just "https://assets.tcgdex.net/en/sv/sv02/278"

        "Water Energy" ->
            Just "https://assets.tcgdex.net/en/sv/sv02/279"

        "Fire Energy" ->
            Just "https://assets.tcgdex.net/en/sv/sv03/230"

        "Lightning Energy" ->
            Just "https://assets.tcgdex.net/en/sv/sv01/257"

        "Fighting Energy" ->
            Just "https://assets.tcgdex.net/en/sv/sv01/258"

        "Psychic Energy" ->
            Just "https://assets.tcgdex.net/en/sv/sv03.5/207"

        "Darkness Energy" ->
            Just "https://assets.tcgdex.net/en/sv/sv06.5/098"

        "Metal Energy" ->
            Just "https://assets.tcgdex.net/en/sv/sv06.5/099"

        _ ->
            Nothing


decodeCardData : String -> Maybe CardData
decodeCardData body =
    let
        attackDecoder =
            Decode.map4 CardAttack
                (Decode.field "name" Decode.string)
                (Decode.oneOf
                    [ Decode.field "cost" (Decode.list Decode.string)
                    , Decode.succeed []
                    ])
                (Decode.oneOf
                    [ Decode.field "damage" Decode.int |> Decode.map String.fromInt
                    , Decode.field "damage" Decode.string
                    , Decode.succeed ""
                    ])
                (Decode.oneOf
                    [ Decode.field "effect" Decode.string
                    , Decode.succeed ""
                    ])

        abilityDecoder =
            Decode.map3 CardAbility
                (Decode.field "type" Decode.string)
                (Decode.field "name" Decode.string)
                (Decode.field "effect" Decode.string)

        cardDecoder =
            Decode.map3 CardData
                (Decode.maybe (Decode.field "image" Decode.string))
                (Decode.oneOf
                    [ Decode.field "attacks" (Decode.list attackDecoder)
                    , Decode.succeed []
                    ])
                (Decode.oneOf
                    [ Decode.field "abilities" (Decode.list abilityDecoder)
                    , Decode.succeed []
                    ])
    in
    Decode.decodeString cardDecoder body |> Result.toMaybe


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



-- HAND STATE


addCard : String -> String -> Maybe Action.CardRef -> HandState -> HandState
addCard red player card hand =
    if player == red then
        { hand | red = hand.red ++ [ card ] }
    else
        { hand | blue = hand.blue ++ [ card ] }


addUnknowns : String -> String -> Int -> HandState -> HandState
addUnknowns red player n hand =
    List.foldl (\_ h -> addCard red player Nothing h) hand (List.repeat n ())


addKnownCards : String -> String -> List Action.CardRef -> HandState -> HandState
addKnownCards red player cards hand =
    List.foldl (\c h -> addCard red player (Just c) h) hand cards


removeById : String -> String -> String -> HandState -> HandState
removeById red player cardId hand =
    let
        remove list =
            case list of
                [] ->
                    []

                x :: rest ->
                    case x of
                        Just c ->
                            if c.id == cardId then
                                rest
                            else
                                x :: remove rest

                        Nothing ->
                            x :: remove rest

        removeFallback list =
            -- If no known card matched, remove the first unknown slot
            case list of
                [] ->
                    []

                x :: rest ->
                    case x of
                        Nothing ->
                            rest

                        Just _ ->
                            x :: removeFallback rest

        attempt list =
            let
                reduced =
                    remove list
            in
            if List.length reduced < List.length list then
                reduced
            else
                removeFallback list
    in
    if player == red then
        { hand | red = attempt hand.red }
    else
        { hand | blue = attempt hand.blue }


removeN : String -> String -> Int -> HandState -> HandState
removeN red player n hand =
    let
        dropLast count list =
            List.take (max 0 (List.length list - count)) list
    in
    if player == red then
        { hand | red = dropLast n hand.red }
    else
        { hand | blue = dropLast n hand.blue }


setHand : String -> String -> List (Maybe Action.CardRef) -> HandState -> HandState
setHand red player cards hand =
    if player == red then
        { hand | red = cards }
    else
        { hand | blue = cards }


bulletCardList : Action.ActionGroup -> List Action.CardRef
bulletCardList group =
    group.details
        |> List.concatMap .bullets
        |> List.filterMap
            (\b ->
                case b.action of
                    Action.CardList cards ->
                        Just cards

                    _ ->
                        Nothing
            )
        |> List.concat


detailCardList : Action.DetailAction -> List Action.CardRef
detailCardList detail =
    detail.bullets
        |> List.filterMap
            (\b ->
                case b.action of
                    Action.CardList cards ->
                        Just cards

                    _ ->
                        Nothing
            )
        |> List.concat


{-| Cards listed as top-level detail actions (BulletLines attached directly
under a TopLine, e.g. the card list under a DrewAndPlayed action).
-}
groupTopDetailCardList : Action.ActionGroup -> List Action.CardRef
groupTopDetailCardList group =
    group.details
        |> List.filterMap
            (\d ->
                case d.action of
                    Action.CardList cards ->
                        Just cards

                    _ ->
                        Nothing
            )
        |> List.concat


{-| Remove the last N Nothing slots from a hand side.
Used to strip unknown drawn cards out of the hand display when showing them in
the played panel instead.
-}
removeLastNUnknowns : Int -> List (Maybe Action.CardRef) -> List (Maybe Action.CardRef)
removeLastNUnknowns n handSide =
    handSide
        |> List.reverse
        |> List.foldl
            (\card ( remaining, acc ) ->
                if remaining > 0 && card == Nothing then
                    ( remaining - 1, acc )

                else
                    ( remaining, card :: acc )
            )
            ( n, [] )
        |> Tuple.second


{-| Strip all drawn cards (known and unknown) from a hand side so they appear
only in the played panel, not in both places at once.
-}
stripDrawnFromHandSide : List (Maybe Action.CardRef) -> List (Maybe Action.CardRef) -> List (Maybe Action.CardRef)
stripDrawnFromHandSide drawn handSide =
    let
        unknownCount =
            List.length (List.filter ((==) Nothing) drawn)
    in
    handSide
        |> removeKnownFromHandSide (List.filterMap identity drawn)
        |> removeLastNUnknowns unknownCount


{-| Remove the first occurrence of each card (matched by id) from a hand side.
Used to strip drawn cards out of the hand display when showing them in the
played panel instead.
-}
removeKnownFromHandSide : List Action.CardRef -> List (Maybe Action.CardRef) -> List (Maybe Action.CardRef)
removeKnownFromHandSide toRemove handSide =
    List.foldl
        (\ref acc ->
            let
                go remaining =
                    case remaining of
                        [] ->
                            []

                        (Just c :: rest) ->
                            if c.id == ref.id then
                                rest

                            else
                                Just c :: go rest

                        (Nothing :: rest) ->
                            Nothing :: go rest
            in
            go acc
        )
        handSide
        toRemove


applyTopAction : String -> HandState -> Action.ActionGroup -> HandState
applyTopAction red hand group =
    case group.action of
        Action.OpeningDraw { player, count } ->
            let
                known =
                    bulletCardList group
            in
            if List.isEmpty known then
                addUnknowns red player count hand
            else
                setHand red player (List.map Just known) hand

        Action.MulliganTaken { player } ->
            -- The player shuffles their hand back and silently redraws 7 cards;
            -- no new OpeningDraw line appears in the log, so we fill with 7 unknowns.
            setHand red player (List.repeat 7 Nothing) hand

        Action.MulliganBonus { player, count } ->
            addUnknowns red player count hand

        Action.Drew { player, card } ->
            addCard red player card hand

        Action.DrewCount { player, count } ->
            addUnknowns red player count hand

        Action.DrewCard { player, card, andPlayed } ->
            case andPlayed of
                Nothing ->
                    addCard red player (Just card) hand

                Just _ ->
                    hand

        Action.CardAddedToHand { card, player } ->
            addCard red player card hand

        Action.MovedToHand { player, card, count } ->
            case card of
                Just c ->
                    addCard red player (Just c) hand

                Nothing ->
                    addUnknowns red player (Maybe.withDefault 1 count) hand

        Action.TookPrize _ ->
            -- Each prize taken is always followed by a separate "A card was added to
            -- X's hand." / "(id) Card was added to X's hand." TopLine group, so we
            -- let those CardAddedToHand groups do the hand update instead of adding
            -- unknowns here (which would double-count).
            hand

        Action.PlayedPokemon { player, card } ->
            removeById red player card.id hand

        Action.PlayedStadium { player, card } ->
            removeById red player card.id hand

        Action.PlayedTrainer { player, card } ->
            removeById red player card.id hand

        Action.Attached { player, item } ->
            removeById red player item.id hand

        Action.Evolved { player, to } ->
            removeById red player to.id hand

        Action.Discarded { player, count } ->
            removeN red player count hand

        Action.DiscardedCard { player, card } ->
            removeById red player card.id hand

        Action.ShuffledInto { player, card, count } ->
            case card of
                Just c ->
                    removeById red player c.id hand

                Nothing ->
                    removeN red player (Maybe.withDefault 1 count) hand

        Action.PutOnTop { player, card } ->
            removeById red player card.id hand

        Action.PutOnBottom { player, card, count } ->
            case card of
                Just c ->
                    removeById red player c.id hand

                Nothing ->
                    removeN red player (Maybe.withDefault 1 count) hand

        _ ->
            hand


applyDetailAction : String -> HandState -> Action.DetailAction -> HandState
applyDetailAction red hand detail =
    case detail.action of
        Action.DrewCount { player, count } ->
            let
                known =
                    detailCardList detail
            in
            if List.isEmpty known then
                addUnknowns red player count hand
            else
                addKnownCards red player known hand

        Action.Drew { player, card } ->
            addCard red player card hand

        Action.DrewCard { player, card, andPlayed } ->
            case andPlayed of
                Nothing ->
                    addCard red player (Just card) hand

                Just _ ->
                    hand

        Action.CardAddedToHand { card, player } ->
            addCard red player card hand

        Action.MovedToHand { player, card, count } ->
            case card of
                Just c ->
                    addCard red player (Just c) hand

                Nothing ->
                    addUnknowns red player (Maybe.withDefault 1 count) hand

        Action.Attached { player, item } ->
            removeById red player item.id hand

        Action.DiscardedCard { player, card } ->
            removeById red player card.id hand

        Action.Discarded { player, count } ->
            removeN red player count hand

        Action.ShuffledInto { player, card, count } ->
            case card of
                Just c ->
                    removeById red player c.id hand

                Nothing ->
                    removeN red player (Maybe.withDefault 1 count) hand

        Action.PutOnBottom { player, card, count } ->
            case card of
                Just c ->
                    removeById red player c.id hand

                Nothing ->
                    removeN red player (Maybe.withDefault 1 count) hand

        Action.PlayedPokemon { player, card } ->
            removeById red player card.id hand

        Action.PlayedTrainer { player, card } ->
            removeById red player card.id hand

        _ ->
            hand


applyGroupToHand : String -> HandState -> Action.ActionGroup -> HandState
applyGroupToHand red hand group =
    let
        hand1 =
            applyTopAction red hand group
    in
    List.foldl (\detail h -> applyDetailAction red h detail) hand1 group.details


collectAllGroups : Replay.Replay -> Int -> Int -> List Action.ActionGroup
collectAllGroups replay sectionIndex groupIndex =
    replay.sections
        |> List.indexedMap
            (\si section ->
                let
                    groups =
                        Action.groupLines (sectionLines section)
                in
                if si < sectionIndex then
                    groups

                else if si == sectionIndex then
                    List.take (groupIndex + 1) groups

                else
                    []
            )
        |> List.concat


{-| Fix detail lines where the player is wrongly attributed to the local recorder
(players.red) when no revealed cards exist. Count-only draw/shuffle lines without
a CardList bullet always belong to the opponent (players.blue). This correction is
idempotent: if a future log already has the correct player, nothing changes.
-}
correctDetailPlayer : Replay.Players -> Action.DetailAction -> Action.DetailAction
correctDetailPlayer players detail =
    let
        hasRevealedCards =
            List.any
                (\b ->
                    case b.action of
                        Action.CardList _ ->
                            True

                        _ ->
                            False
                )
                detail.bullets

        -- Lines with revealed cards belong to the local recorder; count-only lines to the opponent.
        correctPlayer =
            if hasRevealedCards then
                players.red

            else
                players.blue

        replacePlayer : String -> String -> String -> String
        replacePlayer from to raw =
            if String.startsWith (from ++ " ") raw then
                to ++ String.dropLeft (String.length from) raw

            else
                raw
    in
    case detail.action of
        Action.DrewCount { player, count } ->
            if player /= correctPlayer then
                { detail
                    | action = Action.DrewCount { player = correctPlayer, count = count }
                    , raw = replacePlayer player correctPlayer detail.raw
                }

            else
                detail

        Action.ShuffledInto { player, card, count } ->
            case card of
                Nothing ->
                    if player /= correctPlayer then
                        { detail
                            | action = Action.ShuffledInto { player = correctPlayer, card = Nothing, count = count }
                            , raw = replacePlayer player correctPlayer detail.raw
                        }

                    else
                        detail

                Just _ ->
                    detail

        Action.PutOnBottom { player, card, count } ->
            case card of
                Nothing ->
                    if player /= correctPlayer then
                        { detail
                            | action = Action.PutOnBottom { player = correctPlayer, card = Nothing, count = count }
                            , raw =
                                detail.raw
                                    |> replacePlayer player correctPlayer
                                    |> String.replace (player ++ "'s") (correctPlayer ++ "'s")
                        }

                    else
                        detail

                Just _ ->
                    detail

        _ ->
            detail


correctGroupPlayers : Replay.Players -> Action.ActionGroup -> Action.ActionGroup
correctGroupPlayers players group =
    let
        drewCountCount =
            List.foldl
                (\d acc ->
                    case d.action of
                        Action.DrewCount _ ->
                            acc + 1

                        _ ->
                            acc
                )
                0
                group.details

        countOnlyShuffleCount =
            List.foldl
                (\d acc ->
                    case d.action of
                        Action.ShuffledInto { card } ->
                            if card == Nothing then
                                acc + 1

                            else
                                acc

                        _ ->
                            acc
                )
                0
                group.details

        countOnlyPutOnBottomCount =
            List.foldl
                (\d acc ->
                    case d.action of
                        Action.PutOnBottom { card } ->
                            if card == Nothing then
                                acc + 1

                            else
                                acc

                        _ ->
                            acc
                )
                0
                group.details

        correctDetail detail =
            case detail.action of
                Action.DrewCount _ ->
                    if drewCountCount >= 2 then
                        correctDetailPlayer players detail

                    else
                        detail

                Action.ShuffledInto { card } ->
                    if card == Nothing && countOnlyShuffleCount >= 2 then
                        correctDetailPlayer players detail

                    else
                        detail

                Action.PutOnBottom { card } ->
                    if card == Nothing && countOnlyPutOnBottomCount >= 2 then
                        correctDetailPlayer players detail

                    else
                        detail

                _ ->
                    detail
    in
    { group | details = List.map correctDetail group.details }


collectAndCorrectGroups : Replay.Players -> Replay.Replay -> Int -> Int -> List Action.ActionGroup
collectAndCorrectGroups players replay sectionIndex groupIndex =
    collectAllGroups replay sectionIndex groupIndex
        |> List.map (correctGroupPlayers players)


computeHand : Replay.Players -> Replay.Replay -> Int -> Int -> HandState
computeHand players replay sectionIndex groupIndex =
    List.foldl (\group h -> applyGroupToHand players.red h group) emptyHand
        (collectAndCorrectGroups players replay sectionIndex groupIndex)


-- PILE STATE


type alias PileState =
    { deckRed : Int
    , deckBlue : Int
    , discardRed : Int
    , discardBlue : Int
    , prizesRed : Int
    , prizesBlue : Int
    }


emptyPiles : PileState
emptyPiles =
    { deckRed = 60, deckBlue = 60, discardRed = 0, discardBlue = 0, prizesRed = 0, prizesBlue = 0 }


pilesDeckDelta : String -> String -> Int -> PileState -> PileState
pilesDeckDelta red player delta piles =
    if player == red then
        { piles | deckRed = piles.deckRed + delta }

    else
        { piles | deckBlue = piles.deckBlue + delta }


pilesPrizeDelta : String -> String -> Int -> PileState -> PileState
pilesPrizeDelta red player delta piles =
    if player == red then
        { piles | prizesRed = piles.prizesRed + delta }

    else
        { piles | prizesBlue = piles.prizesBlue + delta }


pilesDiscardDelta : String -> String -> Int -> PileState -> PileState
pilesDiscardDelta red player delta piles =
    if player == red then
        { piles | discardRed = piles.discardRed + delta }

    else
        { piles | discardBlue = piles.discardBlue + delta }


applyActionToPiles : String -> Bool -> Action.Action -> PileState -> PileState
applyActionToPiles red isSetup action piles =
    case action of
        Action.OpeningDraw { player, count } ->
            pilesDeckDelta red player -count piles

        Action.MulliganBonus { player, count } ->
            pilesDeckDelta red player -count piles

        Action.Drew { player } ->
            pilesDeckDelta red player -1 piles

        Action.DrewCount { player, count } ->
            pilesDeckDelta red player -count piles

        Action.DrewCard { player } ->
            pilesDeckDelta red player -1 piles

        Action.DrewAndPlayed { player, count } ->
            pilesDeckDelta red player -count piles

        Action.ShuffledInto { player, card, count } ->
            pilesDeckDelta red player
                (case card of
                    Just _ ->
                        1

                    Nothing ->
                        Maybe.withDefault 1 count
                )
                piles

        Action.PutOnTop { player } ->
            pilesDeckDelta red player 1 piles

        Action.PutOnBottom { player, card, count } ->
            pilesDeckDelta red player
                (case card of
                    Just _ ->
                        1

                    Nothing ->
                        Maybe.withDefault 1 count
                )
                piles

        Action.PlayedPokemon { player, position } ->
            -- During setup each player's first Active Spot play also selects 6
            -- prize cards from their deck.
            if isSetup && position == Action.ActiveSpot then
                piles
                    |> pilesDeckDelta red player -6
                    |> pilesPrizeDelta red player 6

            else
                piles

        Action.PlayedTrainer { player } ->
            pilesDiscardDelta red player 1 piles

        Action.CardDiscardedFrom { pokemon } ->
            pilesDiscardDelta red pokemon.player 1 piles

        Action.Discarded { player, count } ->
            pilesDiscardDelta red player count piles

        Action.DiscardedCard { player } ->
            pilesDiscardDelta red player 1 piles

        Action.TookPrize { player, count } ->
            pilesPrizeDelta red player -count piles

        _ ->
            piles


applyGroupToPiles : String -> Bool -> PileState -> Action.ActionGroup -> PileState
applyGroupToPiles red isSetup piles group =
    let
        piles1 =
            applyActionToPiles red isSetup group.action piles
    in
    List.foldl
        (\detail p ->
            List.foldl
                (\bullet bp -> applyActionToPiles red isSetup bullet.action bp)
                (applyActionToPiles red isSetup detail.action p)
                detail.bullets
        )
        piles1
        group.details


computePiles : Replay.Players -> Replay.Replay -> Int -> Int -> PileState
computePiles players replay sectionIndex groupIndex =
    replay.sections
        |> List.indexedMap
            (\si section ->
                let
                    isSetup =
                        case section of
                            Replay.SetupSection _ ->
                                True

                            _ ->
                                False

                    groups =
                        Action.groupLines (sectionLines section)

                    trimmed =
                        if si < sectionIndex then
                            groups

                        else if si == sectionIndex then
                            List.take (groupIndex + 1) groups

                        else
                            []
                in
                List.map (\g -> ( isSetup, correctGroupPlayers players g )) trimmed
            )
        |> List.concat
        |> List.foldl (\( isSetup, group ) p -> applyGroupToPiles players.red isSetup p group) emptyPiles


-- BENCH STATE


addToBench : String -> String -> Action.CardRef -> BenchState -> BenchState
addToBench red player card bench =
    if player == red then
        { bench | red = bench.red ++ [ card ] }

    else
        { bench | blue = bench.blue ++ [ card ] }


removeFromBench : String -> String -> String -> BenchState -> BenchState
removeFromBench red player cardId bench =
    let
        removeFirst list =
            case list of
                [] ->
                    []

                x :: rest ->
                    if x.id == cardId then
                        rest

                    else
                        x :: removeFirst rest
    in
    if player == red then
        { bench | red = removeFirst bench.red }

    else
        { bench | blue = removeFirst bench.blue }


replaceOnBench : String -> String -> String -> Action.CardRef -> BenchState -> BenchState
replaceOnBench red player fromId to bench =
    let
        replaceFirst list =
            case list of
                [] ->
                    []

                x :: rest ->
                    if x.id == fromId then
                        to :: rest

                    else
                        x :: replaceFirst rest
    in
    if player == red then
        { bench | red = replaceFirst bench.red }

    else
        { bench | blue = replaceFirst bench.blue }


applyActionToBench : String -> Action.Action -> BenchState -> BenchState
applyActionToBench red action bench =
    case action of
        Action.PlayedPokemon { player, card, position } ->
            case position of
                Action.BenchSpot ->
                    addToBench red player card bench

                _ ->
                    bench

        Action.DrewCard { player, card, andPlayed } ->
            case andPlayed of
                Just Action.BenchSpot ->
                    addToBench red player card bench

                _ ->
                    bench

        Action.Evolved { player, from, to, position } ->
            case position of
                Action.BenchSpot ->
                    replaceOnBench red player from.id to bench

                _ ->
                    bench

        Action.KnockedOut { pokemon } ->
            removeFromBench red pokemon.player pokemon.card.id bench

        Action.MovedToActive { pokemon } ->
            removeFromBench red pokemon.player pokemon.card.id bench

        Action.Retreated { player, card } ->
            addToBench red player card bench

        Action.Switched { player, from, to } ->
            -- `from` leaves the bench to become active; `to` leaves active to join bench
            bench
                |> removeFromBench red player from.id
                |> (if String.isEmpty to.id then identity else addToBench red player to)

        _ ->
            bench


applyGroupToBench : String -> BenchState -> Action.ActionGroup -> BenchState
applyGroupToBench red bench group =
    let
        bench1 =
            applyActionToBench red group.action bench
    in
    List.foldl
        (\detail b ->
            let
                -- DrewAndPlayed as a detail: cards go straight to bench via bullet CardList
                b1 =
                    case detail.action of
                        Action.DrewAndPlayed { player, position } ->
                            case position of
                                Action.BenchSpot ->
                                    List.foldl (addToBench red player) b
                                        (detailCardList detail)

                                _ ->
                                    b

                        _ ->
                            applyActionToBench red detail.action b
            in
            List.foldl
                (\bullet bb -> applyActionToBench red bullet.action bb)
                b1
                detail.bullets
        )
        bench1
        group.details


computeBench : Replay.Players -> Replay.Replay -> Int -> Int -> BenchState
computeBench players replay sectionIndex groupIndex =
    List.foldl (\group b -> applyGroupToBench players.red b group) emptyBench
        (collectAndCorrectGroups players replay sectionIndex groupIndex)


-- ACTIVE STATE


type alias ActiveState =
    { red : Maybe Action.CardRef
    , blue : Maybe Action.CardRef
    }


emptyActive : ActiveState
emptyActive =
    { red = Nothing, blue = Nothing }


setActive : String -> String -> Action.CardRef -> ActiveState -> ActiveState
setActive red player card active =
    if player == red then
        { active | red = Just card }

    else
        { active | blue = Just card }


applyActionToActive : String -> Action.Action -> ActiveState -> ActiveState
applyActionToActive red action active =
    case action of
        Action.PlayedPokemon { player, card, position } ->
            case position of
                Action.ActiveSpot ->
                    setActive red player card active

                _ ->
                    active

        Action.DrewCard { player, card, andPlayed } ->
            case andPlayed of
                Just Action.ActiveSpot ->
                    setActive red player card active

                _ ->
                    active

        Action.Evolved { player, from, to, position } ->
            case position of
                Action.ActiveSpot ->
                    let
                        matches side =
                            case side of
                                Just c ->
                                    c.id == from.id

                                Nothing ->
                                    False
                    in
                    if player == red && matches active.red then
                        { active | red = Just to }

                    else if player /= red && matches active.blue then
                        { active | blue = Just to }

                    else
                        active

                _ ->
                    active

        Action.KnockedOut { pokemon } ->
            let
                matches side =
                    case side of
                        Just c ->
                            c.id == pokemon.card.id

                        Nothing ->
                            False
            in
            if pokemon.player == red && matches active.red then
                { active | red = Nothing }

            else if pokemon.player /= red && matches active.blue then
                { active | blue = Nothing }

            else
                active

        Action.MovedToActive { pokemon } ->
            setActive red pokemon.player pokemon.card active

        Action.Switched { player, from } ->
            -- `from` is the Pokémon coming from the bench to become the new active
            setActive red player from active

        Action.Retreated { player, card } ->
            let
                matches side =
                    case side of
                        Just c ->
                            c.id == card.id

                        Nothing ->
                            False
            in
            if player == red && matches active.red then
                { active | red = Nothing }

            else if player /= red && matches active.blue then
                { active | blue = Nothing }

            else
                active

        _ ->
            active


applyGroupToActive : String -> ActiveState -> Action.ActionGroup -> ActiveState
applyGroupToActive red active group =
    let
        active1 =
            applyActionToActive red group.action active
    in
    List.foldl
        (\detail a ->
            List.foldl
                (\bullet ba -> applyActionToActive red bullet.action ba)
                (applyActionToActive red detail.action a)
                detail.bullets
        )
        active1
        group.details


computeActive : Replay.Players -> Replay.Replay -> Int -> Int -> ActiveState
computeActive players replay sectionIndex groupIndex =
    List.foldl (\group a -> applyGroupToActive players.red a group) emptyActive
        (collectAndCorrectGroups players replay sectionIndex groupIndex)


-- STADIUM STATE


type alias StadiumState =
    { player : String
    , card : Action.CardRef
    }


applyGroupToStadium : Maybe StadiumState -> Action.ActionGroup -> Maybe StadiumState
applyGroupToStadium stadium group =
    let
        applyAction action st =
            case action of
                Action.PlayedStadium { player, card } ->
                    Just { player = player, card = card }

                _ ->
                    st
    in
    List.foldl
        (\detail st ->
            List.foldl (\bullet bs -> applyAction bullet.action bs) (applyAction detail.action st) detail.bullets
        )
        (applyAction group.action stadium)
        group.details


computeStadium : Replay.Players -> Replay.Replay -> Int -> Int -> Maybe StadiumState
computeStadium players replay sectionIndex groupIndex =
    List.foldl (\group st -> applyGroupToStadium st group) Nothing
        (collectAndCorrectGroups players replay sectionIndex groupIndex)


{-| Return the action group at the given (sectionIndex, groupIndex) position. -}
getCurrentGroup : Replay.Replay -> Int -> Int -> Maybe Action.ActionGroup
getCurrentGroup replay si gi =
    replay.sections
        |> List.drop si
        |> List.head
        |> Maybe.map (sectionLines >> Action.groupLines)
        |> Maybe.andThen (List.drop gi >> List.head)


{-| If this group represents a trainer card being played (\"played X for\"),
return the played card and any cards explicitly discarded in its detail lines,
split by player (red = local recorder, blue = opponent).
-}
currentPlayFromGroup : Replay.Players -> Action.ActionGroup -> Maybe CurrentPlay
currentPlayFromGroup players group =
    case group.action of
        Action.PlayedTrainer { player, card } ->
            let
                correctedDetails =
                    (correctGroupPlayers players group).details

                -- Which player owns this detail line?
                detailOwner d =
                    case d.action of
                        Action.DiscardedCard rec ->
                            Just rec.player

                        Action.Discarded rec ->
                            Just rec.player

                        Action.ShuffledInto rec ->
                            Just rec.player

                        Action.DrewCount rec ->
                            Just rec.player

                        Action.Drew rec ->
                            Just rec.player

                        Action.DrewCard rec ->
                            Just rec.player

                        Action.MovedToHand rec ->
                            Just rec.player

                        _ ->
                            Nothing

                detailsFor p =
                    List.filter (\d -> detailOwner d == Just p) correctedDetails

                extractDiscards ds =
                    List.concatMap
                        (\d ->
                            case d.action of
                                Action.DiscardedCard discardData ->
                                    [ Just discardData.card ]

                                Action.Discarded discardData ->
                                    let
                                        known =
                                            detailCardList d
                                    in
                                    if List.isEmpty known then
                                        List.repeat discardData.count Nothing

                                    else
                                        List.map Just known

                                _ ->
                                    []
                        )
                        ds

                extractShuffled ds =
                    List.concatMap
                        (\d ->
                            case d.action of
                                Action.ShuffledInto shuffleData ->
                                    case shuffleData.card of
                                        Just c ->
                                            [ Just c ]

                                        Nothing ->
                                            let
                                                known =
                                                    detailCardList d
                                            in
                                            if List.isEmpty known then
                                                List.repeat (Maybe.withDefault 1 shuffleData.count) Nothing

                                            else
                                                List.map Just known

                                _ ->
                                    []
                        )
                        ds

                extractDrawn ds =
                    List.concatMap
                        (\d ->
                            case d.action of
                                Action.DrewCount drewData ->
                                    let
                                        known =
                                            detailCardList d
                                    in
                                    if List.isEmpty known then
                                        List.repeat drewData.count Nothing

                                    else
                                        List.map Just known

                                Action.Drew drewData ->
                                    [ drewData.card ]

                                Action.DrewCard drewCardData ->
                                    case drewCardData.andPlayed of
                                        Nothing ->
                                            [ Just drewCardData.card ]

                                        Just _ ->
                                            []

                                Action.MovedToHand movedData ->
                                    case movedData.card of
                                        Just c ->
                                            [ Just c ]

                                        Nothing ->
                                            List.repeat (Maybe.withDefault 1 movedData.count) Nothing

                                _ ->
                                    []
                        )
                        ds

                makePlayerCards p =
                    let
                        ds =
                            detailsFor p
                    in
                    { discarded = extractDiscards ds
                    , shuffled = extractShuffled ds
                    , drawn = extractDrawn ds
                    }
            in
            Just
                { player = player
                , card = Just card
                , red = makePlayerCards players.red
                , blue = makePlayerCards players.blue
                }

        Action.TookPrize { player } ->
            let
                drawnCards =
                    group.details
                        |> List.concatMap
                            (\d ->
                                case d.action of
                                    Action.CardAddedToHand { card } ->
                                        [ card ]

                                    _ ->
                                        []
                            )

                playerCards =
                    { discarded = [], shuffled = [], drawn = drawnCards }
            in
            if List.isEmpty drawnCards then
                Nothing

            else if player == players.red then
                Just { player = player, card = Nothing, red = playerCards, blue = emptyPlayerCards }

            else
                Just { player = player, card = Nothing, red = emptyPlayerCards, blue = playerCards }

        _ ->
            Nothing


viewHandState : Replay.Players -> Dict String CardData -> Bool -> HandState -> BenchState -> ActiveState -> Maybe StadiumState -> PileState -> Maybe CurrentPlay -> Html Msg
viewHandState players cache flipOpponent hand bench active maybeStadium piles maybePlay =
    let
        -- When True, drawn cards are hidden from the hand panel and shown only
        -- in the played panel below. Disabled for now.
        stripDrawnFromHand =
            False

        redDisplay =
            if stripDrawnFromHand then
                case maybePlay of
                    Just play ->
                        stripDrawnFromHandSide play.red.drawn hand.red

                    Nothing ->
                        hand.red

            else
                hand.red

        blueDisplay =
            if stripDrawnFromHand then
                case maybePlay of
                    Just play ->
                        stripDrawnFromHandSide play.blue.drawn hand.blue

                    Nothing ->
                        hand.blue

            else
                hand.blue
    in
    div
        [ style "display" "flex"
        , style "flex-direction" "column"
        , style "gap" "0.75rem"
        , style "padding" "0.5rem 0"
        , style "flex-shrink" "0"
        , style "min-width" "0"
        ]
        [ -- Two-column layout: cards on the left, pile stacks on the right.
          -- Bench rows only span the cards column so centering works correctly.
          div
            [ style "display" "flex"
            , style "align-items" "stretch"
            , style "gap" "0.75rem"
            , style "min-width" "0"
            ]
            [ -- Cards column: hand rows + bench rows
              div
                [ style "display" "flex"
                , style "flex-direction" "column"
                , style "gap" "0.4rem"
                , style "flex" "1"
                , style "min-width" "0"
                ]
                [ viewHandRow "RED" flipOpponent "#c53030" blueDisplay (handCardImage cache)
                , viewBenchRow flipOpponent cache bench.blue
                , viewActiveZone players.red cache flipOpponent active maybeStadium
                , viewBenchRow False cache bench.red
                , viewHandRow "BLUE" False "#2c5282" redDisplay (handCardImage cache)
                ]
            , -- Piles column: blue stacks at top, red stacks at bottom
              div
                [ style "display" "flex"
                , style "flex-direction" "column"
                , style "justify-content" "space-between"
                , style "flex-shrink" "0"
                ]
                [ viewPlayerPiles False piles.deckBlue piles.discardBlue piles.prizesBlue "#c53030"
                , viewPlayerPiles True piles.deckRed piles.discardRed piles.prizesRed "#2c5282"
                ]
            ]

        -- Permanent divider between board state and played card panel
        , div
            [ style "border-top" "1px solid #e2e8f0"
            , style "margin" "0"
            ]
            []

        -- Played card panel — only visible for PlayedTrainer groups
        , case maybePlay of
            Nothing ->
                text ""

            Just play ->
                viewCurrentPlay players cache play
        ]


viewPileStack : String -> Int -> String -> String -> Html Msg
viewPileStack label count bgColor textColor =
    div
        [ style "width" "72px"
        , style "height" "100px"
        , style "border-radius" "4px"
        , style "flex-shrink" "0"
        , style "display" "flex"
        , style "flex-direction" "column"
        , style "align-items" "center"
        , style "justify-content" "center"
        , style "gap" "0.25rem"
        , style "background" bgColor
        , style "color" textColor
        , style "font-size" "0.65rem"
        , style "font-weight" "700"
        , style "letter-spacing" "0.03em"
        , style "user-select" "none"
        ]
        [ div [] [ text label ]
        , div [ style "font-size" "1.1rem" ] [ text (String.fromInt count) ]
        ]


viewPrizeStack : Int -> String -> Html Msg
viewPrizeStack count color =
    div
        [ style "width" "72px"
        , style "height" "100px"
        , style "border-radius" "4px"
        , style "flex-shrink" "0"
        , style "display" "flex"
        , style "align-items" "center"
        , style "justify-content" "center"
        , style "background" color
        , style "color" "white"
        , style "font-size" "1.4rem"
        , style "font-weight" "700"
        , style "user-select" "none"
        ]
        [ text (String.fromInt count) ]


viewHandRow : String -> Bool -> String -> List (Maybe Action.CardRef) -> (Maybe Action.CardRef -> Maybe String) -> Html Msg
viewHandRow playerName upsideDown color cards imageFor =
    div
        [ style "display" "flex"
        , style "align-items" "center"
        , style "gap" "0.35rem"
        , style "min-height" "60px"
        , style "min-width" "0"
        ]
        [ div
            [ style "display" "flex"
            , style "flex-direction" "column"
            , style "align-items" "center"
            , style "flex-shrink" "0"
            , style "gap" "0.2rem"
            ]
            [ div
                [ style "font-size" "0.7rem"
                , style "font-weight" "600"
                , style "color" color
                , style "writing-mode" "vertical-rl"
                , style "transform" "rotate(180deg)"
                , style "overflow" "hidden"
                , style "white-space" "nowrap"
                , style "max-height" "80px"
                ]
                [ text playerName ]
            , div
                [ style "font-size" "0.65rem"
                , style "font-weight" "600"
                , style "color" color
                ]
                [ text ("(" ++ String.fromInt (List.length cards) ++ ")") ]
            ]
        , div
            [ style "display" "flex"
            , style "align-items" "center"
            , style "justify-content" "center"
            , style "gap" "0.35rem"
            , style "overflow-x" "auto"
            , style "flex" "1"
            , style "min-width" "0"
            , style "padding-bottom" "4px"
            ]
            (List.map
                (\item ->
                    case item of
                        KnownPlayCard card ->
                            viewHandCard upsideDown color imageFor (Just card)

                        UnknownPlayCards n ->
                            viewUnknownCardBack "86px" "60px" upsideDown n
                )
                (collapseUnknowns cards)
            )
        ]


viewPlayerPiles : Bool -> Int -> Int -> Int -> String -> Html Msg
viewPlayerPiles prizesOnTop deckCount discardCount prizeCount color =
    let
        deckBin =
            div
                [ style "display" "flex"
                , style "gap" "0.35rem"
                , style "flex-shrink" "0"
                ]
                [ viewPileStack "Deck" deckCount "#bfdbfe" "#1e40af"
                , viewPileStack "Bin" discardCount "#718096" "white"
                ]

        prize =
            viewPrizeStack prizeCount color

        children =
            if prizesOnTop then
                [ prize, deckBin ]

            else
                [ deckBin, prize ]
    in
    div
        [ style "display" "flex"
        , style "flex-direction" "column"
        , style "align-items" "center"
        , style "gap" "0.35rem"
        , style "flex-shrink" "0"
        ]
        children


handCardImage : Dict String CardData -> Maybe Action.CardRef -> Maybe String
handCardImage cache maybeCard =
    case maybeCard of
        Nothing ->
            Nothing

        Just card ->
            Dict.get card.id cache
                |> Maybe.andThen .imageUrl
                |> Maybe.map (\u -> u ++ "/low.webp")


viewHandCard : Bool -> String -> (Maybe Action.CardRef -> Maybe String) -> Maybe Action.CardRef -> Html Msg
viewHandCard upsideDown color imageFor maybeCard =
    let
        -- Cards show only the top half, scaled up so the art fills the viewport
        cardW =
            "86px"

        cardH =
            "60px"

        radius =
            "4px"

        -- Opponent cards are rotated to mimic looking across a table
        rotationStyles =
            if upsideDown then
                [ style "transform" "rotate(180deg)" ]

            else
                []

        -- Shared layout styles for every card variant
        baseStyles =
            [ style "width" cardW
            , style "height" cardH
            , style "border-radius" radius
            , style "flex-shrink" "0"
            , style "box-sizing" "border-box"
            ]
    in
    case maybeCard of
        Just card ->
            case imageFor maybeCard of
                Just imageUrl ->
                    img
                        (baseStyles
                            ++ rotationStyles
                            ++ [ style "object-fit" "cover"
                               , style "object-position" "top"
                               , style "background" "#e2e8f0"
                               , style "cursor" "pointer"
                               , onClick (CardClicked card.id)
                               , src imageUrl
                               ]
                        )
                        []

                Nothing ->
                    viewNoImageCard
                        (baseStyles
                            ++ rotationStyles
                            ++ [ style "cursor" "pointer"
                               , onClick (CardClicked card.id)
                               ]
                        )
                        card.name

        Nothing ->
            -- Unknown card — card back rectangle
            div
                (baseStyles
                    ++ rotationStyles
                    ++ [ style "background" "#bfdbfe"
                       , style "border" "2px solid #1e40af"
                       ]
                )
                []


viewBenchRow : Bool -> Dict String CardData -> List Action.CardRef -> Html Msg
viewBenchRow upsideDown cache cards =
    div
        [ style "display" "flex"
        , style "align-items" "center"
        , style "gap" "0.35rem"
        , style "min-width" "0"
        ]
        [ -- Invisible spacer matching the label column in viewHandRow
          div
            [ style "display" "flex"
            , style "flex-direction" "column"
            , style "align-items" "center"
            , style "flex-shrink" "0"
            , style "gap" "0.2rem"
            , style "visibility" "hidden"
            ]
            [ div
                [ style "font-size" "0.7rem"
                , style "writing-mode" "vertical-rl"
                , style "max-height" "80px"
                ]
                [ text "X" ]
            , div
                [ style "font-size" "0.65rem" ]
                [ text "(0)" ]
            ]
        , div
            [ style "display" "flex"
            , style "align-items" "center"
            , style "justify-content" "center"
            , style "gap" "0.35rem"
            , style "overflow-x" "auto"
            , style "flex" "1"
            , style "min-width" "0"
            , style "min-height" "100px"
            , style "padding" "6px 8px"
            , style "background" "#f7fafc"
            , style "border-radius" "6px"
            ]
            (List.map (viewBenchCard upsideDown cache) cards)
        ]


{-| Fallback for any known card whose image hasn't loaded yet.
Shows a dark card with the card name centered, so it's clear what the card is.
-}
viewNoImageCard : List (Html.Attribute Msg) -> String -> Html Msg
viewNoImageCard extraStyles name =
    div
        ([ style "background" "#1a202c"
         , style "color" "white"
         , style "display" "flex"
         , style "align-items" "center"
         , style "justify-content" "center"
         , style "text-align" "center"
         , style "font-size" "0.6rem"
         , style "font-weight" "600"
         , style "line-height" "1.3"
         , style "padding" "4px"
         , style "overflow" "hidden"
         ]
            ++ extraStyles
        )
        [ text name ]


viewBenchCard : Bool -> Dict String CardData -> Action.CardRef -> Html Msg
viewBenchCard upsideDown cache card =
    let
        maybeUrl =
            Dict.get card.id cache
                |> Maybe.andThen .imageUrl
                |> Maybe.map (\u -> u ++ "/low.webp")

        rotStyles =
            if upsideDown then
                [ style "transform" "rotate(180deg)" ]

            else
                []

        baseStyles =
            [ style "width" "72px"
            , style "height" "100px"
            , style "border-radius" "4px"
            , style "flex-shrink" "0"
            , style "box-sizing" "border-box"
            , style "cursor" "pointer"
            , onClick (CardClicked card.id)
            ]
    in
    case maybeUrl of
        Just u ->
            img
                (baseStyles
                    ++ rotStyles
                    ++ [ style "object-fit" "cover"
                       , style "background" "#e2e8f0"
                       , src u
                       ]
                )
                []

        Nothing ->
            viewNoImageCard (baseStyles ++ rotStyles) card.name



{-| The active zone combines both active spots and the stadium into one row.
Active spots are stacked vertically in the center. Stadium slots sit two card-widths
out on each side: blue's on the left (upside-down), red's on the right.
-}
viewActiveZone : String -> Dict String CardData -> Bool -> ActiveState -> Maybe StadiumState -> Html Msg
viewActiveZone red cache flipOpponent active maybeStadium =
    let
        stadiumSlot upsideDown maybeCard =
            case maybeCard of
                Just card ->
                    viewBenchCard upsideDown cache card

                Nothing ->
                    div
                        [ style "width" "72px"
                        , style "height" "100px"
                        , style "flex-shrink" "0"
                        ]
                        []

        activeCard upsideDown maybeCard =
            case maybeCard of
                Just card ->
                    viewBenchCard upsideDown cache card

                Nothing ->
                    div
                        [ style "width" "72px"
                        , style "height" "100px"
                        , style "border-radius" "4px"
                        , style "flex-shrink" "0"
                        , style "border" "2px dashed #cbd5e0"
                        , style "box-sizing" "border-box"
                        ]
                        []

        ( blueStadium, redStadium ) =
            case maybeStadium of
                Just s ->
                    if s.player == red then
                        ( Nothing, Just s.card )

                    else
                        ( Just s.card, Nothing )

                Nothing ->
                    ( Nothing, Nothing )
    in
    div
        [ style "display" "flex"
        , style "align-items" "center"
        , style "gap" "0.35rem"
        , style "min-width" "0"
        ]
        [ -- Invisible spacer matching the label column in viewHandRow
          div
            [ style "display" "flex"
            , style "flex-direction" "column"
            , style "align-items" "center"
            , style "flex-shrink" "0"
            , style "gap" "0.2rem"
            , style "visibility" "hidden"
            ]
            [ div
                [ style "font-size" "0.7rem"
                , style "writing-mode" "vertical-rl"
                , style "max-height" "80px"
                ]
                [ text "X" ]
            , div
                [ style "font-size" "0.65rem" ]
                [ text "(0)" ]
            ]
        , -- Centered content: stadium + active spots + stadium
          div
            [ style "display" "flex"
            , style "justify-content" "center"
            , style "align-items" "center"
            , style "flex" "1"
            , style "min-width" "0"
            , style "gap" "0"
            ]
            [ -- Blue stadium: two card-widths left of center
              stadiumSlot flipOpponent blueStadium
            , div [ style "width" "72px", style "flex-shrink" "0" ] []

            -- Active spots stacked vertically
            , div
                [ style "display" "flex"
                , style "flex-direction" "column"
                , style "gap" "0.4rem"
                , style "align-items" "center"
                , style "flex-shrink" "0"
                ]
                [ activeCard flipOpponent active.blue
                , activeCard False active.red
                ]
            , div [ style "width" "72px", style "flex-shrink" "0" ] []

            -- Red stadium: two card-widths right of center
            , stadiumSlot False redStadium
            ]
        ]


{-| A card thumbnail at the standard hand size, used for both the hand panel
and the played-card panel. Always an <img> with a gray background placeholder
so there is no flicker when the image loads.
-}
viewKnownCardThumb : Dict String CardData -> Action.CardRef -> Html Msg
viewKnownCardThumb cache card =
    let
        maybeUrl =
            Dict.get card.id cache
                |> Maybe.andThen .imageUrl
                |> Maybe.map (\u -> u ++ "/low.webp")

        baseStyles =
            [ style "width" "72px"
            , style "height" "100px"
            , style "border-radius" "4px"
            , style "flex-shrink" "0"
            , style "box-sizing" "border-box"
            , style "cursor" "pointer"
            , onClick (CardClicked card.id)
            ]
    in
    case maybeUrl of
        Just imageUrl ->
            img
                (baseStyles
                    ++ [ style "object-fit" "cover"
                       , style "background" "#e2e8f0"
                       , src imageUrl
                       ]
                )
                []

        Nothing ->
            viewNoImageCard baseStyles card.name


type PlayItem
    = KnownPlayCard Action.CardRef
    | UnknownPlayCards Int


collapseUnknowns : List (Maybe Action.CardRef) -> List PlayItem
collapseUnknowns cards =
    List.foldr
        (\maybeCard acc ->
            case maybeCard of
                Just card ->
                    KnownPlayCard card :: acc

                Nothing ->
                    case acc of
                        (UnknownPlayCards n) :: rest ->
                            UnknownPlayCards (n + 1) :: rest

                        _ ->
                            UnknownPlayCards 1 :: acc
        )
        []
        cards


{-| A card-back rectangle with an optional ×N count label.
Width/height should match the card slot for the context it's used in.
-}
viewUnknownCardBack : String -> String -> Bool -> Int -> Html Msg
viewUnknownCardBack w h upsideDown count =
    div
        [ style "width" w
        , style "height" h
        , style "border-radius" "4px"
        , style "flex-shrink" "0"
        , style "box-sizing" "border-box"
        , style "background" "#bfdbfe"
        , style "border" "2px solid #1e40af"
        , style "display" "flex"
        , style "align-items" "center"
        , style "justify-content" "center"
        , style "font-size" "1rem"
        , style "font-weight" "700"
        , style "color" "#1e40af"
        , style "transform"
            (if upsideDown then
                "rotate(180deg)"

             else
                ""
            )
        ]
        (if count > 1 then
            [ span
                [ style "transform"
                    (if upsideDown then
                        "rotate(180deg)"

                     else
                        ""
                    )
                , style "display" "inline-block"
                ]
                [ text ("×" ++ String.fromInt count) ]
            ]

         else
            []
        )


{-| The played-card panel shown to the right of the hand rows when the current
action is a trainer play. Shows the played card and any cards discarded as
part of the effect.
-}
viewCurrentPlay : Replay.Players -> Dict String CardData -> CurrentPlay -> Html Msg
viewCurrentPlay players cache play =
    let
        viewPlayItem item =
            case item of
                KnownPlayCard card ->
                    viewKnownCardThumb cache card

                UnknownPlayCards n ->
                    viewUnknownCardBack "72px" "100px" False n

        labeledGroup label cards =
            div
                [ style "display" "flex"
                , style "flex-direction" "column"
                , style "gap" "0.3rem"
                , style "flex-shrink" "0"
                ]
                [ div
                    [ style "font-size" "0.7rem"
                    , style "font-weight" "600"
                    , style "color" "#718096"
                    ]
                    [ text label ]
                , div
                    [ style "display" "flex"
                    , style "gap" "0.35rem"
                    ]
                    (List.map viewPlayItem (collapseUnknowns cards))
                ]

        optionalGroup label cards =
            if List.isEmpty cards then
                []

            else
                [ labeledGroup label cards ]

        -- One horizontal row for a single player
        -- isTookPrize: use "Prizes taken" label instead of "Drawn" for the drawn cards
        isTookPrize =
            play.card == Nothing

        playerSection playerName rowColor playerCards maybePlayedCard =
            let
                cardGroups =
                    if isTookPrize then
                        optionalGroup "Prizes taken" playerCards.drawn

                    else
                        (case maybePlayedCard of
                            Just card ->
                                [ labeledGroup "Played" [ Just card ] ]

                            Nothing ->
                                []
                        )
                            ++ optionalGroup "Discarded" playerCards.discarded
                            ++ optionalGroup "Shuffled" playerCards.shuffled
                            ++ optionalGroup "Drawn" playerCards.drawn
            in
            if List.isEmpty cardGroups then
                []

            else
                div
                    [ style "font-size" "0.7rem"
                    , style "font-weight" "600"
                    , style "color" rowColor
                    , style "writing-mode" "vertical-rl"
                    , style "transform" "rotate(180deg)"
                    , style "flex-shrink" "0"
                    , style "width" "14px"
                    , style "overflow" "hidden"
                    , style "white-space" "nowrap"
                    , style "align-self" "center"
                    ]
                    [ text playerName ]
                    :: cardGroups

        -- "Played" card only shows in the row of the player who played it
        redPlayedCard =
            if play.player == players.red then
                play.card

            else
                Nothing

        bluePlayedCard =
            if play.player /= players.red then
                play.card

            else
                Nothing

        redSection =
            playerSection "BLUE" "#2c5282" play.red redPlayedCard

        blueSection =
            playerSection "RED" "#c53030" play.blue bluePlayedCard
    in
    div
        [ style "display" "flex"
        , style "flex-direction" "row"
        , style "align-items" "flex-start"
        , style "gap" "0.75rem"
        , style "overflow-x" "auto"
        , style "padding-bottom" "4px"
        ]
        (blueSection ++ redSection)


-- VIEW


view : Model -> Html Msg
view model =
    div
        [ style "font-family" "system-ui, -apple-system, sans-serif"
        , style "max-width" "1400px"
        , style "margin" "0 auto"
        , style "padding" "0.5rem 1.5rem 0"
        , style "color" "#1a202c"
        , style "height" "100%"
        , style "display" "flex"
        , style "flex-direction" "column"
        , style "box-sizing" "border-box"
        ]
        -- Two-column layout: play state (75%) on the left, action log (25%) on the right
        [ div
            [ style "flex" "1"
            , style "min-height" "0"
            , style "display" "flex"
            , style "flex-direction" "row"
            , style "gap" "1.5rem"
            , style "margin-top" "0.5rem"
            ]
            [ -- Left column: URL bar + play state
              div
                [ style "flex" "3"
                , style "min-width" "0"
                , style "display" "flex"
                , style "flex-direction" "column"
                ]
                [ div [ style "flex-shrink" "0", style "margin-bottom" "0.25rem" ] [ viewUrlBar model ]
                , case model of
                    Loaded _ replay sectionIndex groupIndex _ cache flip ->
                        case replay.players of
                            Just players ->
                                let
                                    hand =
                                        computeHand players replay sectionIndex groupIndex

                                    bench =
                                        computeBench players replay sectionIndex groupIndex

                                    activeSpots =
                                        computeActive players replay sectionIndex groupIndex

                                    stadium =
                                        computeStadium players replay sectionIndex groupIndex

                                    piles =
                                        computePiles players replay sectionIndex groupIndex

                                    maybePlay =
                                        getCurrentGroup replay sectionIndex groupIndex
                                            |> Maybe.andThen (currentPlayFromGroup players)
                                in
                                viewHandState players cache flip hand bench activeSpots stadium piles maybePlay

                            Nothing ->
                                text ""

                    _ ->
                        text ""
                ]

            -- Right column: settings + action log
            , div
                [ style "flex" "1"
                , style "min-width" "0"
                , style "min-height" "0"
                , style "display" "flex"
                , style "flex-direction" "column"
                ]
                [ viewSettings model
                , viewContent model
                ]
            ]

        , case model of
            Loaded _ _ _ _ (Just popup) _ _ ->
                viewCardPopup popup

            _ ->
                text ""
        ]


viewSettings : Model -> Html Msg
viewSettings model =
    let
        flip =
            currentFlipOpponent model
    in
    div
        [ style "flex-shrink" "0"
        , style "padding" "0.4rem 0"
        , style "display" "flex"
        , style "flex-direction" "column"
        , style "gap" "0.35rem"
        ]
        [ div
            [ style "display" "flex"
            , style "align-items" "center"
            , style "gap" "0.5rem"
            , style "cursor" "pointer"
            , onClick FlipOpponentToggled
            ]
            [ -- Toggle switch
              div
                [ style "width" "32px"
                , style "height" "18px"
                , style "border-radius" "9px"
                , style "background"
                    (if flip then
                        "#4a5568"

                     else
                        "#cbd5e0"
                    )
                , style "position" "relative"
                , style "flex-shrink" "0"
                , style "transition" "background 0.15s"
                ]
                [ div
                    [ style "width" "12px"
                    , style "height" "12px"
                    , style "border-radius" "50%"
                    , style "background" "white"
                    , style "position" "absolute"
                    , style "top" "3px"
                    , style "left"
                        (if flip then
                            "17px"

                         else
                            "3px"
                        )
                    , style "transition" "left 0.15s"
                    ]
                    []
                ]
            , span
                [ style "font-size" "0.75rem"
                , style "color" "#4a5568"
                , style "user-select" "none"
                ]
                [ text "Flip opponent's cards" ]
            ]
        ]


viewUrlBar : Model -> Html Msg
viewUrlBar model =
    let
        url =
            currentUrl model

        isLoading =
            case model of
                Loading _ _ _ _ ->
                    True

                Retrying _ _ _ _ ->
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
    div [ style "margin-bottom" "0.25rem" ]
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

        Loading _ _ _ _ ->
            div
                [ style "color" "#718096"
                , style "font-style" "italic"
                ]
                [ text "Loading replay…" ]

        Retrying _ _ _ _ ->
            div
                [ style "color" "#718096"
                , style "font-style" "italic"
                ]
                [ text "Loading replay…" ]

        Loaded _ replay index groupIndex _ cache _ ->
            viewReplay cache replay index groupIndex

        Failed _ _ ->
            text ""


sectionGroupCount : Replay.Section -> Int
sectionGroupCount section =
    case section of
        Replay.SetupSection lines ->
            List.length (Action.groupLines lines)

        Replay.TurnSection _ lines ->
            List.length (Action.groupLines lines)

        Replay.CheckupSection lines ->
            List.length (Action.groupLines lines)

        Replay.ResultSection _ ->
            1


viewReplay : Dict String CardData -> Replay.Replay -> Int -> Int -> Html Msg
viewReplay cache replay sectionIndex groupIndex =
    let
        players =
            replay.players

        total =
            List.length replay.sections

        currentSection =
            replay.sections |> List.drop sectionIndex |> List.head

        pastSections =
            List.take sectionIndex replay.sections

        -- Nav bar info for the current section
        ( badge, extra, borderColor ) =
            sectionNavInfo players currentSection

        -- Current section: visible groups reversed (most recent at top)
        currentContent =
            case currentSection of
                Nothing ->
                    []

                Just section ->
                    case section of
                        Replay.ResultSection result ->
                            [ viewResultContent players result ]

                        _ ->
                            let
                                groups =
                                    Action.groupLines (sectionLines section)

                                totalGroups =
                                    List.length groups
                            in
                            List.take (groupIndex + 1) groups
                                |> List.reverse
                                |> List.indexedMap
                                    (\i group ->
                                        div
                                            (if i > 0 then
                                                [ style "opacity" "0.4" ]

                                             else
                                                []
                                            )
                                            (viewActionGroup players cache group)
                                    )

        -- Past sections: most recent first, each preceded by a divider
        pastContent =
            pastSections
                |> List.reverse
                |> List.concatMap
                    (\section ->
                        viewSectionDivider players section
                            :: viewPastSectionGroups cache players section
                    )

        totalGroupsInCurrent =
            currentSection |> Maybe.map sectionGroupCount |> Maybe.withDefault 0

        hasPrev =
            groupIndex > 0 || sectionIndex > 0

        hasNext =
            groupIndex < totalGroupsInCurrent - 1 || sectionIndex < total - 1
    in
    viewNavSection
        { badge = badge
        , extra = extra
        , borderColor = borderColor
        , content = currentContent ++ pastContent
        , hasPrev = hasPrev
        , hasNext = hasNext
        }


playerColor : Maybe Replay.Players -> String -> String
playerColor players name =
    case players of
        Just p ->
            if name == p.red then
                "#2c5282"

            else if name == p.blue then
                "#c53030"

            else
                "#2d3748"

        Nothing ->
            "#2d3748"


sectionLines : Replay.Section -> List Replay.ReplayLine
sectionLines section =
    case section of
        Replay.SetupSection lines ->
            lines

        Replay.TurnSection _ lines ->
            lines

        Replay.CheckupSection lines ->
            lines

        Replay.ResultSection _ ->
            []


sectionNavInfo : Maybe Replay.Players -> Maybe Replay.Section -> ( Html Msg, List (Html Msg), String )
sectionNavInfo players maybeSection =
    case maybeSection of
        Nothing ->
            ( text "", [], "#71809640" )

        Just section ->
            case section of
                Replay.SetupSection _ ->
                    ( viewSectionBadge "#718096" "Setup", [], "#71809640" )

                Replay.TurnSection turn _ ->
                    let
                        badgeColor =
                            playerColor players turn.player
                    in
                    ( viewSectionBadge badgeColor ("Turn " ++ String.fromInt turn.number)
                    , [ span
                            [ style "font-weight" "600"
                            , style "color" "#4a5568"
                            , style "font-size" "0.95rem"
                            ]
                            [ text turn.player ]
                      ]
                    , badgeColor ++ "40"
                    )

                Replay.CheckupSection _ ->
                    ( viewSectionBadge "#b7791f" "Pokémon Checkup", [], "#b7791f40" )

                Replay.ResultSection _ ->
                    ( viewSectionBadge "#718096" "Result", [], "#71809640" )


viewResultContent : Maybe Replay.Players -> Replay.MatchResult -> Html Msg
viewResultContent players result =
    div []
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
            , style "color" (playerColor players result.winner)
            , style "padding" "0.2rem 0"
            ]
            [ text (result.winner ++ " wins.") ]
        ]


viewPastSectionGroups : Dict String CardData -> Maybe Replay.Players -> Replay.Section -> List (Html Msg)
viewPastSectionGroups cache players section =
    let
        greyed children =
            div [ style "opacity" "0.4" ] children
    in
    case section of
        Replay.ResultSection result ->
            [ greyed [ viewResultContent players result ] ]

        _ ->
            Action.groupLines (sectionLines section)
                |> List.reverse
                |> List.map (\group -> greyed (viewActionGroup players cache group))


viewSectionDivider : Maybe Replay.Players -> Replay.Section -> Html Msg
viewSectionDivider players section =
    let
        ( label, color ) =
            case section of
                Replay.SetupSection _ ->
                    ( "Setup", "#718096" )

                Replay.TurnSection turn _ ->
                    ( "Turn " ++ String.fromInt turn.number ++ " · " ++ turn.player
                    , playerColor players turn.player
                    )

                Replay.CheckupSection _ ->
                    ( "Pokémon Checkup", "#b7791f" )

                Replay.ResultSection _ ->
                    ( "Result", "#718096" )
    in
    div
        [ style "display" "flex"
        , style "align-items" "center"
        , style "gap" "0.5rem"
        , style "margin" "0.75rem 0"
        , style "opacity" "0.45"
        ]
        [ div [ style "flex" "1", style "height" "1px", style "background" "#cbd5e0" ] []
        , span
            [ style "font-size" "0.7rem"
            , style "font-weight" "600"
            , style "color" color
            , style "white-space" "nowrap"
            , style "letter-spacing" "0.05em"
            , style "text-transform" "uppercase"
            ]
            [ text label ]
        , div [ style "flex" "1", style "height" "1px", style "background" "#cbd5e0" ] []
        ]


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
    div
        [ style "display" "flex"
        , style "flex-direction" "column"
        , style "flex" "1"
        , style "min-height" "0"
        ]
        [ div
            [ style "display" "flex"
            , style "justify-content" "space-between"
            , style "align-items" "center"
            , style "margin-bottom" "0.5rem"
            , style "flex-shrink" "0"
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
            [ id "action-content"
            , style "border-left" ("3px solid " ++ borderColor)
            , style "padding-left" "0.75rem"
            , style "overflow-y" "auto"
            , style "flex" "1"
            , style "min-height" "0"
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


viewActionGroup : Maybe Replay.Players -> Dict String CardData -> Action.ActionGroup -> List (Html Msg)
viewActionGroup players cache group =
    let
        correctedGroup =
            case players of
                Just ps ->
                    correctGroupPlayers ps group

                Nothing ->
                    group

        topHighlight =
            case group.action of
                Action.UsedAttack { attacker, move } ->
                    let
                        cleaned =
                            let
                                trimmed =
                                    String.trim move
                            in
                            if String.endsWith "." trimmed then
                                String.dropRight 1 trimmed

                            else
                                trimmed

                        cardId =
                            attacker.card.id

                        kind =
                            case Dict.get cardId cache of
                                Just cardData ->
                                    if List.any (\a -> a.name == cleaned) cardData.abilities then
                                        Just IsAbility

                                    else if List.any (\a -> a.name == cleaned) cardData.attacks then
                                        Just IsAttack

                                    else
                                        Nothing

                                Nothing ->
                                    Nothing
                    in
                    Just { phrase = cleaned, kind = kind, cardId = cardId }

                _ ->
                    Nothing

        normalDetails =
            List.concatMap
                (\detail ->
                    viewLine players Nothing (Replay.DetailLine detail.raw)
                        :: List.map (\bullet -> viewLine players Nothing (Replay.BulletLine bullet.raw)) detail.bullets
                )
                correctedGroup.details
    in
    case group.action of
        Action.UsedAttack { target, modifier } ->
            case target of
                Just { damage } ->
                    if modifier /= Nothing || (List.head correctedGroup.details |> Maybe.map (\d -> d.raw == "Damage breakdown:") |> Maybe.withDefault False) then
                        let
                            -- Split the raw line at "for X damage." to isolate the modifier sentence
                            forDamage =
                                " for " ++ String.fromInt damage ++ " damage."

                            prefixIdx =
                                String.indexes forDamage group.raw |> List.head

                            attackPrefixRaw =
                                case prefixIdx of
                                    Just i ->
                                        String.left i group.raw

                                    Nothing ->
                                        group.raw

                            breakdownLines =
                                correctedGroup.details
                                    |> List.filter (\d -> d.raw == "Damage breakdown:")
                                    |> List.concatMap (\detail -> detail.raw :: List.map .raw detail.bullets)

                            damageInfo =
                                { breakdownLines = breakdownLines }

                            nonBreakdownDetails =
                                correctedGroup.details
                                    |> List.filter (\d -> d.raw /= "Damage breakdown:")
                                    |> List.concatMap
                                        (\detail ->
                                            viewLine players Nothing (Replay.DetailLine detail.raw)
                                                :: List.map (\bullet -> viewLine players Nothing (Replay.BulletLine bullet.raw)) detail.bullets
                                        )
                        in
                        div
                            [ style "padding" "0.2rem 0"
                            , style "font-size" "0.9rem"
                            , style "color" "#2d3748"
                            , style "line-height" "1.5"
                            ]
                            (viewInlineText players topHighlight attackPrefixRaw
                                ++ [ text " for "
                                   , viewDamageChip damage damageInfo
                                   ]
                            )
                            :: nonBreakdownDetails

                    else
                        viewLine players topHighlight (Replay.TopLine group.raw) :: normalDetails

                Nothing ->
                    viewLine players topHighlight (Replay.TopLine group.raw) :: normalDetails

        _ ->
            viewLine players topHighlight (Replay.TopLine group.raw) :: normalDetails


viewDamageChip : Int -> DamageInfo -> Html Msg
viewDamageChip damage info =
    span
        [ onClick (DamageClicked info)
        , style "cursor" "pointer"
        , style "font-size" "0.8em"
        , style "font-weight" "600"
        , style "background" "#fed7d7"
        , style "color" "#9b2c2c"
        , style "padding" "0.1em 0.45em"
        , style "border-radius" "999px"
        , style "white-space" "nowrap"
        ]
        [ text (String.fromInt damage ++ " damage") ]


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


viewLine : Maybe Replay.Players -> Maybe MoveHighlight -> Replay.ReplayLine -> Html Msg
viewLine players highlight line =
    case line of
        Replay.TopLine content ->
            div
                [ style "padding" "0.2rem 0"
                , style "font-size" "0.9rem"
                , style "color" "#2d3748"
                , style "line-height" "1.5"
                ]
                (viewInlineText players highlight content)

        Replay.DetailLine content ->
            div
                [ style "padding" "0.15rem 0 0.15rem 1.25rem"
                , style "font-size" "0.875rem"
                , style "color" "#4a5568"
                , style "line-height" "1.5"
                ]
                (viewInlineText players highlight content)

        Replay.BulletLine content ->
            div
                [ style "padding" "0.1rem 0 0.1rem 2.5rem"
                , style "font-size" "0.85rem"
                , style "color" "#718096"
                , style "line-height" "1.4"
                ]
                (viewInlineText players highlight content)



-- INLINE CARD REFERENCE PARSING


type TextSegment
    = PlainText String
    | CardRef String String (Maybe String) -- id, name, optional player color
    | PlayerRef String String -- player name, css color
    | MoveRef String (Maybe MoveKind) String -- name, kind, cardId


viewInlineText : Maybe Replay.Players -> Maybe MoveHighlight -> String -> List (Html Msg)
viewInlineText players highlight str =
    segmentText players str
        |> applyHighlights highlight
        |> List.map viewSegment


viewSegment : TextSegment -> Html Msg
viewSegment seg =
    case seg of
        PlainText str ->
            text str

        CardRef id name maybeColor ->
            span
                [ onClick (CardClicked id)
                , style "font-size" "0.8em"
                , style "font-weight" "600"
                , style "background" (Maybe.withDefault "#e2e8f0" maybeColor)
                , style "color"
                    (if maybeColor == Nothing then
                        "#4a5568"

                     else
                        "white"
                    )
                , style "padding" "0.1em 0.45em"
                , style "border-radius" "999px"
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

        MoveRef name kind cardId ->
            span
                [ onClick (MoveClicked cardId name)
                , style "cursor" "pointer"
                , style "font-size" "0.8em"
                , style "font-weight" "600"
                , style "padding" "0.1em 0.45em"
                , style "border-radius" "999px"
                , style "white-space" "nowrap"
                , case kind of
                    Just IsAbility ->
                        style "background" "#9f7aea"

                    Just IsAttack ->
                        style "background" "#ed8936"

                    Nothing ->
                        style "background" "#e2e8f0"
                , case kind of
                    Nothing ->
                        style "color" "#4a5568"

                    _ ->
                        style "color" "white"
                ]
                [ text name ]

        PlayerRef name color ->
            span
                [ style "background" color
                , style "color" "white"
                , style "font-size" "0.78em"
                , style "font-weight" "700"
                , style "padding" "0.1em 0.45em"
                , style "border-radius" "999px"
                , style "white-space" "nowrap"
                , style "vertical-align" "middle"
                ]
                [ text name ]


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

            FetchingMove _ _ ->
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

            ShowingCard _ cardData ->
                case cardData.imageUrl of
                    Just imageUrl ->
                        img
                            [ src (imageUrl ++ "/high.webp")
                            , style "max-height" "80vh"
                            , style "max-width" "90vw"
                            , style "border-radius" "8px"
                            , style "box-shadow" "0 8px 32px rgba(0,0,0,0.5)"
                            , style "display" "block"
                            ]
                            []

                    Nothing ->
                        text ""

            ShowingMove cardData moveName ->
                viewMoveDetail cardData moveName

            ShowingDamageInfo info ->
                viewDamageDetail info
        ]


viewMoveDetail : CardData -> String -> Html Msg
viewMoveDetail cardData moveName =
    let
        maybeAbility =
            List.head (List.filter (\a -> a.name == moveName) cardData.abilities)

        maybeAttack =
            List.head (List.filter (\a -> a.name == moveName) cardData.attacks)
    in
    div
        [ style "background" "white"
        , style "border-radius" "12px"
        , style "padding" "1.5rem 2rem"
        , style "max-width" "380px"
        , style "width" "90vw"
        , style "cursor" "default"
        , style "box-shadow" "0 8px 32px rgba(0,0,0,0.4)"
        ]
        [ case maybeAbility of
            Just ability ->
                viewAbilityDetail ability

            Nothing ->
                case maybeAttack of
                    Just attack ->
                        viewAttackDetail attack

                    Nothing ->
                        div [ style "font-size" "1rem", style "color" "#2d3748" ]
                            [ text moveName ]
        ]


viewAbilityDetail : CardAbility -> Html Msg
viewAbilityDetail ability =
    div []
        [ div
            [ style "display" "flex"
            , style "align-items" "center"
            , style "gap" "0.5rem"
            , style "margin-bottom" "0.75rem"
            ]
            [ span
                [ style "background" "#9f7aea"
                , style "color" "white"
                , style "font-size" "0.7rem"
                , style "font-weight" "700"
                , style "letter-spacing" "0.06em"
                , style "text-transform" "uppercase"
                , style "padding" "0.2rem 0.5rem"
                , style "border-radius" "4px"
                ]
                [ text ability.abilityType ]
            , span
                [ style "font-size" "1rem"
                , style "font-weight" "700"
                , style "color" "#2d3748"
                ]
                [ text ability.name ]
            ]
        , p
            [ style "font-size" "0.875rem"
            , style "color" "#4a5568"
            , style "line-height" "1.6"
            , style "margin" "0"
            ]
            [ text ability.effect ]
        ]


viewAttackDetail : CardAttack -> Html Msg
viewAttackDetail attack =
    div []
        [ div
            [ style "display" "flex"
            , style "align-items" "center"
            , style "justify-content" "space-between"
            , style "margin-bottom" "0.5rem"
            ]
            [ span
                [ style "font-size" "1rem"
                , style "font-weight" "700"
                , style "color" "#2d3748"
                ]
                [ text attack.name ]
            , if String.isEmpty attack.damage then
                text ""

              else
                span
                    [ style "font-size" "1.1rem"
                    , style "font-weight" "800"
                    , style "color" "#2d3748"
                    ]
                    [ text (attack.damage ++ " dmg") ]
            ]
        , if List.isEmpty attack.cost then
            text ""

          else
            div
                [ style "display" "flex"
                , style "gap" "0.3rem"
                , style "flex-wrap" "wrap"
                , style "margin-bottom" "0.6rem"
                ]
                (List.map viewEnergyCost attack.cost)
        , if String.isEmpty attack.effect then
            text ""

          else
            p
                [ style "font-size" "0.875rem"
                , style "color" "#4a5568"
                , style "line-height" "1.6"
                , style "margin" "0"
                ]
                [ text attack.effect ]
        ]


viewEnergyCost : String -> Html Msg
viewEnergyCost energyType =
    span
        [ style "font-size" "0.72em"
        , style "font-weight" "600"
        , style "background" "#e2e8f0"
        , style "color" "#4a5568"
        , style "padding" "0.15em 0.4em"
        , style "border-radius" "999px"
        , style "white-space" "nowrap"
        ]
        [ text energyType ]


viewDamageDetail : DamageInfo -> Html Msg
viewDamageDetail info =
    div
        [ style "background" "white"
        , style "border-radius" "12px"
        , style "padding" "1.25rem 1.75rem"
        , style "max-width" "360px"
        , style "width" "90vw"
        , style "cursor" "default"
        , style "box-shadow" "0 8px 32px rgba(0,0,0,0.4)"
        ]
        [ div []
            (List.map
                (\line ->
                    div
                        [ style "font-size" "0.875rem"
                        , style "color" "#2d3748"
                        , style "padding" "0.15rem 0"
                        , style "line-height" "1.5"
                        ]
                        [ text line ]
                )
                info.breakdownLines
            )
        ]


segmentText : Maybe Replay.Players -> String -> List TextSegment
segmentText players str =
    case String.split "(" str of
        [] ->
            []

        first :: rest ->
            (segmentPlayers players first ++ List.concatMap (parseParen players) rest)
                |> colorPokemonPills


{-| After segmentation, any CardRef that immediately follows PlayerRef + "'s " gets
the player's color so it renders as a tinted Pokémon pill instead of plain gray.
-}
colorPokemonPills : List TextSegment -> List TextSegment
colorPokemonPills segs =
    case segs of
        [] ->
            []

        (PlayerRef name color) :: rest ->
            case rest of
                (PlainText possessive) :: (CardRef id cname Nothing) :: further ->
                    if String.endsWith "'s " possessive then
                        PlayerRef name color
                            :: PlainText possessive
                            :: CardRef id cname (Just color)
                            :: colorPokemonPills further

                    else
                        PlayerRef name color :: colorPokemonPills rest

                _ ->
                    PlayerRef name color :: colorPokemonPills rest

        seg :: rest ->
            seg :: colorPokemonPills rest


parseParen : Maybe Replay.Players -> String -> List TextSegment
parseParen players str =
    case String.split ")" str of
        id :: remainderParts ->
            if isCardId id then
                let
                    remainder =
                        String.join ")" remainderParts

                    ( name, rest ) =
                        extractCardName remainder
                in
                CardRef id name Nothing :: segmentPlayers players rest

            else
                segmentPlayers players ("(" ++ str)

        [] ->
            segmentPlayers players ("(" ++ str)


segmentPlayers : Maybe Replay.Players -> String -> List TextSegment
segmentPlayers maybePlayers str =
    case maybePlayers of
        Nothing ->
            if String.isEmpty str then
                []

            else
                [ PlainText str ]

        Just players ->
            splitByPlayer players.red "#2c5282" str
                |> List.concatMap
                    (\seg ->
                        case seg of
                            PlainText s ->
                                splitByPlayer players.blue "#c53030" s

                            other ->
                                [ other ]
                    )


splitByPlayer : String -> String -> String -> List TextSegment
splitByPlayer playerName color str =
    if String.isEmpty playerName then
        if String.isEmpty str then
            []

        else
            [ PlainText str ]

    else
        case String.split playerName str of
            [] ->
                []

            [ only ] ->
                if String.isEmpty only then
                    []

                else
                    [ PlainText only ]

            parts ->
                let
                    interleave ps =
                        case ps of
                            [] ->
                                []

                            [ last ] ->
                                if String.isEmpty last then
                                    []

                                else
                                    [ PlainText last ]

                            first :: rest ->
                                (if String.isEmpty first then
                                    []

                                 else
                                    [ PlainText first ]
                                )
                                    ++ (PlayerRef playerName color :: interleave rest)
                in
                interleave parts


applyHighlights : Maybe MoveHighlight -> List TextSegment -> List TextSegment
applyHighlights maybeHighlight segs =
    case maybeHighlight of
        Nothing ->
            segs

        Just highlight ->
            List.concatMap
                (\seg ->
                    case seg of
                        PlainText str ->
                            splitByPhrase highlight str

                        other ->
                            [ other ]
                )
                segs


splitByPhrase : MoveHighlight -> String -> List TextSegment
splitByPhrase highlight str =
    let
        phrase =
            highlight.phrase
    in
    if String.isEmpty phrase then
        if String.isEmpty str then
            []

        else
            [ PlainText str ]

    else
        case String.split phrase str of
            [] ->
                []

            [ only ] ->
                if String.isEmpty only then
                    []

                else
                    [ PlainText only ]

            parts ->
                let
                    interleave ps =
                        case ps of
                            [] ->
                                []

                            [ last ] ->
                                if String.isEmpty last then
                                    []

                                else
                                    [ PlainText last ]

                            first :: rest ->
                                (if String.isEmpty first then
                                    []

                                 else
                                    [ PlainText first ]
                                )
                                    ++ (MoveRef phrase highlight.kind highlight.cardId :: interleave rest)
                in
                interleave parts


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
