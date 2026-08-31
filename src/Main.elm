port module Main exposing (CardAttack, CardAbility, CardData, MoveKind(..), MoveHighlight, CardPopup(..), Model(..), Msg(..), HandState, emptyHand, applyGroupToHand, BenchState, emptyBench, applyGroupToBench, ActiveState, emptyActive, applyGroupToActive, PileState, emptyPiles, applyGroupToPiles, StadiumState, applyGroupToStadium, InstanceId, InstanceState, emptyInstances, applyGroupToInstances, instanceIdForField, firstInstance, AttachmentState, emptyAttachments, applyGroupToAttachments, lookupAttachments, correctGroupPlayers, isPokemonAbilityGroup, pokemonAbilityPlayedCardId, sectionLines, CurrentPlay, currentPlayFromGroup, init, main, update)

import Browser
import Browser.Dom
import Browser.Events
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


init : { replayUrl : String, sectionIndex : Int, groupIndex : Int, flipOpponent : Bool, debug : Bool } -> ( Model, Cmd Msg )
init flags =
    let
        url =
            String.trim flags.replayUrl
    in
    if String.isEmpty url then
        ( EnteringUrl "", Cmd.none )

    else
        let
            ctx =
                { flipOpponent = flags.flipOpponent, debug = flags.debug }
        in
        case trainingCourtLogId url of
            Just uuid ->
                ( Loading url flags.sectionIndex flags.groupIndex ctx
                , fetchTrainingCourtLog uuid flags.sectionIndex flags.groupIndex flags.flipOpponent
                )

            Nothing ->
                ( Loading url flags.sectionIndex flags.groupIndex ctx
                , Http.get { url = url, expect = Http.expectString GotReplay }
                )


main : Program { replayUrl : String, sectionIndex : Int, groupIndex : Int, flipOpponent : Bool, debug : Bool } Model Msg
main =
    Browser.element
        { init = init
        , update = update
        , view = view
        , subscriptions =
            \_ ->
                Sub.batch
                    [ onSwipe GotSwipe
                    , Browser.Events.onKeyDown
                        (Decode.map KeyDown (Decode.field "key" Decode.string))
                    ]
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
    , category : Maybe String
    , name : Maybe String
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
    , benched : List (Maybe Action.CardRef)
    }


emptyPlayerCards : PlayerCards
emptyPlayerCards =
    { discarded = [], shuffled = [], drawn = [], benched = [] }


type alias CurrentPlay =
    { player : String
    -- Nothing = prize-taking action (no single card played)
    , card : Maybe Action.CardRef
    -- per-player card buckets (red = local recorder, blue = opponent)
    , red : PlayerCards
    , blue : PlayerCards
    }


type CardPopup
    = FetchingCard String String
    | FetchingMove String String
    | ShowingCard String CardData
    | ShowingMove CardData String
    | ShowingDamageInfo DamageInfo
    | CardNotFound String


type alias ViewContext =
    { flipOpponent : Bool
    , debug : Bool
    }


type Model
    = EnteringUrl String
    | Loading String Int Int ViewContext
    | Retrying String Int Int ViewContext
    | Loaded String Replay.Replay Int Int (Maybe CardPopup) (Dict String CardData) ViewContext
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
        Loading _ _ _ ctx ->
            ctx.flipOpponent

        Retrying _ _ _ ctx ->
            ctx.flipOpponent

        Loaded _ _ _ _ _ _ ctx ->
            ctx.flipOpponent

        _ ->
            False


currentDebug : Model -> Bool
currentDebug model =
    case model of
        Loading _ _ _ ctx ->
            ctx.debug

        Retrying _ _ _ ctx ->
            ctx.debug

        Loaded _ _ _ _ _ _ ctx ->
            ctx.debug

        _ ->
            False



-- UPDATE


type Msg
    = UrlChanged String
    | LoadClicked
    | GotReplay (Result Http.Error String)
    | GotTrainingCourtLog Int Int Bool (Result Http.Error String)
    | FirstSection
    | PrevSection
    | NextSection
    | LastSection
    | GotSwipe String
    | KeyDown String
    | CardClicked String String
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

                flip =
                    currentFlipOpponent model

                debug =
                    currentDebug model
            in
            if String.isEmpty url then
                ( model, Cmd.none )

            else
                let
                    ctx =
                        { flipOpponent = flip, debug = debug }
                in
                case trainingCourtLogId url of
                    Just uuid ->
                        ( Loading url 0 0 ctx
                        , fetchTrainingCourtLog uuid 0 0 flip
                        )

                    Nothing ->
                        ( Loading url 0 0 ctx
                        , Http.get { url = url, expect = Http.expectString GotReplay }
                        )

        GotReplay result ->
            case model of
                Loading url idx gIdx ctx ->
                    case result of
                        Ok content ->
                            loadReplay url idx gIdx ctx content

                        Err Http.NetworkError ->
                            ( Retrying url idx gIdx ctx
                            , Http.get { url = proxyUrl url, expect = Http.expectString GotReplay }
                            )

                        Err err ->
                            ( Failed url (httpErrorToString err), Cmd.none )

                Retrying url idx gIdx ctx ->
                    case result of
                        Ok content ->
                            loadReplay url idx gIdx ctx content

                        Err err ->
                            ( Failed url (httpErrorToString err), Cmd.none )

                _ ->
                    ( model, Cmd.none )

        GotTrainingCourtLog idx gIdx flip result ->
            case model of
                Loading url _ _ ctx ->
                    case result of
                        Ok content ->
                            loadReplay url idx gIdx ctx content

                        Err err ->
                            ( Failed url (httpErrorToString err), Cmd.none )

                _ ->
                    ( model, Cmd.none )

        FirstSection ->
            case model of
                Loaded url replay _ _ _ cache ctx ->
                    ( Loaded url replay 0 0 Nothing cache ctx
                    , Cmd.batch
                        [ pushUrl { url = url, index = 0, groupIndex = 0, flipOpponent = ctx.flipOpponent }
                        , scrollToTop
                        , fetchHandCards replay.players replay 0 0 cache
                        ]
                    )

                _ ->
                    ( model, Cmd.none )

        PrevSection ->
            case model of
                Loaded url replay i g _ cache ctx ->
                    if g > 0 then
                        ( Loaded url replay i (g - 1) Nothing cache ctx
                        , Cmd.batch
                            [ pushUrl { url = url, index = i, groupIndex = g - 1, flipOpponent = ctx.flipOpponent }
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
                        ( Loaded url replay newI newG Nothing cache ctx
                        , Cmd.batch
                            [ pushUrl { url = url, index = newI, groupIndex = newG, flipOpponent = ctx.flipOpponent }
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
                Loaded url replay i g _ cache ctx ->
                    let
                        currentSection =
                            replay.sections |> List.drop i |> List.head

                        totalGroups =
                            currentSection |> Maybe.map sectionGroupCount |> Maybe.withDefault 1

                        totalSections =
                            List.length replay.sections
                    in
                    if g < totalGroups - 1 then
                        ( Loaded url replay i (g + 1) Nothing cache ctx
                        , Cmd.batch
                            [ pushUrl { url = url, index = i, groupIndex = g + 1, flipOpponent = ctx.flipOpponent }
                            , scrollToTop
                            , fetchHandCards replay.players replay i (g + 1) cache
                            ]
                        )

                    else if i < totalSections - 1 then
                        ( Loaded url replay (i + 1) 0 Nothing cache ctx
                        , Cmd.batch
                            [ pushUrl { url = url, index = i + 1, groupIndex = 0, flipOpponent = ctx.flipOpponent }
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
                Loaded url replay _ _ _ cache ctx ->
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
                    ( Loaded url replay lastI lastG Nothing cache ctx
                    , Cmd.batch
                        [ pushUrl { url = url, index = lastI, groupIndex = lastG, flipOpponent = ctx.flipOpponent }
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

        KeyDown key ->
            case key of
                "ArrowLeft" ->
                    update PrevSection model

                "ArrowRight" ->
                    update NextSection model

                _ ->
                    ( model, Cmd.none )

        CardClicked id fallbackName ->
            case model of
                Loaded url replay i g _ cache ctx ->
                    case Dict.get id cache of
                        Just cardData ->
                            ( Loaded url replay i g (Just (ShowingCard id cardData)) cache ctx, Cmd.none )

                        Nothing ->
                            case cardApiUrl id of
                                Just apiUrl ->
                                    ( Loaded url replay i g (Just (FetchingCard id fallbackName)) cache ctx
                                    , Http.get
                                        { url = apiUrl
                                        , expect = Http.expectString (GotCardImage id)
                                        }
                                    )

                                Nothing ->
                                    ( Loaded url replay i g (Just (CardNotFound id)) cache ctx, Cmd.none )

                _ ->
                    ( model, Cmd.none )

        MoveClicked cardId moveName ->
            case model of
                Loaded url replay i g _ cache ctx ->
                    case Dict.get cardId cache of
                        Just cardData ->
                            ( Loaded url replay i g (Just (ShowingMove cardData moveName)) cache ctx, Cmd.none )

                        Nothing ->
                            case cardApiUrl cardId of
                                Just apiUrl ->
                                    ( Loaded url replay i g (Just (FetchingMove cardId moveName)) cache ctx
                                    , Http.get { url = apiUrl, expect = Http.expectString (GotCardImage cardId) }
                                    )

                                Nothing ->
                                    ( Loaded url replay i g (Just (CardNotFound cardId)) cache ctx, Cmd.none )

                _ ->
                    ( model, Cmd.none )

        DamageClicked info ->
            case model of
                Loaded url replay i g _ cache ctx ->
                    ( Loaded url replay i g (Just (ShowingDamageInfo info)) cache ctx, Cmd.none )

                _ ->
                    ( model, Cmd.none )

        GotCardImage id result ->
            case model of
                Loaded url replay i g currentPopup cache ctx ->
                    let
                        -- True only when the user explicitly requested this card
                        -- (by clicking a pill / card thumbnail), so we should open a popup.
                        -- Background hand-prefetch requests must NOT disturb the popup state.
                        isUserFetch =
                            case currentPopup of
                                Just (FetchingCard fetchId _) ->
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
                    ( Loaded url replay i g nextPopup newCache ctx, Cmd.none )

                _ ->
                    ( model, Cmd.none )

        CloseCard ->
            case model of
                Loaded url replay i g _ cache ctx ->
                    ( Loaded url replay i g Nothing cache ctx, Cmd.none )

                _ ->
                    ( model, Cmd.none )

        FlipOpponentToggled ->
            case model of
                Loaded url replay i g popup cache ctx ->
                    let
                        newFlip =
                            not ctx.flipOpponent
                    in
                    ( Loaded url replay i g popup cache { ctx | flipOpponent = newFlip }
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


loadReplay : String -> Int -> Int -> ViewContext -> String -> ( Model, Cmd Msg )
loadReplay url requestedIndex requestedGroupIndex ctx content =
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
        ( Loaded url replay index groupIndex Nothing Dict.empty ctx
        , Cmd.batch
            [ pushUrl { url = url, index = index, groupIndex = groupIndex, flipOpponent = ctx.flipOpponent }
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

                attachmentRefs =
                    computeAttachments players replay si gi
                        |> List.concatMap .items

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
                                            ++ List.filterMap identity pc.benched
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
                    (handRefs ++ benchRefs ++ activeRefs ++ stadiumRef ++ attachmentRefs ++ playRefs)
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


-- TrainingCourt stores logs in Supabase. The anon key is public by design
-- (exposed in their open-source repo and compiled client JS).
trainingCourtSupabaseUrl : String
trainingCourtSupabaseUrl =
    "https://yuruvpbgsukqiaeduaay.supabase.co"


trainingCourtAnonKey : String
trainingCourtAnonKey =
    "eyJhbGciOiJIUzI1NiIsInR5cCI6IkpXVCJ9.eyJpc3MiOiJzdXBhYmFzZSIsInJlZiI6Inl1cnV2cGJnc3VrcWlhZWR1YWF5Iiwicm9sZSI6ImFub24iLCJpYXQiOjE3MjM2NDA2MDcsImV4cCI6MjAzOTIxNjYwN30.GtRRwMpiMMmbcpUci9xXqthWhgL5daKvsUZUaRgFPkI"


{-| Extract the log UUID from a TrainingCourt share URL, e.g.
"https://trainingcourt.app/ptcg/logs/7b9b2ec2-..." → Just "7b9b2ec2-..."
-}
trainingCourtLogId : String -> Maybe String
trainingCourtLogId url =
    case String.split "trainingcourt.app/ptcg/logs/" url of
        [ _, rest ] ->
            let
                uuid =
                    rest |> String.split "?" |> List.head |> Maybe.withDefault ""
            in
            if String.isEmpty uuid then
                Nothing

            else
                Just uuid

        _ ->
            Nothing


fetchTrainingCourtLog : String -> Int -> Int -> Bool -> Cmd Msg
fetchTrainingCourtLog uuid idx gIdx flip =
    Http.request
        { method = "GET"
        , headers =
            [ Http.header "apikey" trainingCourtAnonKey
            , Http.header "Authorization" ("Bearer " ++ trainingCourtAnonKey)
            ]
        , url =
            trainingCourtSupabaseUrl
                ++ "/rest/v1/logs?select=log&id=eq."
                ++ uuid
        , body = Http.emptyBody
        , expect =
            Http.expectJson
                (GotTrainingCourtLog idx gIdx flip)
                (Decode.index 0 (Decode.field "log" Decode.string))
        , timeout = Nothing
        , tracker = Nothing
        }


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


basicEnergyColor : String -> Maybe String
basicEnergyColor apiName =
    let
        key =
            if String.startsWith "Basic " apiName then
                String.dropLeft 6 apiName

            else
                apiName
    in
    case key of
        "Grass Energy" ->
            Just "#4ade80"

        "Water Energy" ->
            Just "#60a5fa"

        "Fire Energy" ->
            Just "#f87171"

        "Lightning Energy" ->
            Just "#facc15"

        "Fighting Energy" ->
            Just "#fb923c"

        "Psychic Energy" ->
            Just "#c084fc"

        "Darkness Energy" ->
            Just "#818cf8"

        "Metal Energy" ->
            Just "#94a3b8"

        _ ->
            Nothing


abbreviateCardName : String -> String
abbreviateCardName name =
    name
        |> String.words
        |> List.filterMap (String.uncons >> Maybe.map (Tuple.first >> String.fromChar))
        |> String.concat
        |> String.left 3
        |> String.toUpper


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
            Decode.map5 CardData
                (Decode.maybe (Decode.field "image" Decode.string))
                (Decode.oneOf
                    [ Decode.field "attacks" (Decode.list attackDecoder)
                    , Decode.succeed []
                    ])
                (Decode.oneOf
                    [ Decode.field "abilities" (Decode.list abilityDecoder)
                    , Decode.succeed []
                    ])
                (Decode.maybe (Decode.field "category" Decode.string))
                (Decode.maybe (Decode.field "name" Decode.string))
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


cardIsInHand : String -> String -> String -> HandState -> Bool
cardIsInHand red player cardId hand =
    let
        playerHand =
            if player == red then
                hand.red

            else
                hand.blue
    in
    List.any
        (\slot ->
            case slot of
                Just c ->
                    c.id == cardId

                Nothing ->
                    False
        )
        playerHand


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


{-| Remove cards that were benched this turn from the bench display, so they
only appear in the play-info "Benched" section when that option is enabled.
Known cards are matched by id; unknown benched cards remove from the tail.
-}
stripBenchedFromBenchSide : List (Maybe Action.CardRef) -> List Action.CardRef -> List Action.CardRef
stripBenchedFromBenchSide benched benchSide =
    let
        knownBenched =
            List.filterMap identity benched

        unknownCount =
            List.length (List.filter ((==) Nothing) benched)

        -- Remove only the first occurrence of the given card id.
        removeFirst ref acc =
            case acc of
                [] ->
                    []

                c :: rest ->
                    if c.id == ref.id then
                        rest

                    else
                        c :: removeFirst ref rest

        afterKnown =
            List.foldl removeFirst benchSide knownBenched
    in
    List.take (max 0 (List.length afterKnown - unknownCount)) afterKnown


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

        Action.MulliganBonus _ ->
            -- The drawn cards are always described by a DrewCount detail (with
            -- optional bullet card list), so let applyDetailAction handle it.
            hand

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
            -- Only remove from hand if the card is actually tracked there.
            -- If not found, the card came from the deck, not the hand.
            if cardIsInHand red player card.id hand then
                removeById red player card.id hand

            else
                hand

        Action.Discarded { player, count } ->
            let
                known =
                    detailCardList detail
            in
            if List.isEmpty known then
                removeN red player count hand

            else
                List.foldl (\card h -> removeById red player card.id h) hand known

        Action.ShuffledInto { player, card, count } ->
            case card of
                Just c ->
                    removeById red player c.id hand

                Nothing ->
                    let
                        known =
                            detailCardList detail
                    in
                    if List.isEmpty known then
                        removeN red player (Maybe.withDefault 1 count) hand

                    else
                        List.foldl (\c h -> removeById red player c.id h) hand known

        Action.PutOnTop { player, card } ->
            removeById red player card.id hand

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

        Action.Evolved { player, to } ->
            -- Rare Candy and similar effects trigger Evolved as a detail of PlayedTrainer.
            removeById red player to.id hand

        Action.MovedToDiscard { owner, count } ->
            let
                known =
                    detailCardList detail
            in
            if List.isEmpty known then
                removeN red owner count hand

            else
                List.foldl (\card h -> removeById red owner card.id h) hand known

        _ ->
            hand


{-| True when the group represents a Pokémon ability (e.g. Dudunsparce
"Run Away Draw") where the ability card ID appears in a ShuffledInto CardList.
Logged as either PlayedTrainer or UsedAttack depending on game version; in both
cases the card was on the bench (not in hand) and shuffled cards come from
bench/evo-buried rather than hand.

For PlayedTrainer groups we additionally require that the draw comes BEFORE
the shuffle in the details list. Pokémon abilities draw first then shuffle the
Pokémon back (Dudunsparce: "drew 3, shuffled Dudunsparce+Dunsparce"). Trainer
cards like Lillie's Determination do the opposite (shuffle hand first, then
draw), so they are never mistaken for abilities even when a second copy of the
trainer happens to appear in the ShuffledInto CardList.
-}
isPokemonAbilityGroup : Action.ActionGroup -> Bool
isPokemonAbilityGroup group =
    case pokemonAbilityPlayedCardId group of
        Nothing ->
            False

        Just cardId ->
            let
                hasMatchingShuffleBack =
                    List.any
                        (\d ->
                            case d.action of
                                Action.ShuffledInto info ->
                                    info.card
                                        == Nothing
                                        && List.any
                                            (\b ->
                                                case b.action of
                                                    Action.CardList cards ->
                                                        List.any (\c -> c.id == cardId) cards

                                                    _ ->
                                                        False
                                            )
                                            d.bullets

                                _ ->
                                    False
                        )
                        group.details

                -- For PlayedTrainer groups, verify draw-before-shuffle order to
                -- avoid false positives when a second copy of a trainer card
                -- appears in the hand being shuffled back (e.g. Lillie's Det.).
                orderIsAbility =
                    case group.action of
                        Action.PlayedTrainer _ ->
                            let
                                indexed =
                                    List.indexedMap Tuple.pair group.details

                                firstDrewIndex =
                                    indexed
                                        |> List.filterMap
                                            (\( i, d ) ->
                                                case d.action of
                                                    Action.DrewCount _ ->
                                                        Just i

                                                    _ ->
                                                        Nothing
                                            )
                                        |> List.head

                                firstShuffleIndex =
                                    indexed
                                        |> List.filterMap
                                            (\( i, d ) ->
                                                case d.action of
                                                    Action.ShuffledInto _ ->
                                                        Just i

                                                    _ ->
                                                        Nothing
                                            )
                                        |> List.head
                            in
                            case ( firstDrewIndex, firstShuffleIndex ) of
                                ( Just di, Just si ) ->
                                    di < si

                                _ ->
                                    -- No DrewCount or no ShuffledInto — order
                                    -- constraint doesn't apply.
                                    True

                        _ ->
                            True
            in
            hasMatchingShuffleBack && orderIsAbility


{-| Returns the card ID of the ability Pokémon for PlayedTrainer or UsedAttack
top actions, Nothing otherwise.
-}
pokemonAbilityPlayedCardId : Action.ActionGroup -> Maybe String
pokemonAbilityPlayedCardId group =
    case group.action of
        Action.PlayedTrainer { card } ->
            Just card.id

        Action.UsedAttack { attacker } ->
            Just attacker.card.id

        _ ->
            Nothing


{-| True for a PlayedTrainer group that shuffles specific cards from the
discard pile into the deck (e.g. Energy Recycler). Signature: exactly one
anonymous ShuffledInto with a CardList bullet and no DrewCount details.
-}
isDiscardShuffleGroup : Action.ActionGroup -> Bool
isDiscardShuffleGroup group =
    -- Energy Recycler is the only trainer that shuffles cards from the discard
    -- pile into the deck. All other shuffle effects come from hand.
    case group.action of
        Action.PlayedTrainer { card } ->
            card.name == "Energy Recycler"

        _ ->
            False


{-| True when the group is a Pokémon ability that attaches energy from the deck
(as opposed to from the hand). Add ability move names here as needed.
-}
isDeckAttachAbilityGroup : Action.ActionGroup -> Bool
isDeckAttachAbilityGroup group =
    case group.action of
        Action.UsedAttack { move } ->
            move == "Metal Maker"

        _ ->
            False


applyGroupToHand : String -> HandState -> Action.ActionGroup -> HandState
applyGroupToHand red hand group =
    let
        isPokemonAbility =
            isPokemonAbilityGroup group

        isDiscardShuffle =
            isDiscardShuffleGroup group

        hand1 =
            if isPokemonAbility then
                -- Card was on bench, not in hand — skip the hand removal.
                hand

            else
                applyTopAction red hand group

        details =
            case group.action of
                Action.PlayedStadium _ ->
                    -- DiscardedCard details under PlayedStadium are displaced stadiums
                    -- leaving the stadium zone, not hand — skip them for hand tracking.
                    List.filter
                        (\d ->
                            case d.action of
                                Action.DiscardedCard _ ->
                                    False

                                _ ->
                                    True
                        )
                        group.details

                _ ->
                    group.details

        -- Attached items come from the deck when the group is a named deck-search
        -- ability (e.g. Metal Maker) or contains a ShuffledDeck detail (deck-search
        -- trainer e.g. Crispin).
        deckAttachPlayers =
            if isDeckAttachAbilityGroup group then
                group.details
                    |> List.filterMap
                        (\d ->
                            case d.action of
                                Action.Attached { player } ->
                                    Just player

                                _ ->
                                    Nothing
                        )

            else
                group.details
                    |> List.filterMap
                        (\d ->
                            case d.action of
                                Action.ShuffledDeck { player } ->
                                    Just player

                                _ ->
                                    Nothing
                        )
    in
    List.foldl
        (\detail h ->
            case detail.action of
                Action.Attached { player } ->
                    if List.member player deckAttachPlayers then
                        h

                    else
                        applyDetailAction red h detail

                Action.ShuffledInto _ ->
                    if isPokemonAbility || isDiscardShuffle then
                        -- Cards come from bench/evo-buried (ability) or discard pile
                        -- (Energy Recycler-like trainers), not from hand.
                        h

                    else
                        applyDetailAction red h detail

                _ ->
                    applyDetailAction red h detail
        )
        hand1
        details


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


correctGroupPlayers : Replay.Players -> InstanceState -> Action.ActionGroup -> Action.ActionGroup
correctGroupPlayers players instanceState group =
    let
        shuffleDeckPlayer =
            group.details
                |> List.filterMap
                    (\d ->
                        case d.action of
                            Action.ShuffledDeck { player } ->
                                Just player

                            _ ->
                                Nothing
                    )
                |> List.head

        -- If there is exactly one DrewCount in the group, record its player.
        -- A single-player draw (e.g. Dudunsparce Run Away Draw) means the raw
        -- attribution is trustworthy; a multi-player draw (e.g. Judge) means
        -- both players' actions are logged under the card player's name and
        -- need correctDetailPlayer.
        singleDrewPlayer =
            let
                drewDetails =
                    List.filterMap
                        (\d ->
                            case d.action of
                                Action.DrewCount { player } ->
                                    Just player

                                _ ->
                                    Nothing
                        )
                        group.details
            in
            case drewDetails of
                [ p ] ->
                    Just p

                _ ->
                    Nothing

        -- If the group has exactly one anonymous ShuffledInto detail, its player
        -- is recorded here. A single shuffle is trustworthy as-is (e.g. Energy
        -- Recycler) — only multi-player groups (e.g. Judge) need reattribution.
        singleShuffledIntoPlayer =
            let
                shufflePlayers =
                    List.filterMap
                        (\d ->
                            case d.action of
                                Action.ShuffledInto { player, card } ->
                                    if card == Nothing then
                                        Just player

                                    else
                                        Nothing

                                _ ->
                                    Nothing
                        )
                        group.details
            in
            case shufflePlayers of
                [ p ] ->
                    Just p

                _ ->
                    Nothing

        -- Given a player name, return the other player.
        otherPlayer p =
            if p == players.red then players.blue else players.red

        -- Player attributed by the first Attached detail in this group, if any.
        groupAttachPlayer =
            group.details
                |> List.filterMap
                    (\d ->
                        case d.action of
                            Action.Attached { player } ->
                                Just player

                            _ ->
                                Nothing
                    )
                |> List.head

        playerHasPokemonOnBench p cardId =
            Dict.get ( p, cardId ) instanceState.bench
                |> Maybe.map (not << List.isEmpty)
                |> Maybe.withDefault False

        playerHasPokemonActive p cardId =
            Dict.get ( p, cardId ) instanceState.activeSpot
                |> Maybe.andThen identity
                |> Maybe.map (\_ -> True)
                |> Maybe.withDefault False

        -- True when the group has a ShuffledInto with a CardList bullet for
        -- the given player — confirms the player is the recorder (cards visible).
        hasRevealedShuffleFor player =
            List.any
                (\d ->
                    case d.action of
                        Action.ShuffledInto info ->
                            info.player
                                == player
                                && List.any
                                    (\b ->
                                        case b.action of
                                            Action.CardList _ ->
                                                True

                                            _ ->
                                                False
                                    )
                                    d.bullets

                        _ ->
                            False
                )
                group.details

        correctDetail detail =
            case detail.action of
                Action.DrewCount { player, count } ->
                    case shuffleDeckPlayer of
                        Just sp ->
                            if player /= sp then
                                let
                                    raw =
                                        detail.raw
                                            |> String.replace (player ++ " drew") (sp ++ " drew")
                                in
                                { detail | action = Action.DrewCount { player = sp, count = count }, raw = raw }

                            else
                                detail

                        Nothing ->
                            -- Single player drew AND has a revealed shuffle (Dudunsparce
                            -- ability): the raw attribution is correct, don't reassign.
                            if singleDrewPlayer == Just player && hasRevealedShuffleFor player then
                                detail

                            else
                                correctDetailPlayer players detail

                Action.ShuffledInto { player, card } ->
                    if card == Nothing then
                        -- Single-player draw+shuffle (Dudunsparce) or single discard
                        -- shuffle (Energy Recycler): the raw attribution is correct.
                        -- Multi-player groups (e.g. Judge): both shuffles are logged under
                        -- the card player but belong to different players — run
                        -- correctDetailPlayer to fix that.
                        if singleDrewPlayer == Just player && hasRevealedShuffleFor player then
                            detail

                        else if singleShuffledIntoPlayer == Just player then
                            detail

                        else
                            correctDetailPlayer players detail

                    else
                        detail

                Action.PutOnBottom { card } ->
                    if card == Nothing then
                        correctDetailPlayer players detail

                    else
                        detail

                Action.ShuffledDeck { player } ->
                    case groupAttachPlayer of
                        Just ap ->
                            if ap /= player then
                                { detail
                                    | action = Action.ShuffledDeck { player = ap }
                                    , raw = String.replace (player ++ " shuffled") (ap ++ " shuffled") detail.raw
                                }

                            else
                                detail

                        Nothing ->
                            detail

                Action.ShuffledCards { player } ->
                    let
                        correctPlayer =
                            case groupAttachPlayer of
                                Just ap ->
                                    Just ap

                                Nothing ->
                                    if isDeckAttachAbilityGroup group then
                                        case group.action of
                                            Action.UsedAttack { attacker } ->
                                                Just attacker.player

                                            _ ->
                                                Nothing

                                    else
                                        Nothing
                    in
                    case correctPlayer of
                        Just ap ->
                            if ap /= player then
                                { detail
                                    | action = Action.ShuffledCards { player = ap }
                                    , raw = String.replace (player ++ " shuffled") (ap ++ " shuffled") detail.raw
                                }

                            else
                                detail

                        Nothing ->
                            detail

                Action.MovedDamageCounters { player, count, from, to } ->
                    let
                        correctPokemon pokemon =
                            let
                                other =
                                    otherPlayer pokemon.player
                            in
                            if playerHasPokemonOnBench pokemon.player pokemon.card.id then
                                pokemon

                            else if playerHasPokemonActive pokemon.player pokemon.card.id then
                                pokemon

                            else if playerHasPokemonOnBench other pokemon.card.id then
                                { pokemon | player = other }

                            else if playerHasPokemonActive other pokemon.card.id then
                                { pokemon | player = other }

                            else
                                pokemon

                        correctedFrom =
                            correctPokemon from

                        correctedTo =
                            correctPokemon to

                        correctedRaw =
                            detail.raw
                                |> (if correctedFrom.player /= from.player then
                                        String.replace (" from " ++ from.player ++ "'s") (" from " ++ correctedFrom.player ++ "'s")

                                    else
                                        identity
                                   )
                                |> (if correctedTo.player /= to.player then
                                        String.replace (" to " ++ to.player ++ "'s") (" to " ++ correctedTo.player ++ "'s")

                                    else
                                        identity
                                   )
                    in
                    if correctedFrom == from && correctedTo == to then
                        detail

                    else
                        { detail
                            | action = Action.MovedDamageCounters { player = player, count = count, from = correctedFrom, to = correctedTo }
                            , raw = correctedRaw
                        }

                Action.PlacedDamageCounters { player, pokemon, count } ->
                    let
                        other =
                            otherPlayer pokemon.player

                        correctedPlayer =
                            if playerHasPokemonOnBench pokemon.player pokemon.card.id then
                                pokemon.player

                            else if playerHasPokemonActive pokemon.player pokemon.card.id then
                                pokemon.player

                            else if playerHasPokemonOnBench other pokemon.card.id then
                                other

                            else if playerHasPokemonActive other pokemon.card.id then
                                other

                            else
                                pokemon.player
                    in
                    if correctedPlayer == pokemon.player then
                        detail

                    else
                        { detail
                            | action = Action.PlacedDamageCounters { player = player, pokemon = { pokemon | player = correctedPlayer }, count = count }
                            , raw =
                                String.replace
                                    (" on " ++ pokemon.player ++ "'s")
                                    (" on " ++ correctedPlayer ++ "'s")
                                    detail.raw
                        }

                _ ->
                    detail
    in
    { group | details = List.map correctDetail group.details }


actionPlayer : Action.Action -> String
actionPlayer action =
    case action of
        Action.DrewCount { player } ->
            player

        Action.ShuffledInto { player } ->
            player

        Action.PutOnBottom { player } ->
            player

        _ ->
            ""


collectAndCorrectGroups : Replay.Players -> Replay.Replay -> Int -> Int -> List Action.ActionGroup
collectAndCorrectGroups players replay sectionIndex groupIndex =
    collectAllGroups replay sectionIndex groupIndex
        |> List.foldl
            (\group ( state, acc ) ->
                let
                    corrected =
                        correctGroupPlayers players state group

                    newState =
                        applyGroupToInstances corrected state
                in
                ( newState, corrected :: acc )
            )
            ( emptyInstances, [] )
        |> Tuple.second
        |> List.reverse


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

        Action.MulliganBonus _ ->
            -- DrewCount detail handles the deck change (avoids double-counting).
            piles

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

        Action.NCardsDiscardedFrom { pokemon, count } ->
            pilesDiscardDelta red pokemon.player count piles

        Action.Discarded { player, count } ->
            pilesDiscardDelta red player count piles

        Action.DiscardedCard { player } ->
            pilesDiscardDelta red player 1 piles

        Action.TookPrize { player, count } ->
            pilesPrizeDelta red player -count piles

        Action.KnockedOut { pokemon } ->
            pilesDiscardDelta red pokemon.player 1 piles

        Action.MovedToHand { player, count } ->
            -- Card retrieved from discard to hand (e.g. Night Stretcher, Brock's Scouting).
            -- When count is Nothing a single card was moved.
            pilesDiscardDelta red player -(Maybe.withDefault 1 count) piles

        Action.MovedToDiscard { owner, count } ->
            pilesDiscardDelta red owner count piles

        _ ->
            piles


applyGroupToPiles : String -> Bool -> PileState -> Action.ActionGroup -> PileState
applyGroupToPiles red isSetup piles group =
    let
        piles1 =
            if isPokemonAbilityGroup group then
                -- The played card was on bench (not a trainer going to discard);
                -- deck changes are handled entirely by the ShuffledInto detail.
                piles

            else
                applyActionToPiles red isSetup group.action piles

        deckAttachPlayers =
            if isDeckAttachAbilityGroup group then
                group.details
                    |> List.filterMap
                        (\d ->
                            case d.action of
                                Action.Attached { player } ->
                                    Just player

                                _ ->
                                    Nothing
                        )

            else
                group.details
                    |> List.filterMap
                        (\d ->
                            case d.action of
                                Action.ShuffledDeck { player } ->
                                    Just player

                                _ ->
                                    Nothing
                        )
    in
    List.foldl
        (\detail p ->
            let
                p1 =
                    case detail.action of
                        Action.Attached { player } ->
                            if List.member player deckAttachPlayers then
                                pilesDeckDelta red player -1 p

                            else
                                applyActionToPiles red isSetup detail.action p

                        _ ->
                            applyActionToPiles red isSetup detail.action p

                -- For discard-shuffle groups (e.g. Energy Recycler), the shuffled
                -- cards come from the discard pile, so also decrement discard.
                p2 =
                    if isDiscardShuffleGroup group then
                        case detail.action of
                            Action.ShuffledInto { player, card, count } ->
                                pilesDiscardDelta red player
                                    -(case card of
                                        Just _ ->
                                            1

                                        Nothing ->
                                            Maybe.withDefault 1 count
                                     )
                                    p1

                            _ ->
                                p1

                    else
                        p1
            in
            List.foldl
                (\bullet bp -> applyActionToPiles red isSetup bullet.action bp)
                p2
                detail.bullets
        )
        piles1
        group.details


-- Adjusts pile state for DiscardedCard details that came from the deck (not hand).
-- applyGroupToPiles already adds +1 to discard for each DiscardedCard, but does not
-- decrement the deck. When the card was not in hand, it must have come from the deck.
applyDeckDiscardCorrection : String -> HandState -> Action.ActionGroup -> PileState -> PileState
applyDeckDiscardCorrection red hand group piles =
    List.foldl
        (\detail p ->
            case detail.action of
                Action.DiscardedCard { player, card } ->
                    if not (cardIsInHand red player card.id hand) then
                        pilesDeckDelta red player -1 p

                    else
                        p

                _ ->
                    p
        )
        piles
        group.details


computePiles : Replay.Players -> Replay.Replay -> Int -> Int -> PileState
computePiles players replay sectionIndex groupIndex =
    let
        groupPairs =
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
                        List.map (\g -> ( isSetup, correctGroupPlayers players emptyInstances g )) trimmed
                    )
                |> List.concat
    in
    -- Hand state is tracked alongside piles so we can detect DiscardedCard
    -- details that came from the deck (card not in hand) vs from hand.
    List.foldl
        (\( isSetup, group ) ( h, p ) ->
            let
                p1 =
                    applyGroupToPiles players.red isSetup p group

                p2 =
                    applyDeckDiscardCorrection players.red h group p1

                h1 =
                    applyGroupToHand players.red h group
            in
            ( h1, p2 )
        )
        ( emptyHand, emptyPiles )
        groupPairs
        |> Tuple.second


-- BENCH STATE


-- When a Pokémon retreats or is switched to the bench, put it at the front
-- (True) or back (False) of the bench list.
retreatToFront : Bool
retreatToFront =
    True



addToBench : String -> String -> Action.CardRef -> BenchState -> BenchState
addToBench red player card bench =
    if player == red then
        { bench | red = bench.red ++ [ card ] }

    else
        { bench | blue = bench.blue ++ [ card ] }


prependToBench : String -> String -> Action.CardRef -> BenchState -> BenchState
prependToBench red player card bench =
    if player == red then
        { bench | red = card :: bench.red }

    else
        { bench | blue = card :: bench.blue }


retreatToBench : String -> String -> Action.CardRef -> BenchState -> BenchState
retreatToBench red player card bench =
    if retreatToFront then
        prependToBench red player card bench

    else
        addToBench red player card bench


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


applyActionToBench : String -> ActiveState -> Action.Action -> BenchState -> BenchState
applyActionToBench red active action bench =
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
            let
                isActive =
                    (if pokemon.player == red then active.red else active.blue)
                        |> Maybe.map .id
                        |> (==) (Just pokemon.card.id)
            in
            if isActive then
                bench

            else
                removeFromBench red pokemon.player pokemon.card.id bench

        Action.MovedToActive { pokemon } ->
            -- Guard: if the pokemon is already in the active spot (set by a Switched
            -- action earlier in this same fold pass), skip the removal so we don't
            -- accidentally remove a second bench pokemon with the same card id.
            let
                alreadyActive =
                    (if pokemon.player == red then active.red else active.blue)
                        |> Maybe.map .id
                        |> (==) (Just pokemon.card.id)
            in
            if alreadyActive then
                bench

            else
                removeFromBench red pokemon.player pokemon.card.id bench

        Action.Retreated { player, card } ->
            retreatToBench red player card bench

        Action.Switched { player, from, to } ->
            -- `from` leaves the bench to become active; `to` leaves active to join bench
            bench
                |> removeFromBench red player from.id
                |> (if String.isEmpty to.id then identity else retreatToBench red player to)

        _ ->
            bench


applyGroupToBench : String -> ActiveState -> BenchState -> Action.ActionGroup -> BenchState
applyGroupToBench red active bench group =
    -- `active` is the accumulated state *before* this group. Switched and
    -- MovedToActive always appear in separate groups, so checking the incoming
    -- active state is the right guard: if the pokemon was already made active
    -- by a prior group's Switched, MovedToActive here is a redundant
    -- confirmation and should not touch the bench.
    let
        -- For a Pokemon ability group, record the played card so the ShuffledInto
        -- detail knows which bench slot to remove.
        pokemonAbilityCardId =
            if isPokemonAbilityGroup group then
                pokemonAbilityPlayedCardId group

            else
                Nothing

        bench1 =
            applyActionToBench red active group.action bench
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

                        Action.ShuffledInto { player } ->
                            case pokemonAbilityCardId of
                                Just cardId ->
                                    -- Only the evolved Pokémon (played card) leaves the bench.
                                    -- The evo-buried pre-evo is tracked in evolution state, not bench.
                                    removeFromBench red player cardId b

                                Nothing ->
                                    applyActionToBench red active detail.action b

                        _ ->
                            applyActionToBench red active detail.action b
            in
            List.foldl
                (\bullet bb -> applyActionToBench red active bullet.action bb)
                b1
                detail.bullets
        )
        bench1
        group.details


computeBench : Replay.Players -> Replay.Replay -> Int -> Int -> BenchState
computeBench players replay sectionIndex groupIndex =
    List.foldl
        (\group ( b, a ) ->
            ( applyGroupToBench players.red a b group
            , applyGroupToActive players.red a group
            )
        )
        ( emptyBench, emptyActive )
        (collectAndCorrectGroups players replay sectionIndex groupIndex)
        |> Tuple.first


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


-- INSTANCE STATE


type alias InstanceId =
    Int


type alias InstanceState =
    { nextId : InstanceId
    , bench : Dict ( String, String ) (List InstanceId)
    , activeSpot : Dict ( String, String ) (Maybe InstanceId)
    , lastMoved : Dict ( String, String ) InstanceId
    }


emptyInstances : InstanceState
emptyInstances =
    { nextId = 0, bench = Dict.empty, activeSpot = Dict.empty, lastMoved = Dict.empty }


assignInstance : String -> String -> InstanceState -> ( InstanceState, InstanceId )
assignInstance player cardId state =
    ( { state | nextId = state.nextId + 1 }, state.nextId )


addToBenchInstances : String -> String -> InstanceId -> InstanceState -> InstanceState
addToBenchInstances player cardId iid state =
    { state | bench = Dict.update ( player, cardId ) (\ex -> Just (Maybe.withDefault [] ex ++ [ iid ])) state.bench }


setActiveInstance : String -> String -> InstanceId -> InstanceState -> InstanceState
setActiveInstance player cardId iid state =
    { state | activeSpot = Dict.insert ( player, cardId ) (Just iid) state.activeSpot }


firstInstance : String -> String -> InstanceState -> Maybe InstanceId
firstInstance player cardId state =
    nthInstance player cardId 0 state


nthInstance : String -> String -> Int -> InstanceState -> Maybe InstanceId
nthInstance player cardId n state =
    let
        active =
            Dict.get ( player, cardId ) state.activeSpot
                |> Maybe.andThen identity
                |> Maybe.map List.singleton
                |> Maybe.withDefault []

        bench =
            Dict.get ( player, cardId ) state.bench
                |> Maybe.withDefault []
    in
    List.drop n (active ++ bench) |> List.head


nthBenchInstance : String -> String -> Int -> InstanceState -> Maybe InstanceId
nthBenchInstance player cardId n state =
    let
        bench =
            Dict.get ( player, cardId ) state.bench
                |> Maybe.withDefault []

        active =
            Dict.get ( player, cardId ) state.activeSpot
                |> Maybe.andThen identity
                |> Maybe.map List.singleton
                |> Maybe.withDefault []
    in
    List.drop n (bench ++ active) |> List.head


retireFirstInstance : String -> String -> InstanceState -> InstanceState
retireFirstInstance player cardId state =
    let
        key =
            ( player, cardId )

        activeIid =
            Dict.get key state.activeSpot |> Maybe.andThen identity
    in
    case activeIid of
        Just _ ->
            { state | activeSpot = Dict.insert key Nothing state.activeSpot }

        Nothing ->
            { state | bench = Dict.update key (Maybe.map (List.drop 1)) state.bench }


transferFirstInstanceBench : String -> String -> String -> InstanceState -> InstanceState
transferFirstInstanceBench player fromId toId state =
    let
        fromKey =
            ( player, fromId )

        toKey =
            ( player, toId )

        maybeIid =
            Dict.get fromKey state.bench |> Maybe.andThen List.head
    in
    case maybeIid of
        Nothing ->
            state

        Just iid ->
            { state
                | bench =
                    state.bench
                        |> Dict.update fromKey (Maybe.map (List.drop 1))
                        |> Dict.update toKey (\ex -> Just (Maybe.withDefault [] ex ++ [ iid ]))
            }


transferFirstInstanceActive : String -> String -> String -> InstanceState -> InstanceState
transferFirstInstanceActive player fromId toId state =
    let
        fromKey =
            ( player, fromId )

        toKey =
            ( player, toId )

        maybeIid =
            Dict.get fromKey state.activeSpot |> Maybe.andThen identity
    in
    case maybeIid of
        Nothing ->
            state

        Just iid ->
            { state
                | activeSpot =
                    state.activeSpot
                        |> Dict.insert fromKey Nothing
                        |> Dict.insert toKey (Just iid)
            }


applyActionToInstances : Action.Action -> InstanceState -> InstanceState
applyActionToInstances action state =
    case action of
        Action.PlayedPokemon { player, card, position } ->
            let
                ( s1, iid ) =
                    assignInstance player card.id state
            in
            case position of
                Action.BenchSpot ->
                    addToBenchInstances player card.id iid s1

                _ ->
                    setActiveInstance player card.id iid s1

        Action.DrewCard { player, card, andPlayed } ->
            case andPlayed of
                Just pos ->
                    let
                        ( s1, iid ) =
                            assignInstance player card.id state
                    in
                    case pos of
                        Action.BenchSpot ->
                            addToBenchInstances player card.id iid s1

                        _ ->
                            setActiveInstance player card.id iid s1

                Nothing ->
                    state

        Action.KnockedOut { pokemon } ->
            retireFirstInstance pokemon.player pokemon.card.id state

        Action.Evolved { player, from, to, position } ->
            case position of
                Action.BenchSpot ->
                    transferFirstInstanceBench player from.id to.id state

                _ ->
                    transferFirstInstanceActive player from.id to.id state

        Action.MovedToActive { pokemon } ->
            let
                key =
                    ( pokemon.player, pokemon.card.id )
            in
            -- Skip if a preceding Switched already moved this pokemon to the active spot.
            case Dict.get key state.activeSpot |> Maybe.andThen identity of
                Just _ ->
                    state

                Nothing ->
                    let
                        benchList =
                            Dict.get key state.bench |> Maybe.withDefault []

                        -- Exclude the instance that just retreated/switched out so we
                        -- don't immediately promote it back when two copies of the same
                        -- card are in play. This is independent of bench display order.
                        -- Exclude the instance that just retreated/switched out so we
                        -- don't immediately promote it back when two copies of the same
                        -- card are in play. This is independent of bench display order.
                        excludeIid =
                            Dict.get key state.lastMoved

                        maybeIid =
                            case excludeIid of
                                Just rid ->
                                    case List.filter (\iid -> iid /= rid) benchList of
                                        first :: _ ->
                                            Just first

                                        [] ->
                                            List.head benchList

                                Nothing ->
                                    List.head benchList
                    in
                    case maybeIid of
                        Nothing ->
                            state

                        Just iid ->
                            { state
                                | bench =
                                    Dict.update key
                                        (Maybe.map (List.filter (\i -> i /= iid)))
                                        state.bench
                                , activeSpot = Dict.insert key (Just iid) state.activeSpot
                                , lastMoved = Dict.remove key state.lastMoved
                            }

        Action.Switched { player, from, to } ->
            if from.id == to.id && not (String.isEmpty from.id) then
                -- Same-card switch: both keys are equal, so the generic update
                -- would overwrite itself. Swap active ↔ first bench instance directly.
                let
                    key =
                        ( player, from.id )

                    activeIid =
                        Dict.get key state.activeSpot |> Maybe.andThen identity

                    benchHead =
                        Dict.get key state.bench |> Maybe.andThen List.head
                in
                case ( activeIid, benchHead ) of
                    ( Just aIid, Just bIid ) ->
                        { state
                            | bench =
                                Dict.update key
                                    (Maybe.map
                                        (\lst ->
                                            let
                                                tail =
                                                    List.drop 1 lst
                                            in
                                            if retreatToFront then
                                                aIid :: tail

                                            else
                                                tail ++ [ aIid ]
                                        )
                                    )
                                    state.bench
                            , activeSpot = Dict.insert key (Just bIid) state.activeSpot
                            , lastMoved = Dict.insert key aIid state.lastMoved
                        }

                    _ ->
                        state

            else
                let
                    fromKey =
                        ( player, from.id )

                    toKey =
                        ( player, to.id )

                    fromIid =
                        Dict.get fromKey state.bench |> Maybe.andThen List.head

                    toIid =
                        Dict.get toKey state.activeSpot |> Maybe.andThen identity
                in
                { state
                    | bench =
                        state.bench
                            |> Dict.update fromKey (Maybe.map (List.drop 1))
                            |> (if String.isEmpty to.id then
                                    identity

                                else
                                    case toIid of
                                        Just iid ->
                                            Dict.update toKey
                                                (\ex ->
                                                    let lst = Maybe.withDefault [] ex
                                                    in Just (if retreatToFront then iid :: lst else lst ++ [ iid ])
                                                )

                                        Nothing ->
                                            identity
                               )
                    , activeSpot =
                        state.activeSpot
                            |> (case fromIid of
                                    Just iid ->
                                        Dict.insert fromKey (Just iid)

                                    Nothing ->
                                        identity
                               )
                            |> (if String.isEmpty to.id then
                                    identity

                                else
                                    Dict.insert toKey Nothing
                               )
                    , lastMoved =
                        case toIid of
                            Just iid ->
                                Dict.insert toKey iid state.lastMoved

                            Nothing ->
                                state.lastMoved
                }

        Action.Retreated { player, card } ->
            let
                key =
                    ( player, card.id )

                maybeIid =
                    Dict.get key state.activeSpot |> Maybe.andThen identity
            in
            case maybeIid of
                Nothing ->
                    state

                Just iid ->
                    { state
                        | activeSpot = Dict.insert key Nothing state.activeSpot
                        , bench =
                            Dict.update key
                                (\ex ->
                                    let lst = Maybe.withDefault [] ex
                                    in Just (if retreatToFront then iid :: lst else lst ++ [ iid ])
                                )
                                state.bench
                        , lastMoved = Dict.insert key iid state.lastMoved
                    }

        _ ->
            state


applyDetailToInstances : Action.DetailAction -> InstanceState -> InstanceState
applyDetailToInstances detail state =
    case detail.action of
        Action.DrewAndPlayed { player, position } ->
            case position of
                Action.BenchSpot ->
                    List.foldl
                        (\card s ->
                            let
                                ( s1, iid ) =
                                    assignInstance player card.id s
                            in
                            addToBenchInstances player card.id iid s1
                        )
                        state
                        (detailCardList detail)

                _ ->
                    state

        _ ->
            applyActionToInstances detail.action state


applyGroupToInstances : Action.ActionGroup -> InstanceState -> InstanceState
applyGroupToInstances group state =
    let
        s1 =
            applyActionToInstances group.action state
    in
    List.foldl
        (\detail s ->
            let
                s2 =
                    applyDetailToInstances detail s
            in
            List.foldl (\bullet bs -> applyActionToInstances bullet.action bs) s2 detail.bullets
        )
        s1
        group.details


instanceIdForField : InstanceState -> String -> String -> Int -> Maybe InstanceId
instanceIdForField instances player cardId fieldOrdinal =
    Dict.get ( player, cardId ) instances.bench
        |> Maybe.andThen (List.drop fieldOrdinal >> List.head)


-- ATTACHMENT STATE


{-| One attachment-list entry per pokemon instance.
Two Staryus on the bench produce two separate entries both with cardId="sv4_123"
and position=BenchSpot; their ordinal within the bench list disambiguates them
at render time.
-}
type alias AttachmentEntry =
    { instanceId : InstanceId
    , player : String
    , cardId : String
    , position : Action.Position
    , items : List Action.CardRef
    }


{-| Ordered list of per-instance attachment entries. Order matters: the Nth
entry with a given (player, cardId, position) triple corresponds to the Nth
pokemon with that card id at that position for that player.
-}
type alias AttachmentState =
    List AttachmentEntry


emptyAttachments : AttachmentState
emptyAttachments =
    []


{-| Index of the first entry matching the given player, card id and position. -}
findEntryIndex : String -> String -> Action.Position -> AttachmentState -> Maybe Int
findEntryIndex player cardId position state =
    state
        |> List.indexedMap Tuple.pair
        |> List.filter (\( _, e ) -> e.player == player && e.cardId == cardId && e.position == position)
        |> List.head
        |> Maybe.map Tuple.first


{-| Index of the first entry matching the given instanceId. -}
findEntryByInstance : InstanceId -> AttachmentState -> Maybe Int
findEntryByInstance iid state =
    state
        |> List.indexedMap Tuple.pair
        |> List.filter (\( _, e ) -> e.instanceId == iid)
        |> List.head
        |> Maybe.map Tuple.first


{-| Update the element at the given index in a list. -}
updateAt : Int -> (a -> a) -> List a -> List a
updateAt idx f list =
    List.indexedMap
        (\i x ->
            if i == idx then
                f x

            else
                x
        )
        list


{-| Return the items attached to the pokemon with the given instanceId. -}
lookupAttachments : AttachmentState -> InstanceId -> List Action.CardRef
lookupAttachments state iid =
    state
        |> List.filter (\e -> e.instanceId == iid)
        |> List.head
        |> Maybe.map .items
        |> Maybe.withDefault []


{-| Move the first attachment entry for the given player's card from one
position to another, e.g. BenchSpot → ActiveSpot when a pokemon becomes Active. -}
moveAttachments : String -> String -> Action.Position -> Action.Position -> AttachmentState -> AttachmentState
moveAttachments player cardId fromPos toPos state =
    case findEntryIndex player cardId fromPos state of
        Just idx ->
            updateAt idx (\e -> { e | position = toPos }) state

        Nothing ->
            state


applyActionToAttachments : InstanceState -> InstanceState -> Action.Action -> AttachmentState -> AttachmentState
applyActionToAttachments preInstances postInstances action state =
    case action of
        Action.Attached { player, item, target, position } ->
            let
                maybeIid =
                    case position of
                        Action.BenchSpot ->
                            nthBenchInstance player target.card.id 0 postInstances

                        _ ->
                            firstInstance player target.card.id postInstances
            in
            case maybeIid of
                Nothing ->
                    state

                Just iid ->
                    case findEntryByInstance iid state of
                        Just idx ->
                            updateAt idx (\e -> { e | items = item :: e.items }) state

                        Nothing ->
                            state ++ [ { instanceId = iid, player = player, cardId = target.card.id, position = position, items = [ item ] } ]

        Action.KnockedOut { pokemon } ->
            -- Remove by the specific instance that was active (from preInstances),
            -- so we don't accidentally wipe a bench copy of the same card.
            let
                removeIdx idx st =
                    List.take idx st ++ List.drop (idx + 1) st

                maybeIid =
                    Dict.get ( pokemon.player, pokemon.card.id ) preInstances.activeSpot
                        |> Maybe.andThen identity
            in
            case maybeIid |> Maybe.andThen (\iid -> findEntryByInstance iid state) of
                Just idx ->
                    removeIdx idx state

                Nothing ->
                    -- Fallback: position-based (handles bench KO edge cases)
                    case findEntryIndex pokemon.player pokemon.card.id Action.ActiveSpot state of
                        Just idx ->
                            removeIdx idx state

                        Nothing ->
                            case findEntryIndex pokemon.player pokemon.card.id Action.BenchSpot state of
                                Just idx ->
                                    removeIdx idx state

                                Nothing ->
                                    state

        Action.Evolved { player, to } ->
            -- Use the post-group instance state to find which instance evolved.
            -- findEntryIndex would pick the first attachment entry by cardId, which
            -- may be a different instance than the one transferFirstInstance chose.
            case firstInstance player to.id postInstances of
                Nothing ->
                    state

                Just iid ->
                    case findEntryByInstance iid state of
                        Nothing ->
                            state

                        Just idx ->
                            updateAt idx (\e -> { e | cardId = to.id }) state

        Action.CardDiscardedFrom { card, pokemon } ->
            -- No position: try active first, then bench.
            let
                tryRemove pos =
                    case findEntryIndex pokemon.player pokemon.card.id pos state of
                        Just idx ->
                            let
                                entry =
                                    List.drop idx state |> List.head
                            in
                            case entry of
                                Just e ->
                                    if List.any (\c -> c.id == card.id) e.items then
                                        Just (updateAt idx (\en -> { en | items = removeFirstById card.id en.items }) state)

                                    else
                                        Nothing

                                Nothing ->
                                    Nothing

                        Nothing ->
                            Nothing
            in
            case tryRemove Action.ActiveSpot of
                Just updated ->
                    updated

                Nothing ->
                    case tryRemove Action.BenchSpot of
                        Just updated ->
                            updated

                        Nothing ->
                            state

        -- Bench ↔ Active movements: carry attachments along with the pokemon.
        Action.Switched { player, from, to } ->
            state
                |> moveAttachments player from.id Action.BenchSpot Action.ActiveSpot
                |> (if String.isEmpty to.id then
                        identity

                    else
                        moveAttachments player to.id Action.ActiveSpot Action.BenchSpot
                   )

        Action.MovedToActive { pokemon } ->
            -- Skip if a preceding Switched already put this pokemon in the active spot.
            -- Use postInstances to identify the SPECIFIC instance that became active,
            -- so we don't accidentally tag the wrong copy when two instances of the same
            -- card are both on the bench (e.g. after a same-card retreat).
            case Dict.get ( pokemon.player, pokemon.card.id ) preInstances.activeSpot |> Maybe.andThen identity of
                Just _ ->
                    state

                Nothing ->
                    case firstInstance pokemon.player pokemon.card.id postInstances of
                        Just iid ->
                            case findEntryByInstance iid state of
                                Just idx ->
                                    updateAt idx (\e -> { e | position = Action.ActiveSpot }) state

                                Nothing ->
                                    state

                        Nothing ->
                            moveAttachments pokemon.player pokemon.card.id Action.BenchSpot Action.ActiveSpot state

        Action.Retreated { player, card } ->
            -- Use preInstances to tag the SPECIFIC retreating instance as BenchSpot,
            -- rather than the first ActiveSpot entry (which could be a different copy).
            case Dict.get ( player, card.id ) preInstances.activeSpot |> Maybe.andThen identity of
                Just iid ->
                    case findEntryByInstance iid state of
                        Just idx ->
                            updateAt idx (\e -> { e | position = Action.BenchSpot }) state

                        Nothing ->
                            state

                Nothing ->
                    moveAttachments player card.id Action.ActiveSpot Action.BenchSpot state

        _ ->
            state


{-| Remove the first occurrence of a card with the given id from a list. -}
removeFirstById : String -> List Action.CardRef -> List Action.CardRef
removeFirstById targetId list =
    case list of
        [] ->
            []

        c :: rest ->
            if c.id == targetId then
                rest

            else
                c :: removeFirstById targetId rest


applyGroupToAttachments : InstanceState -> InstanceState -> Action.ActionGroup -> AttachmentState -> AttachmentState
applyGroupToAttachments preInstances postInstances group state =
    let
        state1 =
            applyActionToAttachments preInstances postInstances group.action state
    in
    List.foldl
        (\detail s ->
            let
                s1 =
                    applyActionToAttachments preInstances postInstances detail.action s

                s2 =
                    List.foldl (\bullet bs -> applyActionToAttachments preInstances postInstances bullet.action bs) s1 detail.bullets
            in
            case detail.action of
                Action.NCardsDiscardedFrom { pokemon } ->
                    -- Bullets list the individual cards discarded from a pokemon.
                    -- The source is always the pokemon that was active before this group
                    -- (retreat cost, KO discard, etc.). Look it up by instance ID from
                    -- preInstances so we don't accidentally strip a bench copy of the same card.
                    let
                        sourceIid =
                            Dict.get ( pokemon.player, pokemon.card.id ) preInstances.activeSpot
                                |> Maybe.andThen identity

                        removeFromSource card bs =
                            case sourceIid of
                                Just iid ->
                                    case findEntryByInstance iid bs of
                                        Just idx ->
                                            case List.drop idx bs |> List.head of
                                                Just e ->
                                                    if List.any (\c -> c.id == card.id) e.items then
                                                        updateAt idx (\en -> { en | items = removeFirstById card.id en.items }) bs
                                                    else
                                                        bs

                                                Nothing ->
                                                    bs

                                        Nothing ->
                                            -- The source instance's whole entry was already removed
                                            -- (e.g. by a preceding KnockedOut in the same group).
                                            -- Nothing left to strip — leave other instances alone.
                                            bs

                                Nothing ->
                                    -- Couldn't determine the source instance at all: fall back to
                                    -- position-based removal as a last resort.
                                    applyActionToAttachments
                                        preInstances
                                        postInstances
                                        (Action.CardDiscardedFrom { card = card, pokemon = pokemon })
                                        bs
                    in
                    List.foldl
                        (\bullet bs ->
                            List.foldl
                                (\card bbs -> removeFromSource card bbs)
                                bs
                                (bullet.raw |> String.split ", " |> List.filterMap Action.parseCardRef)
                        )
                        s2
                        detail.bullets

                _ ->
                    s2
        )
        state1
        group.details


computeAttachments : Replay.Players -> Replay.Replay -> Int -> Int -> AttachmentState
computeAttachments players replay sectionIndex groupIndex =
    let
        groups =
            collectAndCorrectGroups players replay sectionIndex groupIndex
    in
    Tuple.second
        (List.foldl
            (\group ( inst, atts ) ->
                let
                    newInst =
                        applyGroupToInstances group inst
                in
                ( newInst, applyGroupToAttachments inst newInst group atts )
            )
            ( emptyInstances, emptyAttachments )
            groups
        )


-- DAMAGE STATE


type alias DamageState =
    Dict InstanceId Int


emptyDamage : DamageState
emptyDamage =
    Dict.empty


addDamageHp : InstanceId -> Int -> DamageState -> DamageState
addDamageHp key amount state =
    let
        current =
            Dict.get key state |> Maybe.withDefault 0

        next =
            current + amount
    in
    if next <= 0 then
        Dict.remove key state

    else
        Dict.insert key next state


applyGroupToDamage : InstanceState -> InstanceState -> Action.ActionGroup -> DamageState -> DamageState
applyGroupToDamage preInstances postInstances group state =
    let
        -- Card IDs for which damage is explicitly prevented in this group's details
        preventedIds =
            group.details
                |> List.filterMap
                    (\d ->
                        case d.action of
                            Action.DamagePrevented { pokemon } ->
                                Just pokemon.id

                            _ ->
                                Nothing
                    )

        -- Apply main-action damage
        state1 =
            case group.action of
                Action.UsedAttack { target } ->
                    case target of
                        Just { defender, damage } ->
                            if List.member defender.card.id preventedIds then
                                state

                            else
                                case firstInstance defender.player defender.card.id postInstances of
                                    Nothing ->
                                        state

                                    Just iid ->
                                        addDamageHp iid damage state

                        Nothing ->
                            state

                Action.TookDamage { pokemon, amount } ->
                    case firstInstance pokemon.player pokemon.card.id postInstances of
                        Nothing ->
                            state

                        Just iid ->
                            addDamageHp iid amount state

                Action.PoisonCheckupDamage { pokemon, counters } ->
                    case firstInstance pokemon.player pokemon.card.id postInstances of
                        Nothing ->
                            state

                        Just iid ->
                            addDamageHp iid (counters * 10) state

                Action.KnockedOut { pokemon } ->
                    case firstInstance pokemon.player pokemon.card.id preInstances of
                        Nothing ->
                            state

                        Just iid ->
                            Dict.remove iid state

                Action.HealedDamage { pokemon, amount } ->
                    case firstInstance pokemon.player pokemon.card.id postInstances of
                        Nothing ->
                            state

                        Just iid ->
                            addDamageHp iid (negate amount) state

                Action.Evolved { player, from, to } ->
                    -- DamageState is keyed by InstanceId; transferFirstInstance keeps the same
                    -- iid under to.id in postInstances, so the entry remains valid as-is.
                    state

                _ ->
                    state

        -- Apply detail-action damage.
        -- Track per-(player,cardId) ordinals so repeated TookDamage/KnockedOut
        -- details for the same card (e.g. two Beldums hit) address different instances.
        state2 =
            List.foldl
                (\detail ( s, counts ) ->
                    let
                        ordinal key =
                            Dict.get key counts |> Maybe.withDefault 0

                        bump key =
                            Dict.insert key (ordinal key + 1) counts
                    in
                    case detail.action of
                        Action.TookDamage { pokemon, amount } ->
                            let
                                key =
                                    ( pokemon.player, pokemon.card.id )

                                n =
                                    ordinal key
                            in
                            case nthInstance pokemon.player pokemon.card.id n postInstances of
                                Nothing ->
                                    ( s, bump key )

                                Just iid ->
                                    ( addDamageHp iid amount s, bump key )

                        Action.PlacedDamageCounters { pokemon, count } ->
                            let
                                key =
                                    ( pokemon.player, pokemon.card.id )

                                n =
                                    ordinal key
                            in
                            case nthBenchInstance pokemon.player pokemon.card.id n postInstances of
                                Nothing ->
                                    ( s, bump key )

                                Just iid ->
                                    ( addDamageHp iid (count * 10) s, bump key )

                        Action.MovedDamageCounters { count, from, to } ->
                            let
                                s1 =
                                    case firstInstance from.player from.card.id postInstances of
                                        Nothing ->
                                            s

                                        Just iid ->
                                            addDamageHp iid (negate (count * 10)) s

                                s2 =
                                    case firstInstance to.player to.card.id postInstances of
                                        Nothing ->
                                            s1

                                        Just iid ->
                                            addDamageHp iid (count * 10) s1
                            in
                            ( s2, counts )

                        Action.KnockedOut { pokemon } ->
                            let
                                key =
                                    ( pokemon.player, pokemon.card.id )

                                n =
                                    ordinal key
                            in
                            case nthInstance pokemon.player pokemon.card.id n preInstances of
                                Nothing ->
                                    ( s, bump key )

                                Just iid ->
                                    ( Dict.remove iid s, bump key )

                        Action.HealedDamage { pokemon, amount } ->
                            case firstInstance pokemon.player pokemon.card.id postInstances of
                                Nothing ->
                                    ( s, counts )

                                Just iid ->
                                    ( addDamageHp iid (negate amount) s, counts )

                        Action.Evolved { player, from, to } ->
                            -- iid persists through evolution; no ordinal needed.
                            ( s, counts )

                        _ ->
                            ( s, counts )
                )
                ( state1, Dict.empty )
                group.details
                |> Tuple.first
    in
    state2


computeDamage : Replay.Players -> Replay.Replay -> Int -> Int -> DamageState
computeDamage players replay sectionIndex groupIndex =
    let
        groups =
            collectAndCorrectGroups players replay sectionIndex groupIndex
    in
    Tuple.second
        (List.foldl
            (\group ( inst, dmg ) ->
                let
                    pre =
                        inst

                    post =
                        applyGroupToInstances group inst
                in
                ( post, applyGroupToDamage pre post group dmg )
            )
            ( emptyInstances, emptyDamage )
            groups
        )


computeInstances : Replay.Players -> Replay.Replay -> Int -> Int -> InstanceState
computeInstances players replay sectionIndex groupIndex =
    List.foldl applyGroupToInstances emptyInstances
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
                    (correctGroupPlayers players emptyInstances group).details

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

                        Action.DrewAndPlayed rec ->
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

                extractBenched ds =
                    List.concatMap
                        (\d ->
                            case d.action of
                                Action.DrewCard drewCardBench ->
                                    case drewCardBench.andPlayed of
                                        Just Action.BenchSpot ->
                                            [ Just drewCardBench.card ]

                                        _ ->
                                            []

                                Action.DrewAndPlayed { count, position } ->
                                    case position of
                                        Action.BenchSpot ->
                                            let
                                                known =
                                                    detailCardList d
                                            in
                                            if List.isEmpty known then
                                                List.repeat count Nothing

                                            else
                                                List.map Just known

                                        _ ->
                                            []

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
                    , benched = extractBenched ds
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
                    { discarded = [], shuffled = [], drawn = drawnCards, benched = [] }
            in
            if List.isEmpty drawnCards then
                Nothing

            else if player == players.red then
                Just { player = player, card = Nothing, red = playerCards, blue = emptyPlayerCards }

            else
                Just { player = player, card = Nothing, red = emptyPlayerCards, blue = playerCards }

        Action.UsedAttack { attacker } ->
            let
                correctedDetails =
                    (correctGroupPlayers players emptyInstances group).details

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

                discardedFor p =
                    extractDiscards
                        (List.filter
                            (\d ->
                                case d.action of
                                    Action.DiscardedCard rec ->
                                        rec.player == p

                                    Action.Discarded rec ->
                                        rec.player == p

                                    _ ->
                                        False
                            )
                            correctedDetails
                        )

                redCards =
                    { discarded = discardedFor players.red, shuffled = [], drawn = [], benched = [] }

                blueCards =
                    { discarded = discardedFor players.blue, shuffled = [], drawn = [], benched = [] }
            in
            if List.isEmpty redCards.discarded && List.isEmpty blueCards.discarded then
                Nothing

            else
                Just { player = attacker.player, card = Nothing, red = redCards, blue = blueCards }

        _ ->
            Nothing


viewHandState : Replay.Players -> Dict String CardData -> Bool -> HandState -> BenchState -> ActiveState -> Maybe StadiumState -> InstanceState -> AttachmentState -> DamageState -> PileState -> Maybe CurrentPlay -> Html Msg
viewHandState players cache flipOpponent hand bench active maybeStadium instances attachments damageState piles maybePlay =
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

        -- When True, cards benched this turn are hidden from the bench row and
        -- shown only in the play-info "Benched" section. Disabled for now.
        stripBenchedFromBench =
            False

        benchBlueDisplay =
            if stripBenchedFromBench then
                case maybePlay of
                    Just play ->
                        stripBenchedFromBenchSide play.blue.benched bench.blue

                    Nothing ->
                        bench.blue

            else
                bench.blue

        benchRedDisplay =
            if stripBenchedFromBench then
                case maybePlay of
                    Just play ->
                        stripBenchedFromBenchSide play.red.benched bench.red

                    Nothing ->
                        bench.red

            else
                bench.red
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
                [ viewHandRow "RED" flipOpponent "#c53030" "flex-end" blueDisplay (handCardImage cache)
                , viewBenchRow flipOpponent cache "rgba(197, 48, 48, 0.08)" instances attachments damageState players.blue benchBlueDisplay
                , viewActiveZone players cache flipOpponent active maybeStadium instances attachments damageState maybePlay
                , viewBenchRow False cache "rgba(44, 82, 130, 0.08)" instances attachments damageState players.red benchRedDisplay
                , viewHandRow "BLUE" False "#2c5282" "flex-start" redDisplay (handCardImage cache)
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
        ]


viewPileStack : String -> Int -> String -> String -> Html Msg
viewPileStack label count bgColor textColor =
    div
        [ style "width" cardW
        , style "height" cardH
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
        [ style "width" cardW
        , style "height" cardH
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


viewHandRow : String -> Bool -> String -> String -> List (Maybe Action.CardRef) -> (Maybe Action.CardRef -> Maybe String) -> Html Msg
viewHandRow playerName upsideDown color alignItems cards imageFor =
    div
        [ style "display" "flex"
        , style "align-items" alignItems
        , style "gap" "0.35rem"
        , style "min-height" handCardH
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
            [ style "overflow-x" "auto"
            , style "flex" "1"
            , style "min-width" "0"
            , style "min-height" handCardH
            -- Scrollbar clearance always on the side away from the bench:
            -- flex-end (opponent, bench below) → padding-top keeps cards flush at bottom.
            -- flex-start (player, bench above) → padding-bottom keeps cards flush at top.
            , if alignItems == "flex-end" then style "padding-top" "4px" else style "padding-bottom" "4px"
            ]
            [ div
                -- Inner wrapper: centered via margin auto when cards fit, but
                -- flex-shrink:0 keeps it at natural width when they overflow so
                -- the outer scroll container can reach the first card on the left.
                [ style "display" "flex"
                , style "align-items" "center"
                , style "gap" "0.35rem"
                , style "flex-shrink" "0"
                , style "margin" "0 auto"
                , style "width" "fit-content"
                ]
                (List.map
                    (\item ->
                        case item of
                            KnownPlayCard card ->
                                viewHandCard upsideDown color imageFor (Just card)

                            UnknownPlayCards n ->
                                viewUnknownCardBack handCardW handCardH upsideDown n
                    )
                    (collapseUnknowns cards)
                )
            ]
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


{-| Look up a card's image URL from the cache, falling back to a name-based
search when the card has a name-as-ID (e.g. from a no-ID log or a stadium
re-use line). This is the single point of truth for card→image resolution.
-}
cachedImageUrl : Dict String CardData -> Action.CardRef -> Maybe String
cachedImageUrl cache card =
    case Dict.get card.id cache |> Maybe.andThen .imageUrl of
        Just url ->
            Just url

        Nothing ->
            -- Name-as-ID fallback: find a cached entry whose name matches.
            Dict.values cache
                |> List.filterMap
                    (\data ->
                        if data.name == Just card.name then
                            data.imageUrl

                        else
                            Nothing
                    )
                |> List.head


handCardImage : Dict String CardData -> Maybe Action.CardRef -> Maybe String
handCardImage cache maybeCard =
    case maybeCard of
        Nothing ->
            Nothing

        Just card ->
            cachedImageUrl cache card
                |> Maybe.map (\u -> u ++ "/low.webp")


viewHandCard : Bool -> String -> (Maybe Action.CardRef -> Maybe String) -> Maybe Action.CardRef -> Html Msg
viewHandCard upsideDown color imageFor maybeCard =
    let
        -- Cards show only the top half, scaled up so the art fills the viewport
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
            [ style "width" handCardW
            , style "height" handCardH
            , style "border-radius" radius
            , style "flex-shrink" "0"
            , style "box-sizing" "border-box"
            ]
    in
    case maybeCard of
        Just card ->
            case imageFor maybeCard of
                Just imageUrl ->
                    div
                        (baseStyles
                            ++ rotationStyles
                            ++ [ style "background-image" ("url('" ++ imageUrl ++ "')")
                               , style "background-size" "cover"
                               , style "background-position" "top center"
                               , style "background-color" "#e2e8f0"
                               , style "cursor" "pointer"
                               , onClick (CardClicked card.id card.name)
                               ]
                        )
                        []

                Nothing ->
                    viewNoImageCard
                        (baseStyles
                            ++ rotationStyles
                            ++ [ style "cursor" "pointer"
                               , onClick (CardClicked card.id card.name)
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


viewBenchRow : Bool -> Dict String CardData -> String -> InstanceState -> AttachmentState -> DamageState -> String -> List Action.CardRef -> Html Msg
viewBenchRow upsideDown cache bgColor instances attachments damageState player cards =
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
            , style "gap" "14px"
            , style "overflow-x" "auto"
            , style "flex" "1"
            , style "min-width" "0"
            , style "min-height" cardH
            , style "padding" "10px 10px 14px 14px"
            , style "background" bgColor
            , style "border-radius" "6px"
            ]
            (Tuple.first
                (List.foldl
                    (\card ( rendered, counts ) ->
                        let
                            ordinal =
                                Dict.get card.id counts |> Maybe.withDefault 0

                            maybeIid =
                                instanceIdForField instances player card.id ordinal

                            hp =
                                maybeIid |> Maybe.andThen (\iid -> Dict.get iid damageState) |> Maybe.withDefault 0

                            atts =
                                maybeIid |> Maybe.map (lookupAttachments attachments) |> Maybe.withDefault []

                            cardHtml =
                                viewBenchCard upsideDown cache atts hp card
                        in
                        ( rendered ++ [ cardHtml ]
                        , Dict.insert card.id (ordinal + 1) counts
                        )
                    )
                    ( [], Dict.empty )
                    cards
                )
            )
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
         , style "align-items" "flex-start"
         , style "justify-content" "center"
         , style "text-align" "center"
         , style "font-size" "0.6rem"
         , style "font-weight" "600"
         , style "line-height" "1.3"
         , style "padding" "4px"
         , style "padding-top" "8px"
         , style "overflow" "hidden"
         ]
            ++ extraStyles
        )
        [ text name ]


{-| True if the attachment item is an Energy card.
Checks the cached category first; falls back to name-based heuristic when the
card is not yet in the cache (or has no category). -}
isEnergyAttachment : Dict String CardData -> Action.CardRef -> Bool
isEnergyAttachment cache item =
    case Dict.get item.id cache of
        Just cardData ->
            case cardData.category of
                Just cat ->
                    cat == "Energy"

                Nothing ->
                    String.contains "energy" (String.toLower item.name)

        Nothing ->
            String.contains "energy" (String.toLower item.name)


viewAttachmentCircle : Dict String CardData -> Action.CardRef -> Html Msg
viewAttachmentCircle cache item =
    let
        maybeUrl =
            cachedImageUrl cache item
                |> Maybe.map (\u -> u ++ "/high.webp")

        isBasicEnergy =
            basicEnergyImageUrl item.name /= Nothing

        bgPosition =
            if isBasicEnergy then
                "center 55%"

            else
                "center 30%"
    in
    div
        [ style "width" "18px"
        , style "height" "18px"
        , style "border-radius" "50%"
        , style "flex-shrink" "0"
        , style "background-color" "#e2e8f0"
        , style "border" "1.5px solid rgba(255,255,255,0.85)"
        , style "box-shadow" "0 1px 3px rgba(0,0,0,0.35)"
        , style "overflow" "hidden"
        , style "cursor" "pointer"
        , onClick (CardClicked item.id item.name)
        ]
        [ case maybeUrl of
            Just u ->
                div
                    [ style "width" "100%"
                    , style "height" "100%"
                    , style "background-image" ("url('" ++ u ++ "')")
                    , style "background-size" "150%"
                    , style "background-position" bgPosition
                    ]
                    []

            Nothing ->
                case basicEnergyColor item.name of
                    Just color ->
                        div
                            [ style "width" "100%"
                            , style "height" "100%"
                            , style "background" color
                            ]
                            []

                    Nothing ->
                        div
                            [ style "width" "100%"
                            , style "height" "100%"
                            , style "display" "flex"
                            , style "align-items" "center"
                            , style "justify-content" "center"
                            , style "font-size" "9px"
                            , style "font-weight" "700"
                            , style "color" "#4a5568"
                            , style "line-height" "1"
                            ]
                            [ text (abbreviateCardName item.name) ]
        ]


viewAttachmentRect : Dict String CardData -> Action.CardRef -> Html Msg
viewAttachmentRect cache item =
    let
        maybeUrl =
            cachedImageUrl cache item
                |> Maybe.map (\u -> u ++ "/high.webp")
    in
    div
        [ style "width" "20px"
        , style "height" "14px"
        , style "border-radius" "2px"
        , style "flex-shrink" "0"
        , style "background-color" "#e2e8f0"
        , style "border" "1.5px solid rgba(255,255,255,0.85)"
        , style "box-shadow" "0 1px 3px rgba(0,0,0,0.35)"
        , style "overflow" "hidden"
        , style "cursor" "pointer"
        , onClick (CardClicked item.id item.name)
        ]
        [ case maybeUrl of
            Just u ->
                div
                    [ style "width" "100%"
                    , style "height" "100%"
                    , style "background-image" ("url('" ++ u ++ "')")
                    , style "background-size" "150%"
                    , style "background-position" "center 20%"
                    ]
                    []

            Nothing ->
                div
                    [ style "width" "100%"
                    , style "height" "100%"
                    , style "display" "flex"
                    , style "align-items" "center"
                    , style "justify-content" "center"
                    , style "font-size" "8px"
                    , style "font-weight" "700"
                    , style "color" "#4a5568"
                    , style "line-height" "1"
                    , style "text-align" "center"
                    ]
                    [ text (abbreviateCardName item.name) ]
        ]


viewBenchCard : Bool -> Dict String CardData -> List Action.CardRef -> Int -> Action.CardRef -> Html Msg
viewBenchCard upsideDown cache cardAttachments hpDamage card =
    let
        maybeUrl =
            cachedImageUrl cache card
                |> Maybe.map (\u -> u ++ "/high.webp")

        rotStyles =
            if upsideDown then
                [ style "transform" "rotate(180deg)" ]

            else
                []

        -- Styles for the card image itself (fills the wrapper)
        cardStyles =
            [ style "width" "100%"
            , style "height" "100%"
            , style "border-radius" "4px"
            , style "box-sizing" "border-box"
            , style "cursor" "pointer"
            , onClick (CardClicked card.id card.name)
            ]

        cardDiv =
            case maybeUrl of
                Just u ->
                    div
                        (cardStyles
                            ++ rotStyles
                            ++ [ style "background-image" ("url('" ++ u ++ "')")
                               , style "background-size" "cover"
                               , style "background-position" "center"
                               , style "background-color" "#e2e8f0"
                               ]
                        )
                        []

                Nothing ->
                    viewNoImageCard (cardStyles ++ rotStyles) card.name

    in
    let
        energyAttachments =
            cardAttachments
                |> List.filter (isEnergyAttachment cache)
                |> List.sortWith
                    (\a b ->
                        let
                            rank x =
                                if basicEnergyImageUrl x.name /= Nothing then
                                    0

                                else
                                    1
                        in
                        case compare (rank a) (rank b) of
                            EQ ->
                                compare a.name b.name

                            other ->
                                other
                    )

        toolAttachments =
            List.filter (\a -> not (isEnergyAttachment cache a)) cardAttachments

        groupedEnergies =
            List.foldl
                (\item acc ->
                    case acc of
                        ( lastRef, count ) :: rest ->
                            if lastRef.id == item.id then
                                ( lastRef, count + 1 ) :: rest

                            else
                                ( item, 1 ) :: acc

                        [] ->
                            [ ( item, 1 ) ]
                )
                []
                energyAttachments
                |> List.reverse

        energyOverlay =
            if List.isEmpty groupedEnergies then
                []
            else
                [ div
                    [ style "position" "absolute"
                    , style "bottom" "-9px"
                    , style "left" "-9px"
                    , style "display" "flex"
                    , style "flex-direction" "row"
                    , style "gap" "2px"
                    ]
                    (List.map
                        (\( item, count ) ->
                            div
                                [ style "position" "relative" ]
                                (viewAttachmentCircle cache item
                                    :: (if count > 1 then
                                            [ div
                                                [ style "position" "absolute"
                                                , style "top" "0"
                                                , style "left" "0"
                                                , style "width" "100%"
                                                , style "height" "100%"
                                                , style "display" "flex"
                                                , style "align-items" "center"
                                                , style "justify-content" "center"
                                                , style "font-size" "10px"
                                                , style "font-weight" "700"
                                                , style "color" "white"
                                                , style "text-shadow" "0 0 3px rgba(0,0,0,0.8)"
                                                , style "pointer-events" "none"
                                                ]
                                                [ text (String.fromInt count) ]
                                            ]

                                        else
                                            []
                                       )
                                )
                        )
                        groupedEnergies
                    )
                ]

        toolOverlay =
            if List.isEmpty toolAttachments then
                []
            else
                [ div
                    [ style "position" "absolute"
                    , style "top" "25%"
                    , style "left" "-10px"
                    , style "display" "flex"
                    , style "flex-direction" "column"
                    , style "gap" "2px"
                    ]
                    (List.map (viewAttachmentRect cache) toolAttachments)
                ]
    in
    let
        damageOverlay =
            if hpDamage > 0 then
                [ div
                    [ style "position" "absolute"
                    , style "top" "25%"
                    , style "right" "2px"
                    , style "background" "#d69e2e"
                    , style "color" "white"
                    , style "border-radius" "50%"
                    , style "width" "28px"
                    , style "height" "28px"
                    , style "font-size" "0.65rem"
                    , style "font-weight" "700"
                    , style "display" "flex"
                    , style "align-items" "center"
                    , style "justify-content" "center"
                    , style "pointer-events" "none"
                    , style "flex-shrink" "0"
                    , style "border" "1.5px solid rgba(0,0,0,0.55)"
                    , style "text-shadow" "0 0 3px rgba(0,0,0,0.9), 0 1px 3px rgba(0,0,0,0.9)"
                    ]
                    [ text (String.fromInt hpDamage) ]
                ]

            else
                []
    in
    div
        [ style "position" "relative"
        , style "width" cardW
        , style "height" cardH
        , style "flex-shrink" "0"
        ]
        (cardDiv :: energyOverlay ++ toolOverlay ++ damageOverlay)



{-| The active zone combines both active spots and the stadium into one row.
Active spots are stacked vertically in the center. Stadium slots sit two card-widths
out on each side: blue's on the left (upside-down), red's on the right.
-}
viewActiveZone : Replay.Players -> Dict String CardData -> Bool -> ActiveState -> Maybe StadiumState -> InstanceState -> AttachmentState -> DamageState -> Maybe CurrentPlay -> Html Msg
viewActiveZone players cache flipOpponent active maybeStadium instances attachments damageState maybePlay =
    let
        red =
            players.red

        stadiumSlot maybeEntry =
            case maybeEntry of
                Just ( card, upsideDown, shadowColor ) ->
                    div
                        [ style "width" cardW
                        , style "height" cardH
                        , style "border-radius" "4px"
                        , style "flex-shrink" "0"
                        , style "box-shadow" ("0 0 0 4px " ++ shadowColor)
                        , style "overflow" "hidden"
                        ]
                        [ viewBenchCard upsideDown cache [] 0 card ]

                Nothing ->
                    div
                        [ style "width" cardW
                        , style "height" cardH
                        , style "border-radius" "4px"
                        , style "flex-shrink" "0"
                        , style "border" "2px dashed #cbd5e0"
                        , style "box-sizing" "border-box"
                        ]
                        []

        activeCard upsideDown activePlayer maybeCard =
            case maybeCard of
                Just card ->
                    let
                        maybeIid =
                            firstInstance activePlayer card.id instances

                        hp =
                            maybeIid |> Maybe.andThen (\iid -> Dict.get iid damageState) |> Maybe.withDefault 0

                        atts =
                            maybeIid |> Maybe.map (lookupAttachments attachments) |> Maybe.withDefault []
                    in
                    viewBenchCard upsideDown cache atts hp card

                Nothing ->
                    div
                        [ style "width" cardW
                        , style "height" cardH
                        , style "border-radius" "4px"
                        , style "flex-shrink" "0"
                        , style "border" "2px dashed #cbd5e0"
                        , style "box-sizing" "border-box"
                        ]
                        []

        -- Single stadium slot: flip if the opponent played it; border color by player
        stadiumEntry =
            case maybeStadium of
                Just s ->
                    let
                        upsideDown =
                            flipOpponent && s.player /= red

                        borderColor =
                            if s.player == red then
                                "rgba(44, 82, 130, 0.45)"

                            else
                                "rgba(197, 48, 48, 0.45)"
                    in
                    Just ( s.card, upsideDown, borderColor )

                Nothing ->
                    Nothing
    in
    div
        [ style "display" "flex"
        , style "align-items" "center"
        , style "gap" "0.35rem"
        , style "min-width" "0"
        , style "padding-bottom" "1rem"
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

        -- 5-column grid:  1fr | 72px gap | auto (active) | 72px gap | 1fr
        -- The outer 1fr columns are equal, so the auto active column is exactly
        -- centered. The 72px columns provide one-card spacing on each side.
        -- Stadium spans both rows in col 1; play info rows land in col 5.
        -- row-gap: 0.4rem separates the two rows.
        -- align-items: start + padding-top: 1rem on active + line-height: 1 on
        -- play-info labels keeps card images pixel-aligned across columns.
        , let
            isTookPrize =
                Maybe.map isTookPrizePlay maybePlay |> Maybe.withDefault False

            bluePlay =
                case maybePlay of
                    Just play ->
                        let bluePlayedCard = if play.player /= red then play.card else Nothing
                        in viewPlayerPlayInfo cache flipOpponent isTookPrize "#c53030" play.blue bluePlayedCard
                    Nothing ->
                        text ""

            redPlay =
                case maybePlay of
                    Just play ->
                        let redPlayedCard = if play.player == red then play.card else Nothing
                        in viewPlayerPlayInfo cache False isTookPrize "#2c5282" play.red redPlayedCard
                    Nothing ->
                        text ""
          in
          div
            [ style "display" "grid"
            , style "grid-template-columns" "minmax(0,1fr) 72px auto 72px minmax(0,1fr)"
            , style "grid-template-rows" (activeRowH ++ " " ++ activeRowH)
            , style "row-gap" "0.4rem"
            , style "align-items" "end"
            , style "flex" "1"
            , style "min-width" "0"
            ]
            [ -- Stadium: col 1, spans both rows, vertically centered between them
              div
                [ style "grid-column" "1"
                , style "grid-row" "1 / 3"
                , style "align-self" "center"
                , style "display" "flex"
                , style "justify-content" "flex-end"
                , style "align-items" "center"
                ]
                [ stadiumSlot stadiumEntry ]

            -- Blue active: col 3, row 1
            , div
                [ style "grid-column" "3"
                , style "grid-row" "1"
                ]
                [ activeCard flipOpponent players.blue active.blue ]

            -- Red active: col 3, row 2
            , div
                [ style "grid-column" "3"
                , style "grid-row" "2"
                ]
                [ activeCard False players.red active.red ]

            -- Blue play info: col 5, row 1  (clip so tall content never bleeds into the other row)
            , div
                [ style "grid-column" "5"
                , style "grid-row" "1"
                , style "overflow" "hidden"
                , style "min-width" "0"
                ]
                [ bluePlay ]

            -- Red play info: col 5, row 2  (clip so tall content never bleeds into the other row)
            , div
                [ style "grid-column" "5"
                , style "grid-row" "2"
                , style "overflow" "hidden"
                , style "min-width" "0"
                ]
                [ redPlay ]
            ]
        ]


{-| A card thumbnail at the standard hand size, used for both the hand panel
and the played-card panel. Always an <img> with a gray background placeholder
so there is no flicker when the image loads.
-}
viewKnownCardThumb : Bool -> Dict String CardData -> Action.CardRef -> Html Msg
viewKnownCardThumb upsideDown cache card =
    let
        maybeUrl =
            cachedImageUrl cache card
                |> Maybe.map (\u -> u ++ "/low.webp")

        baseStyles =
            [ style "width" cardW
            , style "height" cardH
            , style "border-radius" "4px"
            , style "flex-shrink" "0"
            , style "box-sizing" "border-box"
            , style "cursor" "pointer"
            , onClick (CardClicked card.id card.name)
            ]
            ++ (if upsideDown then [ style "transform" "rotate(180deg)" ] else [])
    in
    case maybeUrl of
        Just imageUrl ->
            div
                (baseStyles
                    ++ [ style "background-image" ("url('" ++ imageUrl ++ "')")
                       , style "background-size" "cover"
                       , style "background-position" "center"
                       , style "background-color" "#e2e8f0"
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
    let
        known =
            List.filterMap (Maybe.map KnownPlayCard) cards

        unknownCount =
            List.length (List.filter ((==) Nothing) cards)
    in
    if unknownCount > 0 then
        UnknownPlayCards unknownCount :: known

    else
        known


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


{-| Render the play info (played card + discarded / shuffled / drawn) for a
single player next to their active card slot. Returns `text ""` when there is
nothing to show for that player.
-}
viewPlayerPlayInfo : Dict String CardData -> Bool -> Bool -> String -> PlayerCards -> Maybe Action.CardRef -> Html Msg
viewPlayerPlayInfo cache upsideDown isTookPrize color playerCards maybePlayedCard =
    let
        viewPlayItem item =
            case item of
                KnownPlayCard card ->
                    viewKnownCardThumb upsideDown cache card

                UnknownPlayCards n ->
                    viewUnknownCardBack cardW cardH upsideDown n

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
                    , style "color" color
                    , style "line-height" "1"
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
                    ++ optionalGroup "Benched" playerCards.benched
    in
    if List.isEmpty cardGroups then
        text ""

    else
        div
            [ style "display" "flex"
            , style "flex-direction" "row"
            , style "align-items" "flex-start"
            , style "gap" "0.5rem"
            , style "overflow-x" "auto"
            , Html.Attributes.class "play-info-scroll"
            ]
            cardGroups


isTookPrizePlay : CurrentPlay -> Bool
isTookPrizePlay play =
    play.card == Nothing
        && List.isEmpty play.red.discarded
        && List.isEmpty play.blue.discarded


viewCurrentPlay : Replay.Players -> Dict String CardData -> CurrentPlay -> Html Msg
viewCurrentPlay players cache play =
    let
        isTookPrize =
            isTookPrizePlay play

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
    in
    div
        [ style "display" "flex"
        , style "flex-direction" "row"
        , style "align-items" "flex-start"
        , style "gap" "0.75rem"
        , style "overflow-x" "auto"
        , style "padding-bottom" "4px"
        ]
        [ viewPlayerPlayInfo cache False isTookPrize "#c53030" play.blue bluePlayedCard
        , viewPlayerPlayInfo cache False isTookPrize "#2c5282" play.red redPlayedCard
        ]


-- VIEW


{- Card dimension constants.
   All card sizes are derived from `cardH` (14 vh) so they scale with
   the viewport and fill the available screen space.

   Bench / active / pile cards  – portrait, aspect ratio 0.72 : 1
   Hand cards                   – landscape crop, aspect ratio 0.86 : 0.60
-}


cardH : String
cardH =
    "14vh"


cardW : String
cardW =
    "calc(14vh * 0.72)"


handCardH : String
handCardH =
    "calc(14vh * 0.60)"


handCardW : String
handCardW =
    "calc(14vh * 0.86)"


{-| Height of one active-zone grid row: card height plus 1 rem for the
play-info label that floats above the card.
-}
activeRowH : String
activeRowH =
    "calc(14vh + 1rem)"


view : Model -> Html Msg
view model =
    div
        [ style "font-family" "system-ui, -apple-system, sans-serif"
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
                    Loaded _ replay sectionIndex groupIndex _ cache ctx ->
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

                                    attachments =
                                        computeAttachments players replay sectionIndex groupIndex

                                    damageState =
                                        computeDamage players replay sectionIndex groupIndex

                                    instances =
                                        computeInstances players replay sectionIndex groupIndex

                                    maybePlay =
                                        getCurrentGroup replay sectionIndex groupIndex
                                            |> Maybe.andThen (currentPlayFromGroup players)
                                in
                                viewHandState players cache ctx.flipOpponent hand bench activeSpots stadium instances attachments damageState piles maybePlay

                            Nothing ->
                                text ""

                    _ ->
                        text ""
                ]

            -- Right column: settings + action log
            , div
                [ style "flex" "0 0 320px"
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

        debug =
            currentDebug model
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
            , if debug then
                span
                    [ style "padding" "0.1rem 0.4rem"
                    , style "border-radius" "4px"
                    , style "background" "rgba(237, 137, 54, 0.15)"
                    , style "border" "1px solid #ed8936"
                    , style "font-size" "0.65rem"
                    , style "color" "#c05621"
                    , style "font-weight" "700"
                    , style "letter-spacing" "0.05em"
                    , style "user-select" "none"
                    ]
                    [ text "DEBUG" ]

              else
                text ""
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

        Loaded _ replay index groupIndex _ cache ctx ->
            viewReplay ctx cache replay index groupIndex

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


viewReplay : ViewContext -> Dict String CardData -> Replay.Replay -> Int -> Int -> Html Msg
viewReplay ctx cache replay sectionIndex groupIndex =
    let
        players =
            replay.players

        total =
            List.length replay.sections

        -- Fold over all sections, accumulating instance state, to produce corrected
        -- groups with their per-group preInstances for use in both display and correction.
        sectionsWithPres : List ( Replay.Section, List ( Action.ActionGroup, InstanceState ) )
        sectionsWithPres =
            case players of
                Nothing ->
                    List.map (\sec -> ( sec, [] )) replay.sections

                Just ps ->
                    List.foldl
                        (\section ( state, acc ) ->
                            let
                                rawGroups =
                                    Action.groupLines (sectionLines section)

                                ( newState, revGroupsWithPre ) =
                                    List.foldl
                                        (\group ( s, gacc ) ->
                                            let
                                                corrected =
                                                    correctGroupPlayers ps s group

                                                nextState =
                                                    applyGroupToInstances corrected s
                                            in
                                            ( nextState, ( group, s ) :: gacc )
                                        )
                                        ( state, [] )
                                        rawGroups
                            in
                            ( newState, ( section, List.reverse revGroupsWithPre ) :: acc )
                        )
                        ( emptyInstances, [] )
                        replay.sections
                        |> Tuple.second
                        |> List.reverse

        currentSectionWithPre =
            sectionsWithPres |> List.drop sectionIndex |> List.head

        currentSection =
            Maybe.map Tuple.first currentSectionWithPre

        pastSectionsWithPre =
            List.take sectionIndex sectionsWithPres

        -- Nav bar info for the current section
        ( badge, extra, borderColor ) =
            sectionNavInfo players currentSection

        -- Current section: visible groups reversed (most recent at top)
        currentContent =
            case currentSectionWithPre of
                Nothing ->
                    []

                Just ( section, groupsWithPre ) ->
                    case section of
                        Replay.ResultSection result ->
                            [ viewResultContent players result ]

                        _ ->
                            List.take (groupIndex + 1) groupsWithPre
                                |> List.reverse
                                |> List.indexedMap
                                    (\i ( group, pre ) ->
                                        div
                                            (if i > 0 then
                                                [ style "opacity" "0.4" ]

                                             else
                                                []
                                            )
                                            (viewActionGroup ctx pre players cache group)
                                    )

        -- Past sections: most recent first, each preceded by a divider
        pastContent =
            pastSectionsWithPre
                |> List.reverse
                |> List.concatMap
                    (\( section, groupsWithPre ) ->
                        viewSectionDivider players section
                            :: viewPastSectionGroups ctx cache players section groupsWithPre
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


viewPastSectionGroups : ViewContext -> Dict String CardData -> Maybe Replay.Players -> Replay.Section -> List ( Action.ActionGroup, InstanceState ) -> List (Html Msg)
viewPastSectionGroups ctx cache players section groupsWithPre =
    let
        greyed children =
            div [ style "opacity" "0.4" ] children
    in
    case section of
        Replay.ResultSection result ->
            [ greyed [ viewResultContent players result ] ]

        _ ->
            groupsWithPre
                |> List.reverse
                |> List.map (\( group, pre ) -> greyed (viewActionGroup ctx pre players cache group))


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


viewActionGroup : ViewContext -> InstanceState -> Maybe Replay.Players -> Dict String CardData -> Action.ActionGroup -> List (Html Msg)
viewActionGroup ctx preInstances players cache group =
    let
        correctedGroup =
            case players of
                Just ps ->
                    correctGroupPlayers ps preInstances group

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

        -- Per-detail flag: True when correctGroupPlayers changed the raw text.
        wasDetailCorrected =
            List.map2 (\orig corr -> orig.raw /= corr.raw) group.details correctedGroup.details

        viewDetailLines wasCorrected detail =
            let
                detailLine =
                    viewLine players Nothing (Replay.DetailLine detail.raw)

                wrappedLine =
                    if ctx.debug && wasCorrected then
                        div
                            [ style "background-color" "rgba(237, 137, 54, 0.15)"
                            , style "border-left" "3px solid #ed8936"
                            , style "padding-left" "4px"
                            , style "margin-left" "-4px"
                            ]
                            [ detailLine ]

                    else
                        detailLine
            in
            wrappedLine :: List.map (\bullet -> viewLine players Nothing (Replay.BulletLine bullet.raw)) detail.bullets

        normalDetails =
            List.concat (List.map2 viewDetailLines wasDetailCorrected correctedGroup.details)
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
                                List.concat (List.map2 viewDetailLines wasDetailCorrected correctedGroup.details)
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
                [ onClick (CardClicked id name)
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
            FetchingCard _ fallbackName ->
                div
                    [ style "color" "white"
                    , style "font-style" "italic"
                    , style "font-size" "1rem"
                    ]
                    [ text (fallbackName ++ "…") ]

            FetchingMove _ moveName ->
                div
                    [ style "color" "white"
                    , style "font-style" "italic"
                    , style "font-size" "1rem"
                    ]
                    [ text (moveName ++ "…") ]

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
