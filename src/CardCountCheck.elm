port module CardCountCheck exposing (main)

import Action
import Dict exposing (Dict)
import Main
    exposing
        ( ActiveState
        , AttachmentState
        , BenchState
        , HandState
        , PileState
        , StadiumState
        , applyGroupToActive
        , applyGroupToAttachments
        , applyGroupToBench
        , applyGroupToHand
        , applyGroupToPiles
        , applyGroupToStadium
        , correctGroupPlayers
        , emptyActive
        , emptyAttachments
        , emptyBench
        , emptyHand
        , emptyPiles
        , lookupAttachments
        , sectionLines
        )
import Platform
import Replay exposing (Section(..))
import Url


port done : { output : String, ok : Bool } -> Cmd msg


type alias FileInput =
    { name : String
    , content : String
    , replayUrl : String
    }


main : Platform.Program FileInput () Never
main =
    Platform.worker
        { init = \flags -> ( (), done (checkFile flags) )
        , update = \_ model -> ( model, Cmd.none )
        , subscriptions = \_ -> Sub.none
        }



-- GAME STATE


type alias GameState =
    { piles : PileState
    , hand : HandState
    , bench : BenchState
    , active : ActiveState
    , stadium : Maybe StadiumState
    , attachments : AttachmentState
    , evolution : EvolutionState
    }


type alias EvolutionState =
    { red : Dict String Int
    , blue : Dict String Int
    }


emptyEvolution : EvolutionState
emptyEvolution =
    { red = Dict.empty, blue = Dict.empty }


initialState : GameState
initialState =
    { piles = emptyPiles
    , hand = emptyHand
    , bench = emptyBench
    , active = emptyActive
    , stadium = Nothing
    , attachments = emptyAttachments
    , evolution = emptyEvolution
    }


applyActionToEvolution : String -> Action.Action -> EvolutionState -> EvolutionState
applyActionToEvolution red action evo =
    case action of
        Action.Evolved { player, from, to } ->
            let
                dict =
                    if player == red then
                        evo.red

                    else
                        evo.blue

                fromDepth =
                    Dict.get from.id dict |> Maybe.withDefault 0

                newDict =
                    dict
                        |> Dict.remove from.id
                        |> Dict.insert to.id (fromDepth + 1)
            in
            if player == red then
                { evo | red = newDict }

            else
                { evo | blue = newDict }

        Action.KnockedOut { pokemon } ->
            let
                dict =
                    if pokemon.player == red then
                        evo.red

                    else
                        evo.blue

                newDict =
                    Dict.remove pokemon.card.id dict
            in
            if pokemon.player == red then
                { evo | red = newDict }

            else
                { evo | blue = newDict }

        _ ->
            evo


applyGroupToEvolution : String -> EvolutionState -> Action.ActionGroup -> EvolutionState
applyGroupToEvolution red evo group =
    let
        evo1 =
            applyActionToEvolution red group.action evo
    in
    List.foldl
        (\detail acc ->
            List.foldl
                (\bullet a -> applyActionToEvolution red bullet.action a)
                (applyActionToEvolution red detail.action acc)
                detail.bullets
        )
        evo1
        group.details


stepGroup : String -> Bool -> Action.ActionGroup -> GameState -> GameState
stepGroup red isSetup group gs =
    { piles = applyGroupToPiles red isSetup gs.piles group
    , hand = applyGroupToHand red gs.hand group
    , bench = applyGroupToBench red gs.active gs.bench group
    , active = applyGroupToActive red gs.active group
    , stadium = applyGroupToStadium gs.stadium group
    , attachments = applyGroupToAttachments group gs.attachments
    , evolution = applyGroupToEvolution red gs.evolution group
    }



-- COUNTING


type alias PlayerBreakdown =
    { deck : Int
    , discard : Int
    , prizes : Int
    , hand : Int
    , active : Int
    , bench : Int
    , attachments : Int
    , stadium : Int
    , evolutionBuried : Int
    , total : Int
    }


attachmentCountForSide :
    String
    -> List Action.CardRef
    -> Maybe Action.CardRef
    -> AttachmentState
    -> Int
attachmentCountForSide player benchCards maybeActive state =
    let
        activeCount =
            case maybeActive of
                Just card ->
                    List.length (lookupAttachments state player card.id Action.ActiveSpot 0)

                Nothing ->
                    0

        ( benchCount, _ ) =
            List.foldl
                (\card ( total, counts ) ->
                    let
                        ordinal =
                            Dict.get card.id counts |> Maybe.withDefault 0

                        itemCount =
                            List.length (lookupAttachments state player card.id Action.BenchSpot ordinal)
                    in
                    ( total + itemCount, Dict.insert card.id (ordinal + 1) counts )
                )
                ( 0, Dict.empty )
                benchCards
    in
    activeCount + benchCount


breakdownForRed : String -> GameState -> PlayerBreakdown
breakdownForRed red gs =
    let
        deckVal =
            gs.piles.deckRed

        discardVal =
            gs.piles.discardRed

        prizesVal =
            gs.piles.prizesRed

        handVal =
            List.length gs.hand.red

        activeVal =
            case gs.active.red of
                Just _ ->
                    1

                Nothing ->
                    0

        benchVal =
            List.length gs.bench.red

        attachmentsVal =
            attachmentCountForSide red gs.bench.red gs.active.red gs.attachments

        stadiumVal =
            case gs.stadium of
                Just s ->
                    if s.player == red then
                        1

                    else
                        0

                Nothing ->
                    0

        evolutionBuriedVal =
            Dict.values gs.evolution.red |> List.sum

        totalVal =
            deckVal + discardVal + prizesVal + handVal + activeVal + benchVal + attachmentsVal + stadiumVal + evolutionBuriedVal
    in
    { deck = deckVal
    , discard = discardVal
    , prizes = prizesVal
    , hand = handVal
    , active = activeVal
    , bench = benchVal
    , attachments = attachmentsVal
    , stadium = stadiumVal
    , evolutionBuried = evolutionBuriedVal
    , total = totalVal
    }


breakdownForBlue : String -> String -> GameState -> PlayerBreakdown
breakdownForBlue red blue gs =
    let
        deckVal =
            gs.piles.deckBlue

        discardVal =
            gs.piles.discardBlue

        prizesVal =
            gs.piles.prizesBlue

        handVal =
            List.length gs.hand.blue

        activeVal =
            case gs.active.blue of
                Just _ ->
                    1

                Nothing ->
                    0

        benchVal =
            List.length gs.bench.blue

        attachmentsVal =
            attachmentCountForSide blue gs.bench.blue gs.active.blue gs.attachments

        stadiumVal =
            case gs.stadium of
                Just s ->
                    if s.player /= red then
                        1

                    else
                        0

                Nothing ->
                    0

        evolutionBuriedVal =
            Dict.values gs.evolution.blue |> List.sum

        totalVal =
            deckVal + discardVal + prizesVal + handVal + activeVal + benchVal + attachmentsVal + stadiumVal + evolutionBuriedVal
    in
    { deck = deckVal
    , discard = discardVal
    , prizes = prizesVal
    , hand = handVal
    , active = activeVal
    , bench = benchVal
    , attachments = attachmentsVal
    , stadium = stadiumVal
    , evolutionBuried = evolutionBuriedVal
    , total = totalVal
    }



-- GROUP ITERATION


type alias IndexedGroup =
    { sectionIndex : Int
    , groupIndex : Int
    , isSetup : Bool
    , group : Action.ActionGroup
    }


type alias FailInfo =
    { sectionIndex : Int
    , groupIndex : Int
    , groupRaw : String
    , redBreakdown : PlayerBreakdown
    , blueBreakdown : PlayerBreakdown
    , redDuplicates : List ( Action.CardRef, Int )
    , blueDuplicates : List ( Action.CardRef, Int )
    }


{-| Cards whose ID appears more than once across bench + active for a player.
Returns (representative CardRef, count) pairs, sorted by count descending.
-}
duplicatesOnBoard : List Action.CardRef -> Maybe Action.CardRef -> List ( Action.CardRef, Int )
duplicatesOnBoard bench maybeActive =
    let
        all =
            bench ++ (Maybe.map List.singleton maybeActive |> Maybe.withDefault [])

        folder card acc =
            case Dict.get card.id acc of
                Just ( existing, n ) ->
                    Dict.insert card.id ( existing, n + 1 ) acc

                Nothing ->
                    Dict.insert card.id ( card, 1 ) acc
    in
    List.foldl folder Dict.empty all
        |> Dict.values
        |> List.filter (\( _, n ) -> n > 1)
        |> List.sortBy (\( _, n ) -> -n)


allGroupsIndexed : Replay.Players -> Replay.Replay -> List IndexedGroup
allGroupsIndexed players replay =
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
                in
                List.indexedMap
                    (\gi grp ->
                        { sectionIndex = si
                        , groupIndex = gi
                        , isSetup = isSetup
                        , group = correctGroupPlayers players grp
                        }
                    )
                    groups
            )
        |> List.concat


{-| Left fold that stops at the first Err and returns it unchanged.
-}
foldUntilError : (a -> b -> Result e b) -> b -> List a -> Result e b
foldUntilError f acc list =
    case list of
        [] ->
            Ok acc

        x :: rest ->
            case f x acc of
                Err e ->
                    Err e

                Ok newAcc ->
                    foldUntilError f newAcc rest


checkGroups : String -> String -> List IndexedGroup -> Result FailInfo GameState
checkGroups red blue groups =
    foldUntilError
        (\indexed gs ->
            let
                newGs =
                    stepGroup red indexed.isSetup indexed.group gs

                redBD =
                    breakdownForRed red newGs

                blueBD =
                    breakdownForBlue red blue newGs
            in
            if redBD.total /= 60 || blueBD.total /= 60 then
                Err
                    { sectionIndex = indexed.sectionIndex
                    , groupIndex = indexed.groupIndex
                    , groupRaw = indexed.group.raw
                    , redBreakdown = redBD
                    , blueBreakdown = blueBD
                    , redDuplicates = duplicatesOnBoard gs.bench.red gs.active.red
                    , blueDuplicates = duplicatesOnBoard gs.bench.blue gs.active.blue
                    }

            else
                Ok newGs
        )
        initialState
        groups



-- FORMATTING


formatBreakdown : String -> PlayerBreakdown -> String
formatBreakdown label bd =
    let
        wrong =
            if bd.total /= 60 then
                "  ← WRONG (off by " ++ String.fromInt (bd.total - 60) ++ ")"

            else
                ""

        stadiumStr =
            if bd.stadium > 0 then
                "  stadium=" ++ String.fromInt bd.stadium

            else
                ""

        evoStr =
            if bd.evolutionBuried > 0 then
                "  evo-buried=" ++ String.fromInt bd.evolutionBuried

            else
                ""
    in
    "  "
        ++ label
        ++ ":"
        ++ "  deck="
        ++ String.fromInt bd.deck
        ++ "  prizes="
        ++ String.fromInt bd.prizes
        ++ "  hand="
        ++ String.fromInt bd.hand
        ++ "  active="
        ++ String.fromInt bd.active
        ++ "  bench="
        ++ String.fromInt bd.bench
        ++ "  attach="
        ++ String.fromInt bd.attachments
        ++ "  discard="
        ++ String.fromInt bd.discard
        ++ stadiumStr
        ++ evoStr
        ++ "  = "
        ++ String.fromInt bd.total
        ++ wrong


formatDuplicates :
    String
    -> String
    -> List ( Action.CardRef, Int )
    -> List ( Action.CardRef, Int )
    -> String
formatDuplicates red blue redDups blueDups =
    if List.isEmpty redDups && List.isEmpty blueDups then
        ""

    else
        let
            formatOne ( card, n ) =
                card.name ++ " ×" ++ String.fromInt n

            formatSide label dups =
                if List.isEmpty dups then
                    []

                else
                    [ "  ⚠  " ++ label ++ ": " ++ String.join ", " (List.map formatOne dups) ]
        in
        String.join "\n"
            ([ "  ⚠  Duplicate card IDs in play — attachment placement may be inaccurate:" ]
                ++ formatSide ("red (" ++ red ++ ")") redDups
                ++ formatSide ("blue (" ++ blue ++ ")") blueDups
            )


checkFile : FileInput -> { output : String, ok : Bool }
checkFile flags =
    let
        replay =
            Replay.parse flags.content
    in
    case replay.players of
        Nothing ->
            { output = "ERROR  " ++ flags.name ++ "\n  could not identify players\n"
            , ok = False
            }

        Just players ->
            let
                groups =
                    allGroupsIndexed players replay

                result =
                    checkGroups players.red players.blue groups
            in
            case result of
                Ok _ ->
                    { output =
                        "✓  "
                            ++ flags.name
                            ++ "  ("
                            ++ String.fromInt (List.length groups)
                            ++ " groups)\n"
                    , ok = True
                    }

                Err fail ->
                    let
                        visualUrl =
                            if String.isEmpty flags.replayUrl then
                                "  (provide --url to get a visual link)"

                            else
                                "  Visual: http://localhost:8000/?replay_url="
                                    ++ Url.percentEncode flags.replayUrl
                                    ++ "&section="
                                    ++ String.fromInt fail.sectionIndex
                                    ++ "&group="
                                    ++ String.fromInt fail.groupIndex
                    in
                    { output =
                        String.join "\n"
                            [ "✗  " ++ flags.name
                            , "  Failed at section "
                                ++ String.fromInt fail.sectionIndex
                                ++ ", group "
                                ++ String.fromInt fail.groupIndex
                            , "  Action: " ++ fail.groupRaw
                            , formatBreakdown ("red (" ++ players.red ++ ")") fail.redBreakdown
                            , formatBreakdown ("blue (" ++ players.blue ++ ")") fail.blueBreakdown
                            , formatDuplicates players.red players.blue fail.redDuplicates fail.blueDuplicates
                            , visualUrl
                            , ""
                            ]
                    , ok = False
                    }
