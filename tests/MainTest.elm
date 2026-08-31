module MainTest exposing (suite)

import Dict
import Expect
import Http
import Action exposing (CardRef)
import Main exposing (CardData, CardPopup(..), CurrentPlay, HandState, Model(..), Msg(..), PileState, BenchState, ActiveState, InstanceState, AttachmentState, ConditionState, applyGroupToHand, applyGroupToPiles, applyGroupToBench, applyGroupToInstances, applyGroupToAttachments, applyGroupToConditions, correctGroupPlayers, emptyInstances, emptyAttachments, emptyConditions, lookupAttachments, lookupConditions, firstInstance, currentPlayFromGroup, emptyHand, emptyPiles, emptyBench, emptyActive, init, update)
import Replay exposing (ReplayLine(..), Section(..))
import Test exposing (Test, describe, test)


cardDataWithImage : String -> CardData
cardDataWithImage url =
    { imageUrl = Just url, attacks = [], abilities = [], category = Nothing, name = Nothing }


suite : Test
suite =
    describe "Replay.parse"
        [ describe "sections"
            [ test "parses a setup section" <|
                \_ ->
                    Replay.parse "Setup\nPlayer did something.\n"
                        |> .sections
                        |> List.map sectionKind
                        |> Expect.equalLists [ "setup" ]
            , test "parses all three section types" <|
                \_ ->
                    multiSectionInput
                        |> Replay.parse
                        |> .sections
                        |> List.map sectionKind
                        |> Expect.equalLists [ "setup", "turn", "checkup" ]
            , test "win line followed by post-game actions produces ResultSection" <|
                \_ ->
                    Replay.parse "Turn # 1 - A's Turn\nA drew a card.\nB wins.\nBoomerang Energy triggered.\n- Something happened.\n"
                        |> .sections
                        |> List.map sectionKind
                        |> Expect.equalLists [ "turn", "result" ]
            , test "win line followed by post-game actions: turn section retains post-game lines but not win line" <|
                \_ ->
                    Replay.parse "Turn # 1 - A's Turn\nA drew a card.\nB wins.\nBoomerang Energy triggered.\n- Something happened.\n"
                        |> .sections
                        |> List.filterMap
                            (\s ->
                                case s of
                                    TurnSection _ lines ->
                                        Just lines

                                    _ ->
                                        Nothing
                            )
                        |> List.head
                        |> Expect.equal (Just [ TopLine "A drew a card.", TopLine "Boomerang Energy triggered.", DetailLine "Something happened." ])
            , test "win line followed by post-game actions: result winner is correct" <|
                \_ ->
                    Replay.parse "Turn # 1 - A's Turn\nA drew a card.\nB wins.\nBoomerang Energy triggered.\n"
                        |> .sections
                        |> List.filterMap
                            (\s ->
                                case s of
                                    ResultSection r ->
                                        Just r.winner

                                    _ ->
                                        Nothing
                            )
                        |> List.head
                        |> Expect.equal (Just "B")
            ]
        , describe "turn header"
            [ test "numbered format: extracts turn number" <|
                \_ ->
                    Replay.parse "Turn # 5 - zosiu's Turn\n"
                        |> firstTurn
                        |> Maybe.map .number
                        |> Expect.equal (Just 5)
            , test "numbered format: extracts player name" <|
                \_ ->
                    Replay.parse "Turn # 1 - NoxFoxEX's Turn\n"
                        |> firstTurn
                        |> Maybe.map .player
                        |> Expect.equal (Just "NoxFoxEX")
            , test "unnumbered format: extracts player name" <|
                \_ ->
                    Replay.parse "takeshi516's Turn\n"
                        |> firstTurn
                        |> Maybe.map .player
                        |> Expect.equal (Just "takeshi516")
            , test "unnumbered format: assigns sequential numbers" <|
                \_ ->
                    Replay.parse "A's Turn\nA drew.\nB's Turn\nB drew.\n"
                        |> .sections
                        |> List.filterMap
                            (\s ->
                                case s of
                                    TurnSection t _ ->
                                        Just t.number

                                    _ ->
                                        Nothing
                            )
                        |> Expect.equalLists [ 1, 2 ]
            ]
        , describe "line classification"
            [ test "top-level lines become TopLine" <|
                \_ ->
                    Replay.parse "Turn # 1 - A's Turn\nA played a card.\n"
                        |> firstTurnLines
                        |> Expect.equalLists [ TopLine "A played a card." ]
            , test "dash-prefixed lines become DetailLine" <|
                \_ ->
                    Replay.parse "Turn # 1 - A's Turn\nA played a card.\n- A drew 2 cards.\n"
                        |> firstTurnLines
                        |> Expect.equalLists
                            [ TopLine "A played a card."
                            , DetailLine "A drew 2 cards."
                            ]
            , test "bullet lines become BulletLine with prefix stripped" <|
                \_ ->
                    Replay.parse "Turn # 1 - A's Turn\nA played X.\n- A drew 2 cards.\n   • CardA, CardB\n"
                        |> firstTurnLines
                        |> Expect.equalLists
                            [ TopLine "A played X."
                            , DetailLine "A drew 2 cards."
                            , BulletLine "CardA, CardB"
                            ]
            , test "empty lines are skipped" <|
                \_ ->
                    Replay.parse "Turn # 1 - A's Turn\n\nA drew a card.\n\n"
                        |> firstTurnLines
                        |> Expect.equalLists [ TopLine "A drew a card." ]
            ]
        , describe "init"
            [ test "empty flags start in EnteringUrl" <|
                \_ ->
                    init { replayUrl = "", sectionIndex = 0, groupIndex = 0, flipOpponent = True, debug = False }
                        |> Tuple.first
                        |> Expect.equal (EnteringUrl "")
            , test "whitespace flags start in EnteringUrl" <|
                \_ ->
                    init { replayUrl = "   ", sectionIndex = 0, groupIndex = 0, flipOpponent = True, debug = False }
                        |> Tuple.first
                        |> Expect.equal (EnteringUrl "")
            , test "url flags start in Loading" <|
                \_ ->
                    init { replayUrl = "https://example.com/replay.txt", sectionIndex = 0, groupIndex = 0, flipOpponent = True, debug = False }
                        |> Tuple.first
                        |> Expect.equal (Loading "https://example.com/replay.txt" 0 0 { flipOpponent = True, debug = False })
            , test "url flags are trimmed" <|
                \_ ->
                    init { replayUrl = "  https://example.com/replay.txt  ", sectionIndex = 0, groupIndex = 0, flipOpponent = True, debug = False }
                        |> Tuple.first
                        |> Expect.equal (Loading "https://example.com/replay.txt" 0 0 { flipOpponent = True, debug = False })
            , test "section index is preserved in Loading state" <|
                \_ ->
                    init { replayUrl = "https://example.com/replay.txt", sectionIndex = 3, groupIndex = 0, flipOpponent = True, debug = False }
                        |> Tuple.first
                        |> Expect.equal (Loading "https://example.com/replay.txt" 3 0 { flipOpponent = True, debug = False })
            ]
        , describe "GotReplay"
            [ test "Ok result transitions to Loaded with parsed replay" <|
                \_ ->
                    let
                        url =
                            "https://example.com/replay.txt"

                        content =
                            "Turn # 1 - A's Turn\nA drew a card.\n"
                    in
                    update (GotReplay (Ok content)) (Loading url 0 0 { flipOpponent = True, debug = False })
                        |> Tuple.first
                        |> Expect.equal (Loaded url (Replay.parse content) 0 0 Nothing Dict.empty { flipOpponent = True, debug = False })
            , test "NetworkError triggers proxy retry" <|
                \_ ->
                    update (GotReplay (Err Http.NetworkError)) (Loading "https://example.com" 0 0 { flipOpponent = True, debug = False })
                        |> Tuple.first
                        |> Expect.equal (Retrying "https://example.com" 0 0 { flipOpponent = True, debug = False })
            , test "404 transitions to Failed with friendly message" <|
                \_ ->
                    update (GotReplay (Err (Http.BadStatus 404))) (Loading "https://example.com" 0 0 { flipOpponent = True, debug = False })
                        |> Tuple.first
                        |> Expect.equal (Failed "https://example.com" "No replay content found — check the URL")
            , test "other errors transition to Failed without retrying" <|
                \_ ->
                    update (GotReplay (Err Http.Timeout)) (Loading "https://example.com" 0 0 { flipOpponent = True, debug = False })
                        |> Tuple.first
                        |> Expect.equal (Failed "https://example.com" "Request timed out")
            , test "proxy Ok result transitions to Loaded" <|
                \_ ->
                    let
                        url =
                            "https://example.com/replay.txt"

                        content =
                            "Turn # 1 - A's Turn\nA drew a card.\n"
                    in
                    update (GotReplay (Ok content)) (Retrying url 0 0 { flipOpponent = True, debug = False })
                        |> Tuple.first
                        |> Expect.equal (Loaded url (Replay.parse content) 0 0 Nothing Dict.empty { flipOpponent = True, debug = False })
            , test "proxy 404 transitions to Failed with friendly message" <|
                \_ ->
                    update (GotReplay (Err (Http.BadStatus 404))) (Retrying "https://example.com" 0 0 { flipOpponent = True, debug = False })
                        |> Tuple.first
                        |> Expect.equal (Failed "https://example.com" "No replay content found — check the URL")
            , test "proxy error transitions to Failed" <|
                \_ ->
                    update (GotReplay (Err Http.Timeout)) (Retrying "https://example.com" 0 0 { flipOpponent = True, debug = False })
                        |> Tuple.first
                        |> Expect.equal (Failed "https://example.com" "Request timed out")
            , test "empty content transitions to Failed" <|
                \_ ->
                    update (GotReplay (Ok "")) (Loading "https://example.com" 0 0 { flipOpponent = True, debug = False })
                        |> Tuple.first
                        |> Expect.equal (Failed "https://example.com" "No replay content found — check the URL")
            , test "unrecognised content from proxy transitions to Failed" <|
                \_ ->
                    update (GotReplay (Ok "<html>404 Not Found</html>")) (Retrying "https://example.com" 0 0 { flipOpponent = True, debug = False })
                        |> Tuple.first
                        |> Expect.equal (Failed "https://example.com" "No replay content found — check the URL")
            , test "curly apostrophes (U+2019) are normalized to straight before parsing" <|
                \_ ->
                    let
                        url =
                            "https://example.com/replay.txt"

                        -- Some replay sources use U+2019 RIGHT SINGLE QUOTATION MARK
                        -- in player possessives on action lines (e.g. the defender in an
                        -- attack line). Without normalization parsePokemonRef fails to split
                        -- on "'s (" and the move field becomes the whole remainder string.
                        curlyContent =
                            "A's Turn\nA's (sv01_001) Bulbasaur used Tackle on B\u{2019}s (sv01_002) Ivysaur for 10 damage.\n"

                        normalizedContent =
                            "A's Turn\nA's (sv01_001) Bulbasaur used Tackle on B's (sv01_002) Ivysaur for 10 damage.\n"
                    in
                    update (GotReplay (Ok curlyContent)) (Loading url 0 0 { flipOpponent = True, debug = False })
                        |> Tuple.first
                        |> Expect.equal (Loaded url (Replay.parse normalizedContent) 0 0 Nothing Dict.empty { flipOpponent = True, debug = False })
            , test "deep-linked section index is restored on load" <|
                \_ ->
                    let
                        url =
                            "https://example.com/replay.txt"

                        content =
                            "Setup\nSome setup.\nTurn # 1 - A's Turn\nA drew a card.\n"
                    in
                    update (GotReplay (Ok content)) (Loading url 1 0 { flipOpponent = True, debug = False })
                        |> Tuple.first
                        |> Expect.equal (Loaded url (Replay.parse content) 1 0 Nothing Dict.empty { flipOpponent = True, debug = False })
            , test "out-of-range section index is clamped to last section" <|
                \_ ->
                    let
                        url =
                            "https://example.com/replay.txt"

                        content =
                            "Setup\nSome setup.\n"
                    in
                    update (GotReplay (Ok content)) (Loading url 99 0 { flipOpponent = True, debug = False })
                        |> Tuple.first
                        |> Expect.equal (Loaded url (Replay.parse content) 0 0 Nothing Dict.empty { flipOpponent = True, debug = False })
            ]
        , describe "section navigation"
            [ test "FirstSection jumps to index 0" <|
                \_ ->
                    let
                        content =
                            "Setup\nSome setup.\nTurn # 1 - A's Turn\nA drew a card.\n"

                        replay =
                            Replay.parse content
                    in
                    update FirstSection (Loaded "url" replay 1 0 Nothing Dict.empty { flipOpponent = True, debug = False })
                        |> Tuple.first
                        |> Expect.equal (Loaded "url" replay 0 0 Nothing Dict.empty { flipOpponent = True, debug = False })
            , test "LastSection jumps to the last index" <|
                \_ ->
                    let
                        content =
                            "Setup\nSome setup.\nTurn # 1 - A's Turn\nA drew a card.\n"

                        replay =
                            Replay.parse content
                    in
                    update LastSection (Loaded "url" replay 0 0 Nothing Dict.empty { flipOpponent = True, debug = False })
                        |> Tuple.first
                        |> Expect.equal (Loaded "url" replay 1 0 Nothing Dict.empty { flipOpponent = True, debug = False })
            , test "NextSection increments the index" <|
                \_ ->
                    let
                        content =
                            "Setup\nSome setup.\nTurn # 1 - A's Turn\nA drew a card.\n"

                        replay =
                            Replay.parse content
                    in
                    update NextSection (Loaded "url" replay 0 0 Nothing Dict.empty { flipOpponent = True, debug = False })
                        |> Tuple.first
                        |> Expect.equal (Loaded "url" replay 1 0 Nothing Dict.empty { flipOpponent = True, debug = False })
            , test "NextSection does not go past the last section" <|
                \_ ->
                    let
                        content =
                            "Setup\nSome setup.\n"

                        replay =
                            Replay.parse content
                    in
                    update NextSection (Loaded "url" replay 0 0 Nothing Dict.empty { flipOpponent = True, debug = False })
                        |> Tuple.first
                        |> Expect.equal (Loaded "url" replay 0 0 Nothing Dict.empty { flipOpponent = True, debug = False })
            , test "PrevSection decrements the index" <|
                \_ ->
                    let
                        content =
                            "Setup\nSome setup.\nTurn # 1 - A's Turn\nA drew a card.\n"

                        replay =
                            Replay.parse content
                    in
                    update PrevSection (Loaded "url" replay 1 0 Nothing Dict.empty { flipOpponent = True, debug = False })
                        |> Tuple.first
                        |> Expect.equal (Loaded "url" replay 0 0 Nothing Dict.empty { flipOpponent = True, debug = False })
            , test "PrevSection does not go below zero" <|
                \_ ->
                    let
                        content =
                            "Setup\nSome setup.\n"

                        replay =
                            Replay.parse content
                    in
                    update PrevSection (Loaded "url" replay 0 0 Nothing Dict.empty { flipOpponent = True, debug = False })
                        |> Tuple.first
                        |> Expect.equal (Loaded "url" replay 0 0 Nothing Dict.empty { flipOpponent = True, debug = False })
            , test "NextSection reveals next group within section when more groups exist" <|
                \_ ->
                    let
                        -- One section with 2 action groups
                        content =
                            "Setup\nA did something.\nB did something.\n"

                        replay =
                            Replay.parse content
                    in
                    update NextSection (Loaded "url" replay 0 0 Nothing Dict.empty { flipOpponent = True, debug = False })
                        |> Tuple.first
                        |> Expect.equal (Loaded "url" replay 0 1 Nothing Dict.empty { flipOpponent = True, debug = False })
            , test "PrevSection hides last group within section when groupIndex > 0" <|
                \_ ->
                    let
                        content =
                            "Setup\nA did something.\nB did something.\n"

                        replay =
                            Replay.parse content
                    in
                    update PrevSection (Loaded "url" replay 0 1 Nothing Dict.empty { flipOpponent = True, debug = False })
                        |> Tuple.first
                        |> Expect.equal (Loaded "url" replay 0 0 Nothing Dict.empty { flipOpponent = True, debug = False })
            ]
        , describe "card popup"
            [ test "CardClicked sets FetchingCard popup for a valid id" <|
                \_ ->
                    let
                        replay =
                            Replay.parse "Setup\nSome setup.\n"
                    in
                    update (CardClicked "sv4_160_ph" "sv4_160_ph") (Loaded "url" replay 0 0 Nothing Dict.empty { flipOpponent = True, debug = False })
                        |> Tuple.first
                        |> Expect.equal (Loaded "url" replay 0 0 (Just (FetchingCard "sv4_160_ph" "sv4_160_ph")) Dict.empty { flipOpponent = True, debug = False })
            , test "CardClicked with unparseable id shows CardNotFound immediately" <|
                \_ ->
                    let
                        replay =
                            Replay.parse "Setup\nSome setup.\n"
                    in
                    update (CardClicked "nounderscore" "nounderscore") (Loaded "url" replay 0 0 Nothing Dict.empty { flipOpponent = True, debug = False })
                        |> Tuple.first
                        |> Expect.equal (Loaded "url" replay 0 0 (Just (CardNotFound "nounderscore")) Dict.empty { flipOpponent = True, debug = False })
            , test "GotCardImage with valid JSON shows card image" <|
                \_ ->
                    let
                        replay =
                            Replay.parse "Setup\nSome setup.\n"

                        json =
                            "{\"id\":\"swsh1-1\",\"image\":\"https://assets.tcgdex.net/en/swsh/swsh1/1\"}"
                    in
                    -- FetchingCard is set by CardClicked before the HTTP response arrives
                    update (GotCardImage "swsh1-1" (Ok json)) (Loaded "url" replay 0 0 (Just (FetchingCard "swsh1-1" "swsh1-1")) Dict.empty { flipOpponent = True, debug = False })
                        |> Tuple.first
                        |> Expect.equal (Loaded "url" replay 0 0 (Just (ShowingCard "swsh1-1" (cardDataWithImage "https://assets.tcgdex.net/en/swsh/swsh1/1"))) (Dict.fromList [ ( "swsh1-1", cardDataWithImage "https://assets.tcgdex.net/en/swsh/swsh1/1" ) ]) { flipOpponent = True, debug = False })
            , test "GotCardImage with invalid JSON shows CardNotFound" <|
                \_ ->
                    let
                        replay =
                            Replay.parse "Setup\nSome setup.\n"

                        emptyCardData =
                            { imageUrl = Nothing, attacks = [], abilities = [], category = Nothing, name = Nothing }
                    in
                    update (GotCardImage "swsh1-1" (Ok "{\"error\":\"not found\"}")) (Loaded "url" replay 0 0 (Just (FetchingCard "swsh1-1" "swsh1-1")) Dict.empty { flipOpponent = True, debug = False })
                        |> Tuple.first
                        |> Expect.equal (Loaded "url" replay 0 0 (Just (CardNotFound "swsh1-1")) (Dict.fromList [ ( "swsh1-1", emptyCardData ) ]) { flipOpponent = True, debug = False })
            , test "GotCardImage with HTTP error shows CardNotFound" <|
                \_ ->
                    let
                        replay =
                            Replay.parse "Setup\nSome setup.\n"
                    in
                    update (GotCardImage "swsh1-1" (Err Http.NetworkError)) (Loaded "url" replay 0 0 (Just (FetchingCard "swsh1-1" "swsh1-1")) Dict.empty { flipOpponent = True, debug = False })
                        |> Tuple.first
                        |> Expect.equal (Loaded "url" replay 0 0 (Just (CardNotFound "swsh1-1")) Dict.empty { flipOpponent = True, debug = False })
            , test "GotCardImage as background hand fetch does not open popup" <|
                \_ ->
                    let
                        replay =
                            Replay.parse "Setup\nSome setup.\n"

                        json =
                            "{\"id\":\"swsh1-1\",\"image\":\"https://assets.tcgdex.net/en/swsh/swsh1/1\"}"
                    in
                    -- No FetchingCard popup = background prefetch; should just update cache silently
                    update (GotCardImage "swsh1-1" (Ok json)) (Loaded "url" replay 0 0 Nothing Dict.empty { flipOpponent = True, debug = False })
                        |> Tuple.first
                        |> Expect.equal (Loaded "url" replay 0 0 Nothing (Dict.fromList [ ( "swsh1-1", cardDataWithImage "https://assets.tcgdex.net/en/swsh/swsh1/1" ) ]) { flipOpponent = True, debug = False })
            , test "CloseCard removes the popup" <|
                \_ ->
                    let
                        replay =
                            Replay.parse "Setup\nSome setup.\n"
                    in
                    update CloseCard (Loaded "url" replay 0 0 (Just (CardNotFound "swsh1-1")) Dict.empty { flipOpponent = True, debug = False })
                        |> Tuple.first
                        |> Expect.equal (Loaded "url" replay 0 0 Nothing Dict.empty { flipOpponent = True, debug = False })
            , test "MoveClicked on cache hit shows ShowingMove" <|
                \_ ->
                    let
                        replay = Replay.parse "Setup\nSome setup.\n"
                        ability = { abilityType = "Ability", name = "Recon Directive", effect = "Once during your turn..." }
                        cardData = { imageUrl = Just "https://assets.tcgdex.net/en/sv/sv08.5/072", attacks = [], abilities = [ ability ], category = Nothing, name = Nothing }
                        cache = Dict.fromList [ ( "sv8-5_72_sph", cardData ) ]
                    in
                    update (MoveClicked "sv8-5_72_sph" "Recon Directive") (Loaded "url" replay 0 0 Nothing cache { flipOpponent = True, debug = False })
                        |> Tuple.first
                        |> Expect.equal (Loaded "url" replay 0 0 (Just (ShowingMove cardData "Recon Directive")) cache { flipOpponent = True, debug = False })
            , test "MoveClicked on cache miss shows FetchingMove" <|
                \_ ->
                    let
                        replay = Replay.parse "Setup\nSome setup.\n"
                    in
                    update (MoveClicked "sv04_160" "Tackle") (Loaded "url" replay 0 0 Nothing Dict.empty { flipOpponent = True, debug = False })
                        |> Tuple.first
                        |> Expect.equal (Loaded "url" replay 0 0 (Just (FetchingMove "sv04_160" "Tackle")) Dict.empty { flipOpponent = True, debug = False })
            , test "GotCardImage when FetchingMove resolves to ShowingMove" <|
                \_ ->
                    let
                        replay = Replay.parse "Setup\nSome setup.\n"
                        json = "{\"image\":\"https://assets.tcgdex.net/en/sv/sv04/160\",\"attacks\":[{\"name\":\"Tackle\",\"cost\":[\"Colorless\"],\"damage\":10}],\"abilities\":[]}"
                        expectedData = { imageUrl = Just "https://assets.tcgdex.net/en/sv/sv04/160", attacks = [ { name = "Tackle", cost = [ "Colorless" ], damage = "10", effect = "" } ], abilities = [], category = Nothing, name = Nothing }
                    in
                    update (GotCardImage "sv04_160" (Ok json)) (Loaded "url" replay 0 0 (Just (FetchingMove "sv04_160" "Tackle")) Dict.empty { flipOpponent = True, debug = False })
                        |> Tuple.first
                        |> Expect.equal (Loaded "url" replay 0 0 (Just (ShowingMove expectedData "Tackle")) (Dict.fromList [ ( "sv04_160", expectedData ) ]) { flipOpponent = True, debug = False })
            , test "GotCardImage stores attacks and abilities in cache" <|
                \_ ->
                    let
                        replay = Replay.parse "Setup\nSome setup.\n"
                        json = "{\"image\":\"https://example.com/img\",\"attacks\":[{\"name\":\"Scratch\",\"cost\":[\"Colorless\"],\"damage\":10}],\"abilities\":[{\"type\":\"Ability\",\"name\":\"Swift Run\",\"effect\":\"Once per turn.\"}]}"
                        expectedData =
                            { imageUrl = Just "https://example.com/img"
                            , attacks = [ { name = "Scratch", cost = [ "Colorless" ], damage = "10", effect = "" } ]
                            , abilities = [ { abilityType = "Ability", name = "Swift Run", effect = "Once per turn." } ]
                            , category = Nothing
                            , name = Nothing
                            }
                    in
                    update (GotCardImage "sv04_001" (Ok json)) (Loaded "url" replay 0 0 (Just (FetchingCard "sv04_001" "sv04_001")) Dict.empty { flipOpponent = True, debug = False })
                        |> Tuple.first
                        |> Expect.equal (Loaded "url" replay 0 0 (Just (ShowingCard "sv04_001" expectedData)) (Dict.fromList [ ( "sv04_001", expectedData ) ]) { flipOpponent = True, debug = False })
            ]
        , describe "card image cache"
            [ test "CardClicked uses cache hit and shows card immediately" <|
                \_ ->
                    let
                        replay =
                            Replay.parse "Setup\nSome setup.\n"

                        imageUrl =
                            "https://assets.tcgdex.net/en/sv/sv04/160"

                        cache =
                            Dict.fromList [ ( "sv04_160", cardDataWithImage imageUrl ) ]
                    in
                    update (CardClicked "sv04_160" "sv04_160") (Loaded "url" replay 0 0 Nothing cache { flipOpponent = True, debug = False })
                        |> Tuple.first
                        |> Expect.equal (Loaded "url" replay 0 0 (Just (ShowingCard "sv04_160" (cardDataWithImage imageUrl))) cache { flipOpponent = True, debug = False })
            , test "CardClicked on cache miss shows FetchingCard" <|
                \_ ->
                    let
                        replay =
                            Replay.parse "Setup\nSome setup.\n"
                    in
                    update (CardClicked "sv04_160" "sv04_160") (Loaded "url" replay 0 0 Nothing Dict.empty { flipOpponent = True, debug = False })
                        |> Tuple.first
                        |> Expect.equal (Loaded "url" replay 0 0 (Just (FetchingCard "sv04_160" "sv04_160")) Dict.empty { flipOpponent = True, debug = False })
            , test "cache is preserved after navigating to next section" <|
                \_ ->
                    let
                        replay =
                            Replay.parse "Setup\nSome setup.\n\nTurn # 1 - A's Turn\nA drew.\n"

                        imageUrl =
                            "https://assets.tcgdex.net/en/sv/sv04/160"

                        cache =
                            Dict.fromList [ ( "sv04_160", cardDataWithImage imageUrl ) ]
                    in
                    update NextSection (Loaded "url" replay 0 0 Nothing cache { flipOpponent = True, debug = False })
                        |> Tuple.first
                        |> Expect.equal (Loaded "url" replay 1 0 Nothing cache { flipOpponent = True, debug = False })
            , test "GotCardImage success adds to cache without removing other entries" <|
                \_ ->
                    let
                        replay =
                            Replay.parse "Setup\nSome setup.\n"

                        existingUrl =
                            "https://assets.tcgdex.net/en/sv/sv04/160"

                        newUrl =
                            "https://assets.tcgdex.net/en/swsh/swsh1/1"

                        priorCache =
                            Dict.fromList [ ( "sv04_160", cardDataWithImage existingUrl ) ]

                        expectedCache =
                            Dict.fromList
                                [ ( "sv04_160", cardDataWithImage existingUrl )
                                , ( "swsh1-1", cardDataWithImage newUrl )
                                ]

                        json =
                            "{\"id\":\"swsh1-1\",\"image\":\"" ++ newUrl ++ "\"}"
                    in
                    update (GotCardImage "swsh1-1" (Ok json)) (Loaded "url" replay 0 0 (Just (FetchingCard "swsh1-1" "swsh1-1")) priorCache { flipOpponent = True, debug = False })
                        |> Tuple.first
                        |> Expect.equal (Loaded "url" replay 0 0 (Just (ShowingCard "swsh1-1" (cardDataWithImage newUrl))) expectedCache { flipOpponent = True, debug = False })
            , test "GotCardImage network error does not populate the cache" <|
                \_ ->
                    let
                        replay =
                            Replay.parse "Setup\nSome setup.\n"
                    in
                    update (GotCardImage "sv04_160" (Err Http.NetworkError)) (Loaded "url" replay 0 0 (Just (FetchingCard "sv04_160" "sv04_160")) Dict.empty { flipOpponent = True, debug = False })
                        |> Tuple.first
                        |> Expect.equal (Loaded "url" replay 0 0 (Just (CardNotFound "sv04_160")) Dict.empty { flipOpponent = True, debug = False })
            , test "cache hit fires no HTTP command" <|
                \_ ->
                    let
                        replay =
                            Replay.parse "Setup\nSome setup.\n"

                        imageUrl =
                            "https://assets.tcgdex.net/en/sv/sv04/160"

                        cache =
                            Dict.fromList [ ( "sv04_160", cardDataWithImage imageUrl ) ]
                    in
                    update (CardClicked "sv04_160" "sv04_160") (Loaded "url" replay 0 0 Nothing cache { flipOpponent = True, debug = False })
                        |> Tuple.second
                        |> Expect.equal Cmd.none
            ]
        , describe "player identification"
            [ test "identifies the player with revealed hand as red" <|
                \_ ->
                    setupWith { redFirst = True }
                        |> Replay.parse
                        |> .players
                        |> Expect.equal (Just { red = "A", blue = "B" })
            , test "works when red player draws second" <|
                \_ ->
                    setupWith { redFirst = False }
                        |> Replay.parse
                        |> .players
                        |> Expect.equal (Just { red = "A", blue = "B" })
            , test "is not fooled by mulligan bullets after both draws" <|
                \_ ->
                    String.join "\n"
                        [ "Setup"
                        , "A drew 7 cards for the opening hand."
                        , "- 7 drawn cards."
                        , "   • Card1, Card2"
                        , "B drew 7 cards for the opening hand."
                        , "- 7 drawn cards."
                        , "A took a mulligan."
                        , "- Cards revealed from Mulligan 1"
                        , "   • Card3, Card4"
                        ]
                        |> Replay.parse
                        |> .players
                        |> Expect.equal (Just { red = "A", blue = "B" })
            ]
        , describe "hand state"
            [ test "OpeningDraw adds unknown cards" <|
                \_ ->
                    let
                        group =
                            { raw = "A drew 7 cards for the opening hand."
                            , action = Action.OpeningDraw { player = "A", count = 7 }
                            , details = []
                            }
                    in
                    applyGroupToHand "A" emptyHand group
                        |> .red
                        |> List.length
                        |> Expect.equal 7

            , test "OpeningDraw with CardList bullet uses known cards" <|
                \_ ->
                    let
                        cards =
                            [ { id = "sv1_1", name = "Bulbasaur" }
                            , { id = "sv1_2", name = "Ivysaur" }
                            ]

                        group =
                            { raw = "A drew 2 cards for the opening hand."
                            , action = Action.OpeningDraw { player = "A", count = 2 }
                            , details =
                                [ { raw = "2 drawn cards."
                                  , action = Action.UnknownAction "2 drawn cards."
                                  , bullets =
                                        [ { raw = "(sv1_1) Bulbasaur, (sv1_2) Ivysaur"
                                          , action = Action.CardList cards
                                          }
                                        ]
                                  }
                                ]
                            }
                    in
                    applyGroupToHand "A" emptyHand group
                        |> .red
                        |> Expect.equal [ Just { id = "sv1_1", name = "Bulbasaur" }, Just { id = "sv1_2", name = "Ivysaur" } ]

            , test "PlayedTrainer removes card from hand" <|
                \_ ->
                    let
                        card =
                            { id = "sv1_100", name = "Nest Ball" }

                        startHand =
                            { red = [ Just card, Nothing ], blue = [] }

                        group =
                            { raw = "A played (sv1_100) Nest Ball."
                            , action = Action.PlayedTrainer { player = "A", card = card }
                            , details = []
                            }
                    in
                    applyGroupToHand "A" startHand group
                        |> .red
                        |> Expect.equal [ Nothing ]

            , test "Drew adds named card to hand" <|
                \_ ->
                    let
                        card =
                            { id = "sv1_50", name = "Pikachu" }

                        group =
                            { raw = "A drew (sv1_50) Pikachu."
                            , action = Action.Drew { player = "A", card = Just card }
                            , details = []
                            }
                    in
                    applyGroupToHand "A" emptyHand group
                        |> .red
                        |> Expect.equal [ Just card ]

            , test "detail DrewCount with CardList adds known cards" <|
                \_ ->
                    let
                        cards =
                            [ { id = "sv1_1", name = "Bulbasaur" }, { id = "sv1_2", name = "Ivysaur" } ]

                        group =
                            { raw = "A played (sv1_90) Trainer."
                            , action = Action.PlayedTrainer { player = "A", card = { id = "sv1_90", name = "Trainer" } }
                            , details =
                                [ { raw = "A drew 2 cards."
                                  , action = Action.DrewCount { player = "A", count = 2 }
                                  , bullets =
                                        [ { raw = "(sv1_1) Bulbasaur, (sv1_2) Ivysaur"
                                          , action = Action.CardList cards
                                          }
                                        ]
                                  }
                                ]
                            }

                        startHand =
                            { red = [ Just { id = "sv1_90", name = "Trainer" } ], blue = [] }
                    in
                    applyGroupToHand "A" startHand group
                        |> .red
                        |> Expect.equal [ Just { id = "sv1_1", name = "Bulbasaur" }, Just { id = "sv1_2", name = "Ivysaur" } ]

            , test "opponent draws go to blue hand" <|
                \_ ->
                    let
                        group =
                            { raw = "B drew a card."
                            , action = Action.Drew { player = "B", card = Nothing }
                            , details = []
                            }
                    in
                    applyGroupToHand "A" emptyHand group
                        |> .blue
                        |> List.length
                        |> Expect.equal 1

            , test "MulliganTaken replaces hand with 7 unknowns" <|
                \_ ->
                    let
                        startHand =
                            { red = [ Nothing, Nothing, Nothing ], blue = [] }

                        group =
                            { raw = "A took a mulligan."
                            , action = Action.MulliganTaken { player = "A", count = 1 }
                            , details = []
                            }
                    in
                    applyGroupToHand "A" startHand group
                        |> .red
                        |> Expect.equal (List.repeat 7 Nothing)

            , test "TookPrize does not add cards (CardAddedToHand groups do it)" <|
                \_ ->
                    let
                        prize =
                            { raw = "A took 2 Prize cards."
                            , action = Action.TookPrize { player = "A", count = 2 }
                            , details = []
                            }

                        added1 =
                            { raw = "A card was added to A's hand."
                            , action = Action.CardAddedToHand { card = Nothing, player = "A" }
                            , details = []
                            }

                        added2 =
                            { raw = "(sv1_1) Pikachu was added to A's hand."
                            , action = Action.CardAddedToHand { card = Just { id = "sv1_1", name = "Pikachu" }, player = "A" }
                            , details = []
                            }

                        startHand =
                            { red = [ Just { id = "sv1_50", name = "Raichu" } ], blue = [] }

                        finalHand =
                            List.foldl (\g h -> applyGroupToHand "A" h g) startHand [ prize, added1, added2 ]
                    in
                    finalHand.red
                        |> List.length
                        |> Expect.equal 3

            , test "TookPrize with named cards ends up with correct known cards" <|
                \_ ->
                    let
                        prize =
                            { raw = "A took 2 Prize cards."
                            , action = Action.TookPrize { player = "A", count = 2 }
                            , details = []
                            }

                        card1 =
                            { id = "sv6_130", name = "Dragapult ex" }

                        card2 =
                            { id = "sv6_165", name = "Unfair Stamp" }

                        added1 =
                            { raw = "(sv6_130) Dragapult ex was added to A's hand."
                            , action = Action.CardAddedToHand { card = Just card1, player = "A" }
                            , details = []
                            }

                        added2 =
                            { raw = "(sv6_165) Unfair Stamp was added to A's hand."
                            , action = Action.CardAddedToHand { card = Just card2, player = "A" }
                            , details = []
                            }

                        finalHand =
                            List.foldl (\g h -> applyGroupToHand "A" h g) emptyHand [ prize, added1, added2 ]
                    in
                    finalHand.red
                        |> Expect.equal [ Just card1, Just card2 ]

            , test "tryCardList parses multi-card bullet" <|
                \_ ->
                    Action.parseAction "(sv1_1) Bulbasaur, (sv1_2) Ivysaur"
                        |> Expect.equal
                            (Action.CardList
                                [ { id = "sv1_1", name = "Bulbasaur" }
                                , { id = "sv1_2", name = "Ivysaur" }
                                ]
                            )
            ]

        , describe "pile state"
            [ test "emptyPiles starts with 60 in each deck, 0 in each discard, 0 prizes" <|
                \_ ->
                    emptyPiles
                        |> Expect.equal { deckRed = 60, deckBlue = 60, discardRed = 0, discardBlue = 0, prizesRed = 0, prizesBlue = 0 }

            , test "OpeningDraw decreases the drawing player's deck" <|
                \_ ->
                    let
                        group =
                            { raw = "A drew 7 cards for the opening hand."
                            , action = Action.OpeningDraw { player = "A", count = 7 }
                            , details = []
                            }
                    in
                    applyGroupToPiles "A" False emptyPiles group
                        |> .deckRed
                        |> Expect.equal 53

            , test "opponent OpeningDraw decreases blue deck" <|
                \_ ->
                    let
                        group =
                            { raw = "B drew 7 cards for the opening hand."
                            , action = Action.OpeningDraw { player = "B", count = 7 }
                            , details = []
                            }
                    in
                    applyGroupToPiles "A" False emptyPiles group
                        |> .deckBlue
                        |> Expect.equal 53

            , test "DrewCount in detail decreases deck" <|
                \_ ->
                    let
                        group =
                            { raw = "A played (sv1_90) Trainer."
                            , action = Action.PlayedTrainer { player = "A", card = { id = "sv1_90", name = "Trainer" } }
                            , details =
                                [ { raw = "A drew 3 cards."
                                  , action = Action.DrewCount { player = "A", count = 3 }
                                  , bullets = []
                                  }
                                ]
                            }
                    in
                    applyGroupToPiles "A" False emptyPiles group
                        |> .deckRed
                        |> Expect.equal 57

            , test "DiscardedCard increases discard pile" <|
                \_ ->
                    let
                        group =
                            { raw = "A discarded (sv1_50) Pikachu."
                            , action = Action.DiscardedCard { player = "A", card = { id = "sv1_50", name = "Pikachu" } }
                            , details = []
                            }
                    in
                    applyGroupToPiles "A" False emptyPiles group
                        |> .discardRed
                        |> Expect.equal 1

            , test "ShuffledInto increases deck" <|
                \_ ->
                    let
                        group =
                            { raw = "A shuffled (sv1_50) Pikachu into the deck."
                            , action = Action.ShuffledInto { player = "A", card = Just { id = "sv1_50", name = "Pikachu" }, count = Nothing }
                            , details = []
                            }
                    in
                    applyGroupToPiles "A" False emptyPiles group
                        |> .deckRed
                        |> Expect.equal 61

            , test "PlayedPokemon to ActiveSpot during setup sets 6 prizes and removes 6 from deck" <|
                \_ ->
                    let
                        group =
                            { raw = "A played (sv1_1) Bulbasaur to the Active Spot."
                            , action = Action.PlayedPokemon { player = "A", card = { id = "sv1_1", name = "Bulbasaur" }, position = Action.ActiveSpot }
                            , details = []
                            }
                    in
                    applyGroupToPiles "A" True emptyPiles group
                        |> (\p -> ( p.prizesRed, p.deckRed ))
                        |> Expect.equal ( 6, 54 )

            , test "PlayedPokemon to ActiveSpot mid-game does not change prizes" <|
                \_ ->
                    let
                        setupPiles =
                            { emptyPiles | prizesRed = 4, deckRed = 40 }

                        group =
                            { raw = "A played (sv1_1) Bulbasaur to the Active Spot."
                            , action = Action.PlayedPokemon { player = "A", card = { id = "sv1_1", name = "Bulbasaur" }, position = Action.ActiveSpot }
                            , details = []
                            }
                    in
                    applyGroupToPiles "A" False setupPiles group
                        |> (\p -> ( p.prizesRed, p.deckRed ))
                        |> Expect.equal ( 4, 40 )
            ]

        , describe "current play"
            [ test "TookPrize with CardAddedToHand details returns Just with drawn cards" <|
                \_ ->
                    let
                        players =
                            { red = "A", blue = "B" }

                        card1 =
                            { id = "sv6_130", name = "Dragapult ex" }

                        card2 =
                            { id = "sv6_165", name = "Unfair Stamp" }

                        group =
                            { raw = "A took 2 Prize cards."
                            , action = Action.TookPrize { player = "A", count = 2 }
                            , details =
                                [ { raw = "(sv6_130) Dragapult ex was added to A's hand."
                                  , action = Action.CardAddedToHand { card = Just card1, player = "A" }
                                  , bullets = []
                                  }
                                , { raw = "(sv6_165) Unfair Stamp was added to A's hand."
                                  , action = Action.CardAddedToHand { card = Just card2, player = "A" }
                                  , bullets = []
                                  }
                                ]
                            }
                    in
                    currentPlayFromGroup players group
                        |> Expect.equal
                            (Just
                                { player = "A"
                                , card = Nothing
                                , red = { discarded = [], shuffled = [], drawn = [ Just card1, Just card2 ], benched = [] }
                                , blue = { discarded = [], shuffled = [], drawn = [], benched = [] }
                                }
                            )

            , test "TookPrize with unknown prize cards returns Just with Nothing entries" <|
                \_ ->
                    let
                        players =
                            { red = "A", blue = "B" }

                        group =
                            { raw = "A took 2 Prize cards."
                            , action = Action.TookPrize { player = "A", count = 2 }
                            , details =
                                [ { raw = "A card was added to A's hand."
                                  , action = Action.CardAddedToHand { card = Nothing, player = "A" }
                                  , bullets = []
                                  }
                                , { raw = "A card was added to A's hand."
                                  , action = Action.CardAddedToHand { card = Nothing, player = "A" }
                                  , bullets = []
                                  }
                                ]
                            }
                    in
                    currentPlayFromGroup players group
                        |> Expect.equal
                            (Just
                                { player = "A"
                                , card = Nothing
                                , red = { discarded = [], shuffled = [], drawn = [ Nothing, Nothing ], benched = [] }
                                , blue = { discarded = [], shuffled = [], drawn = [], benched = [] }
                                }
                            )

            , test "TookPrize with no details returns Nothing" <|
                \_ ->
                    let
                        players =
                            { red = "A", blue = "B" }

                        group =
                            { raw = "A took a Prize card."
                            , action = Action.TookPrize { player = "A", count = 1 }
                            , details = []
                            }
                    in
                    currentPlayFromGroup players group
                        |> Expect.equal Nothing

            , test "non-trainer action returns Nothing" <|
                \_ ->
                    let
                        players =
                            { red = "A", blue = "B" }

                        group =
                            { raw = "A drew a card."
                            , action = Action.Drew { player = "A", card = Nothing }
                            , details = []
                            }
                    in
                    currentPlayFromGroup players group
                        |> Expect.equal Nothing

            , test "PlayedTrainer with no discards returns Just with empty discards" <|
                \_ ->
                    let
                        players =
                            { red = "A", blue = "B" }

                        card =
                            { id = "sv1_100", name = "Nest Ball" }

                        group =
                            { raw = "A played (sv1_100) Nest Ball."
                            , action = Action.PlayedTrainer { player = "A", card = card }
                            , details = []
                            }
                    in
                    currentPlayFromGroup players group
                        |> Expect.equal
                            (Just
                                { player = "A"
                                , card = Just card
                                , red = { discarded = [], shuffled = [], drawn = [], benched = [] }
                                , blue = { discarded = [], shuffled = [], drawn = [], benched = [] }
                                }
                            )

            , test "PlayedTrainer with DiscardedCard detail includes that card in discards" <|
                \_ ->
                    let
                        players =
                            { red = "A", blue = "B" }

                        played =
                            { id = "sv4_160", name = "Ultra Ball" }

                        discardedCard =
                            { id = "sv1_50", name = "Pikachu" }

                        group =
                            { raw = "A played (sv4_160) Ultra Ball."
                            , action = Action.PlayedTrainer { player = "A", card = played }
                            , details =
                                [ { raw = "A discarded (sv1_50) Pikachu."
                                  , action = Action.DiscardedCard { player = "A", card = discardedCard }
                                  , bullets = []
                                  }
                                ]
                            }
                    in
                    currentPlayFromGroup players group
                        |> Expect.equal
                            (Just
                                { player = "A"
                                , card = Just played
                                , red = { discarded = [ Just discardedCard ], shuffled = [], drawn = [], benched = [] }
                                , blue = { discarded = [], shuffled = [], drawn = [], benched = [] }
                                }
                            )

            , test "PlayedTrainer with Discarded+CardList bullet includes those cards in discards" <|
                \_ ->
                    let
                        players =
                            { red = "A", blue = "B" }

                        played =
                            { id = "sv4_160", name = "Ultra Ball" }

                        energy1 =
                            { id = "mee_1", name = "Basic Fire Energy" }

                        energy2 =
                            { id = "mee_4", name = "Basic Psychic Energy" }

                        group =
                            { raw = "A played (sv4_160) Ultra Ball."
                            , action = Action.PlayedTrainer { player = "A", card = played }
                            , details =
                                [ { raw = "A discarded 2 cards."
                                  , action = Action.Discarded { player = "A", count = 2 }
                                  , bullets =
                                        [ { raw = "(mee_1) Basic Fire Energy, (mee_4) Basic Psychic Energy"
                                          , action = Action.CardList [ energy1, energy2 ]
                                          }
                                        ]
                                  }
                                ]
                            }
                    in
                    currentPlayFromGroup players group
                        |> Expect.equal
                            (Just
                                { player = "A"
                                , card = Just played
                                , red = { discarded = [ Just energy1, Just energy2 ], shuffled = [], drawn = [], benched = [] }
                                , blue = { discarded = [], shuffled = [], drawn = [], benched = [] }
                                }
                            )

            , test "PlayedTrainer with DrewCount+CardList collects drawn cards and excludes them from nothing else" <|
                \_ ->
                    let
                        players =
                            { red = "A", blue = "B" }

                        played =
                            { id = "sv4_160", name = "Ultra Ball" }

                        drew1 =
                            { id = "sv1_1", name = "Bulbasaur" }

                        drew2 =
                            { id = "sv1_2", name = "Ivysaur" }

                        group =
                            { raw = "A played (sv4_160) Ultra Ball."
                            , action = Action.PlayedTrainer { player = "A", card = played }
                            , details =
                                [ { raw = "A drew 2 cards."
                                  , action = Action.DrewCount { player = "A", count = 2 }
                                  , bullets =
                                        [ { raw = "(sv1_1) Bulbasaur, (sv1_2) Ivysaur"
                                          , action = Action.CardList [ drew1, drew2 ]
                                          }
                                        ]
                                  }
                                ]
                            }
                    in
                    currentPlayFromGroup players group
                        |> Expect.equal
                            (Just
                                { player = "A"
                                , card = Just played
                                , red = { discarded = [], shuffled = [], drawn = [ Just drew1, Just drew2 ], benched = [] }
                                , blue = { discarded = [], shuffled = [], drawn = [], benched = [] }
                                }
                            )

            , test "PlayedTrainer with ShuffledInto detail collects shuffled cards" <|
                \_ ->
                    let
                        players =
                            { red = "A", blue = "B" }

                        played =
                            { id = "sv4_160", name = "Ultra Ball" }

                        shuffled1 =
                            { id = "sv1_1", name = "Bulbasaur" }

                        group =
                            { raw = "A played (sv4_160) Ultra Ball."
                            , action = Action.PlayedTrainer { player = "A", card = played }
                            , details =
                                [ { raw = "A shuffled (sv1_1) Bulbasaur into the deck."
                                  , action = Action.ShuffledInto { player = "A", card = Just shuffled1, count = Nothing }
                                  , bullets = []
                                  }
                                ]
                            }
                    in
                    currentPlayFromGroup players group
                        |> Expect.equal
                            (Just
                                { player = "A"
                                , card = Just played
                                , red = { discarded = [], shuffled = [ Just shuffled1 ], drawn = [], benched = [] }
                                , blue = { discarded = [], shuffled = [], drawn = [], benched = [] }
                                }
                            )

            , test "PlayedTrainer with ShuffledInto count+CardList bullet collects shuffled cards" <|
                \_ ->
                    let
                        players =
                            { red = "A", blue = "B" }

                        played =
                            { id = "sv9_108", name = "Lillie's Determination" }

                        dark =
                            { id = "mee_6", name = "Basic Darkness Energy" }

                        ultraBall =
                            { id = "sv4_160", name = "Ultra Ball" }

                        group =
                            { raw = "A played (sv9_108) Lillie's Determination."
                            , action = Action.PlayedTrainer { player = "A", card = played }
                            , details =
                                [ { raw = "A shuffled 2 cards into their deck."
                                  , action = Action.ShuffledInto { player = "A", card = Nothing, count = Just 2 }
                                  , bullets =
                                        [ { raw = "(mee_6) Basic Darkness Energy, (sv4_160) Ultra Ball"
                                          , action = Action.CardList [ dark, ultraBall ]
                                          }
                                        ]
                                  }
                                ]
                            }
                    in
                    currentPlayFromGroup players group
                        |> Expect.equal
                            (Just
                                { player = "A"
                                , card = Just played
                                , red = { discarded = [], shuffled = [ Just dark, Just ultraBall ], drawn = [], benched = [] }
                                , blue = { discarded = [], shuffled = [], drawn = [], benched = [] }
                                }
                            )

            , test "UsedAttack with DiscardedCard detail returns Just with discards" <|
                \_ ->
                    let
                        players =
                            { red = "A", blue = "B" }

                        attacker =
                            { player = "A", card = { id = "sv7_58", name = "Slowking" } }

                        discardedCard =
                            { id = "me4_61_ph", name = "Metagross" }

                        group =
                            { raw = "A's (sv7_58) Slowking used Seek Inspiration on B's (sv8-5_27) Wellspring Mask Ogerpon ex for 300 damage."
                            , action =
                                Action.UsedAttack
                                    { attacker = attacker
                                    , move = "Seek Inspiration"
                                    , target = Nothing
                                    , modifier = Nothing
                                    }
                            , details =
                                [ { raw = "A discarded (me4_61_ph) Metagross."
                                  , action = Action.DiscardedCard { player = "A", card = discardedCard }
                                  , bullets = []
                                  }
                                ]
                            }
                    in
                    currentPlayFromGroup players group
                        |> Expect.equal
                            (Just
                                { player = "A"
                                , card = Nothing
                                , red = { discarded = [ Just discardedCard ], shuffled = [], drawn = [], benched = [] }
                                , blue = { discarded = [], shuffled = [], drawn = [], benched = [] }
                                }
                            )

            , test "UsedAttack with no discard details returns Nothing" <|
                \_ ->
                    let
                        players =
                            { red = "A", blue = "B" }

                        attacker =
                            { player = "A", card = { id = "sv7_58", name = "Slowking" } }

                        group =
                            { raw = "A's (sv7_58) Slowking used Seek Inspiration on B's (sv8-5_27) Wellspring Mask Ogerpon ex for 300 damage."
                            , action =
                                Action.UsedAttack
                                    { attacker = attacker
                                    , move = "Seek Inspiration"
                                    , target = Nothing
                                    , modifier = Nothing
                                    }
                            , details = []
                            }
                    in
                    currentPlayFromGroup players group
                        |> Expect.equal Nothing
            ]
        , describe "bench state"
            [ test "DrewAndPlayed parses correctly" <|
                \_ ->
                    Action.parseAction "A drew 2 cards and played them to the Bench."
                        |> Expect.equal (Action.DrewAndPlayed { player = "A", count = 2, position = Action.BenchSpot })
            , test "DrewAndPlayed group has detail with CardList bullet" <|
                \_ ->
                    let
                        group =
                            Action.groupLines
                                [ TopLine "A played (me2-5_184) Buddy-Buddy Poffin."
                                , DetailLine "A drew 2 cards and played them to the Bench."
                                , BulletLine "(me2-5_171) Fan Rotom, (sv6_56) Froakie"
                                , DetailLine "A shuffled their deck."
                                ]
                                |> List.head
                                |> Maybe.withDefault
                                    { raw = "", action = Action.UnknownAction "", details = [] }
                        drewDetail =
                            group.details
                                |> List.filter (\d -> d.action == Action.DrewAndPlayed { player = "A", count = 2, position = Action.BenchSpot })
                                |> List.head
                    in
                    drewDetail
                        |> Maybe.map (\d -> List.length d.bullets)
                        |> Expect.equal (Just 1)
            , test "DrewAndPlayed detail adds cards to bench via bullet CardList" <|
                \_ ->
                    let
                        fanRotom = { id = "me2-5_171", name = "Fan Rotom" }
                        froakie  = { id = "sv6_56",    name = "Froakie" }
                        group =
                            Action.groupLines
                                [ TopLine "A played (me2-5_184) Buddy-Buddy Poffin."
                                , DetailLine "A drew 2 cards and played them to the Bench."
                                , BulletLine "(me2-5_171) Fan Rotom, (sv6_56) Froakie"
                                , DetailLine "A shuffled their deck."
                                ]
                                |> List.head
                                |> Maybe.withDefault
                                    { raw = "", action = Action.UnknownAction "", details = [] }
                        bench =
                            applyGroupToBench "A" emptyActive emptyBench group
                    in
                    bench.red
                        |> Expect.equalLists [ fanRotom, froakie ]
            ]
        , describe "attachment instance tracking (regression)"
            [ test "KnockedOut removes only the KO'd instance's attachments, leaving a bench duplicate untouched" <|
                \_ ->
                    let
                        ( inst, atts ) =
                            finalAttachments
                                [ TopLine "A played (aaa_1) Foo to the Bench."
                                , TopLine "A played (aaa_1) Foo to the Active Spot."
                                , TopLine "A attached (mee_8) Basic Metal Energy to (aaa_1) Foo on the Bench."
                                , TopLine "A attached (mee_8) Basic Metal Energy to (aaa_1) Foo in the Active Spot."
                                , TopLine "B's (bbb_1) Bar used Tackle on A's (aaa_1) Foo for 200 damage."
                                , TopLine "A's (aaa_1) Foo was Knocked Out!"
                                , DetailLine "1 cards were discarded from A's (aaa_1) Foo."
                                , BulletLine "(mee_8) Basic Metal Energy"
                                ]

                        benchItems =
                            firstInstance "A" "aaa_1" inst
                                |> Maybe.map (lookupAttachments atts)
                                |> Maybe.withDefault []
                    in
                    List.length benchItems |> Expect.equal 1
            , test "NCardsDiscardedFrom in a KnockedOut group does not fall back to a bench duplicate" <|
                \_ ->
                    let
                        ( inst, atts ) =
                            finalAttachments
                                [ TopLine "A played (aaa_1) Foo to the Bench."
                                , TopLine "A played (aaa_1) Foo to the Active Spot."
                                , TopLine "A attached (mee_8) Basic Metal Energy to (aaa_1) Foo on the Bench."
                                , TopLine "A attached (mee_8) Basic Metal Energy to (aaa_1) Foo in the Active Spot."
                                , TopLine "A attached (mee_8) Basic Metal Energy to (aaa_1) Foo in the Active Spot."
                                , TopLine "B's (bbb_1) Bar used Tackle on A's (aaa_1) Foo for 200 damage."
                                , TopLine "A's (aaa_1) Foo was Knocked Out!"
                                , DetailLine "2 cards were discarded from A's (aaa_1) Foo."
                                , BulletLine "(mee_8) Basic Metal Energy, (mee_8) Basic Metal Energy"
                                ]

                        benchItems =
                            firstInstance "A" "aaa_1" inst
                                |> Maybe.map (lookupAttachments atts)
                                |> Maybe.withDefault []
                    in
                    List.length benchItems |> Expect.equal 1
            , test "Retreated moves the specific active instance's attachments to bench, not a duplicate's" <|
                \_ ->
                    let
                        ( inst, atts ) =
                            finalAttachments
                                [ TopLine "A played (aaa_1) Foo to the Bench."
                                , TopLine "A played (aaa_1) Foo to the Active Spot."
                                , TopLine "A attached (mee_8) Basic Metal Energy to (aaa_1) Foo on the Bench."
                                , TopLine "A attached (mee_1) Basic Grass Energy to (aaa_1) Foo in the Active Spot."
                                , TopLine "A retreated (aaa_1) Foo to the Bench."
                                ]

                        active =
                            Dict.get ( "A", "aaa_1" ) inst.activeSpot |> Maybe.andThen identity
                    in
                    -- retreatToFront puts the retreated instance at the bench head, so
                    -- firstInstance (active-first-then-bench) now surfaces it: its items
                    -- should be the energy that was attached while it was Active, not the
                    -- other instance's Bench-attached energy.
                    ( active
                    , firstInstance "A" "aaa_1" inst |> Maybe.map (lookupAttachments atts)
                    )
                        |> Expect.equal ( Nothing, Just [ { id = "mee_1", name = "Basic Grass Energy" } ] )
            , test "NCardsDiscardedFrom on a retreat only strips the retreated instance, not the untouched bench duplicate" <|
                \_ ->
                    let
                        ( inst, atts ) =
                            finalAttachments
                                [ TopLine "A played (aaa_1) Foo to the Bench."
                                , TopLine "A played (aaa_1) Foo to the Active Spot."
                                , TopLine "A attached (mee_8) Basic Metal Energy to (aaa_1) Foo on the Bench."
                                , TopLine "A attached (mee_8) Basic Metal Energy to (aaa_1) Foo in the Active Spot."
                                , TopLine "A retreated (aaa_1) Foo to the Bench."
                                , DetailLine "1 cards were discarded from A's (aaa_1) Foo."
                                , BulletLine "(mee_8) Basic Metal Energy"
                                ]

                        benchCounts =
                            Dict.get ( "A", "aaa_1" ) inst.bench
                                |> Maybe.withDefault []
                                |> List.map (\iid -> lookupAttachments atts iid |> List.length)
                    in
                    List.sort benchCounts |> Expect.equal [ 0, 1 ]
            , test "MovedToActive promotes the untouched bench instance, not the one that just retreated" <|
                \_ ->
                    let
                        -- Instance ids are assigned in appearance order starting at 0:
                        -- the 1st "played to the Bench" line is instance 0, the 2nd is
                        -- instance 1. Instance 0 gets promoted then retreated; the final
                        -- MovedToActive must promote instance 1 instead of re-promoting 0.
                        ( inst, _ ) =
                            finalAttachments
                                [ TopLine "A played (aaa_1) Foo to the Bench."
                                , TopLine "A played (aaa_1) Foo to the Bench."
                                , TopLine "A's (aaa_1) Foo is now in the Active Spot."
                                , TopLine "A retreated (aaa_1) Foo to the Bench."
                                , TopLine "A's (aaa_1) Foo is now in the Active Spot."
                                ]

                        finalActive =
                            Dict.get ( "A", "aaa_1" ) inst.activeSpot |> Maybe.andThen identity
                    in
                    finalActive |> Expect.equal (Just 1)
            , test "Switched with the same card id swaps active and bench instances without losing either" <|
                \_ ->
                    let
                        ( inst, atts ) =
                            finalAttachments
                                [ TopLine "A played (aaa_1) Foo to the Bench."
                                , TopLine "A played (aaa_1) Foo to the Active Spot."
                                , TopLine "A attached (mee_8) Basic Metal Energy to (aaa_1) Foo on the Bench."
                                , TopLine "A attached (mee_1) Basic Grass Energy to (aaa_1) Foo in the Active Spot."
                                , TopLine "A's (aaa_1) Foo was switched with A's (aaa_1) Foo to become the Active Pokémon."
                                ]

                        activeItems =
                            Dict.get ( "A", "aaa_1" ) inst.activeSpot
                                |> Maybe.andThen identity
                                |> Maybe.map (lookupAttachments atts)
                                |> Maybe.withDefault []
                    in
                    activeItems |> Expect.equal [ { id = "mee_8", name = "Basic Metal Energy" } ]
            ]
        , describe "special conditions (regression)"
            [ test "ConditionApplied sets a condition on the Active instance" <|
                \_ ->
                    let
                        ( inst, conds ) =
                            finalConditions
                                [ TopLine "A played (aaa_1) Foo to the Active Spot."
                                , TopLine "A's (aaa_1) Foo is now Poisoned."
                                ]
                    in
                    firstInstance "A" "aaa_1" inst
                        |> Maybe.map (lookupConditions conds)
                        |> Expect.equal (Just [ "Poisoned" ])
            , test "ConditionRemoved clears a previously applied condition" <|
                \_ ->
                    let
                        ( inst, conds ) =
                            finalConditions
                                [ TopLine "A played (aaa_1) Foo to the Active Spot."
                                , TopLine "A's (aaa_1) Foo is now Confused."
                                , TopLine "A's (aaa_1) Foo is no longer Confused."
                                ]
                    in
                    firstInstance "A" "aaa_1" inst
                        |> Maybe.map (lookupConditions conds)
                        |> Expect.equal (Just [])
            , test "Retreated clears every condition on the instance leaving the Active Spot" <|
                \_ ->
                    let
                        ( inst, conds ) =
                            finalConditions
                                [ TopLine "A played (aaa_1) Foo to the Active Spot."
                                , TopLine "A's (aaa_1) Foo is now Confused."
                                , TopLine "A retreated (aaa_1) Foo to the Bench."
                                ]
                    in
                    -- retreated instance moves to the bench head, so firstInstance
                    -- (active-first-then-bench) still surfaces it.
                    firstInstance "A" "aaa_1" inst
                        |> Maybe.map (lookupConditions conds)
                        |> Expect.equal (Just [])
            , test "Same-card Switched clears the instance that was Active, leaving the other instance's (empty) state alone" <|
                \_ ->
                    let
                        ( inst, conds ) =
                            finalConditions
                                [ TopLine "A played (aaa_1) Foo to the Bench."
                                , TopLine "A played (aaa_1) Foo to the Active Spot."
                                , TopLine "A's (aaa_1) Foo is now Confused."
                                , TopLine "A's (aaa_1) Foo was switched with A's (aaa_1) Foo to become the Active Pokémon."
                                ]

                        -- Instance 1 (the 2nd "played to the Bench" line) was Active and
                        -- Confused, then got swapped to the Bench by the same-card switch.
                        formerlyActiveConditions =
                            lookupConditions conds 1
                    in
                    formerlyActiveConditions |> Expect.equal []
            , test "KnockedOut clears the KO'd instance's conditions" <|
                \_ ->
                    let
                        ( inst, conds ) =
                            finalConditions
                                [ TopLine "A played (aaa_1) Foo to the Active Spot."
                                , TopLine "A's (aaa_1) Foo is now Poisoned."
                                , TopLine "B's (bbb_1) Bar used Tackle on A's (aaa_1) Foo for 200 damage."
                                , TopLine "A's (aaa_1) Foo was Knocked Out!"
                                ]
                    in
                    lookupConditions conds 0 |> Expect.equal []
            , test "ConditionRemoved inside an Evolved group's detail resolves via the post-evolution card id" <|
                \_ ->
                    let
                        -- Regression: the pokemon's card id changes mid-group (Drakloak ->
                        -- Dragapult). A "no longer Confused" detail for the NEW card id must
                        -- resolve against postInstances, since preInstances only knows the
                        -- pokemon under its OLD (pre-evolution) card id.
                        ( inst, conds ) =
                            finalConditions
                                [ TopLine "A played (aaa_1) Drakloak to the Active Spot."
                                , TopLine "A's (aaa_1) Drakloak is now Confused."
                                , TopLine "A evolved (aaa_1) Drakloak to (bbb_1) Dragapult in the Active Spot."
                                , DetailLine "A's (bbb_1) Dragapult is no longer Confused."
                                ]
                    in
                    firstInstance "A" "bbb_1" inst
                        |> Maybe.map (lookupConditions conds)
                        |> Expect.equal (Just [])
            , test "ConditionApplied on a benched (non-Active) pokemon is a no-op" <|
                \_ ->
                    let
                        ( inst, conds ) =
                            finalConditions
                                [ TopLine "A played (aaa_1) Foo to the Bench."
                                , TopLine "A's (aaa_1) Foo is now Confused."
                                ]
                    in
                    firstInstance "A" "aaa_1" inst
                        |> Maybe.map (lookupConditions conds)
                        |> Expect.equal (Just [])
            ]
        ]



-- HELPERS


{-| Run a flat list of replay lines (as one section's worth of groups) through
the same instance-tracking + attachment pipeline the app uses, returning the
final InstanceState and AttachmentState. Mirrors Main's private
collectAndCorrectGroups/computeAttachments using only exposed functions.
-}
finalAttachments : List Replay.ReplayLine -> ( InstanceState, AttachmentState )
finalAttachments lines =
    let
        players =
            { red = "A", blue = "B" }
    in
    Action.groupLines lines
        |> List.foldl
            (\group ( instState, atts ) ->
                let
                    corrected =
                        correctGroupPlayers players instState group

                    newInstState =
                        applyGroupToInstances corrected instState
                in
                ( newInstState, applyGroupToAttachments instState newInstState corrected atts )
            )
            ( emptyInstances, emptyAttachments )


{-| Same pipeline as finalAttachments, but for special-condition tracking. -}
finalConditions : List Replay.ReplayLine -> ( InstanceState, ConditionState )
finalConditions lines =
    let
        players =
            { red = "A", blue = "B" }
    in
    Action.groupLines lines
        |> List.foldl
            (\group ( instState, conds ) ->
                let
                    corrected =
                        correctGroupPlayers players instState group

                    newInstState =
                        applyGroupToInstances corrected instState
                in
                ( newInstState, applyGroupToConditions instState newInstState corrected conds )
            )
            ( emptyInstances, emptyConditions )


sectionKind : Replay.Section -> String
sectionKind section =
    case section of
        SetupSection _ ->
            "setup"

        TurnSection _ _ ->
            "turn"

        CheckupSection _ ->
            "checkup"

        ResultSection _ ->
            "result"


firstTurn : Replay.Replay -> Maybe Replay.Turn
firstTurn replay =
    replay.sections
        |> List.filterMap
            (\s ->
                case s of
                    TurnSection turn _ ->
                        Just turn

                    _ ->
                        Nothing
            )
        |> List.head


firstTurnLines : Replay.Replay -> List Replay.ReplayLine
firstTurnLines replay =
    replay.sections
        |> List.filterMap
            (\s ->
                case s of
                    TurnSection _ lines ->
                        Just lines

                    _ ->
                        Nothing
            )
        |> List.head
        |> Maybe.withDefault []


setupWith : { redFirst : Bool } -> String
setupWith { redFirst } =
    let
        revealedDraw player =
            [ player ++ " drew 7 cards for the opening hand."
            , "- 7 drawn cards."
            , "   • Card1, Card2"
            ]

        hiddenDraw player =
            [ player ++ " drew 7 cards for the opening hand."
            , "- 7 drawn cards."
            ]

        draws =
            if redFirst then
                revealedDraw "A" ++ hiddenDraw "B"

            else
                hiddenDraw "B" ++ revealedDraw "A"
    in
    String.join "\n" ([ "Setup" ] ++ draws)


multiSectionInput : String
multiSectionInput =
    String.join "\n"
        [ "Setup"
        , "Some setup line."
        , ""
        , "Turn # 1 - A's Turn"
        , "A drew a card."
        , ""
        , "Pokémon Checkup"
        , "1 damage counter placed."
        , ""
        ]
