module MainTest exposing (suite)

import Dict
import Expect
import Http
import Action exposing (CardRef)
import Main exposing (CardData, CardPopup(..), CurrentPlay, HandState, Model(..), Msg(..), PileState, BenchState, applyGroupToHand, applyGroupToPiles, applyGroupToBench, currentPlayFromGroup, emptyHand, emptyPiles, emptyBench, init, update)
import Replay exposing (ReplayLine(..), Section(..))
import Test exposing (Test, describe, test)


cardDataWithImage : String -> CardData
cardDataWithImage url =
    { imageUrl = Just url, attacks = [], abilities = [] }


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
                    init { replayUrl = "", sectionIndex = 0, groupIndex = 0, flipOpponent = True }
                        |> Tuple.first
                        |> Expect.equal (EnteringUrl "")
            , test "whitespace flags start in EnteringUrl" <|
                \_ ->
                    init { replayUrl = "   ", sectionIndex = 0, groupIndex = 0, flipOpponent = True }
                        |> Tuple.first
                        |> Expect.equal (EnteringUrl "")
            , test "url flags start in Loading" <|
                \_ ->
                    init { replayUrl = "https://example.com/replay.txt", sectionIndex = 0, groupIndex = 0, flipOpponent = True }
                        |> Tuple.first
                        |> Expect.equal (Loading "https://example.com/replay.txt" 0 0 True)
            , test "url flags are trimmed" <|
                \_ ->
                    init { replayUrl = "  https://example.com/replay.txt  ", sectionIndex = 0, groupIndex = 0, flipOpponent = True }
                        |> Tuple.first
                        |> Expect.equal (Loading "https://example.com/replay.txt" 0 0 True)
            , test "section index is preserved in Loading state" <|
                \_ ->
                    init { replayUrl = "https://example.com/replay.txt", sectionIndex = 3, groupIndex = 0, flipOpponent = True }
                        |> Tuple.first
                        |> Expect.equal (Loading "https://example.com/replay.txt" 3 0 True)
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
                    update (GotReplay (Ok content)) (Loading url 0 0 True)
                        |> Tuple.first
                        |> Expect.equal (Loaded url (Replay.parse content) 0 0 Nothing Dict.empty True)
            , test "NetworkError triggers proxy retry" <|
                \_ ->
                    update (GotReplay (Err Http.NetworkError)) (Loading "https://example.com" 0 0 True)
                        |> Tuple.first
                        |> Expect.equal (Retrying "https://example.com" 0 0 True)
            , test "404 transitions to Failed with friendly message" <|
                \_ ->
                    update (GotReplay (Err (Http.BadStatus 404))) (Loading "https://example.com" 0 0 True)
                        |> Tuple.first
                        |> Expect.equal (Failed "https://example.com" "No replay content found — check the URL")
            , test "other errors transition to Failed without retrying" <|
                \_ ->
                    update (GotReplay (Err Http.Timeout)) (Loading "https://example.com" 0 0 True)
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
                    update (GotReplay (Ok content)) (Retrying url 0 0 True)
                        |> Tuple.first
                        |> Expect.equal (Loaded url (Replay.parse content) 0 0 Nothing Dict.empty True)
            , test "proxy 404 transitions to Failed with friendly message" <|
                \_ ->
                    update (GotReplay (Err (Http.BadStatus 404))) (Retrying "https://example.com" 0 0 True)
                        |> Tuple.first
                        |> Expect.equal (Failed "https://example.com" "No replay content found — check the URL")
            , test "proxy error transitions to Failed" <|
                \_ ->
                    update (GotReplay (Err Http.Timeout)) (Retrying "https://example.com" 0 0 True)
                        |> Tuple.first
                        |> Expect.equal (Failed "https://example.com" "Request timed out")
            , test "empty content transitions to Failed" <|
                \_ ->
                    update (GotReplay (Ok "")) (Loading "https://example.com" 0 0 True)
                        |> Tuple.first
                        |> Expect.equal (Failed "https://example.com" "No replay content found — check the URL")
            , test "unrecognised content from proxy transitions to Failed" <|
                \_ ->
                    update (GotReplay (Ok "<html>404 Not Found</html>")) (Retrying "https://example.com" 0 0 True)
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
                    update (GotReplay (Ok curlyContent)) (Loading url 0 0 True)
                        |> Tuple.first
                        |> Expect.equal (Loaded url (Replay.parse normalizedContent) 0 0 Nothing Dict.empty True)
            , test "deep-linked section index is restored on load" <|
                \_ ->
                    let
                        url =
                            "https://example.com/replay.txt"

                        content =
                            "Setup\nSome setup.\nTurn # 1 - A's Turn\nA drew a card.\n"
                    in
                    update (GotReplay (Ok content)) (Loading url 1 0 True)
                        |> Tuple.first
                        |> Expect.equal (Loaded url (Replay.parse content) 1 0 Nothing Dict.empty True)
            , test "out-of-range section index is clamped to last section" <|
                \_ ->
                    let
                        url =
                            "https://example.com/replay.txt"

                        content =
                            "Setup\nSome setup.\n"
                    in
                    update (GotReplay (Ok content)) (Loading url 99 0 True)
                        |> Tuple.first
                        |> Expect.equal (Loaded url (Replay.parse content) 0 0 Nothing Dict.empty True)
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
                    update FirstSection (Loaded "url" replay 1 0 Nothing Dict.empty True)
                        |> Tuple.first
                        |> Expect.equal (Loaded "url" replay 0 0 Nothing Dict.empty True)
            , test "LastSection jumps to the last index" <|
                \_ ->
                    let
                        content =
                            "Setup\nSome setup.\nTurn # 1 - A's Turn\nA drew a card.\n"

                        replay =
                            Replay.parse content
                    in
                    update LastSection (Loaded "url" replay 0 0 Nothing Dict.empty True)
                        |> Tuple.first
                        |> Expect.equal (Loaded "url" replay 1 0 Nothing Dict.empty True)
            , test "NextSection increments the index" <|
                \_ ->
                    let
                        content =
                            "Setup\nSome setup.\nTurn # 1 - A's Turn\nA drew a card.\n"

                        replay =
                            Replay.parse content
                    in
                    update NextSection (Loaded "url" replay 0 0 Nothing Dict.empty True)
                        |> Tuple.first
                        |> Expect.equal (Loaded "url" replay 1 0 Nothing Dict.empty True)
            , test "NextSection does not go past the last section" <|
                \_ ->
                    let
                        content =
                            "Setup\nSome setup.\n"

                        replay =
                            Replay.parse content
                    in
                    update NextSection (Loaded "url" replay 0 0 Nothing Dict.empty True)
                        |> Tuple.first
                        |> Expect.equal (Loaded "url" replay 0 0 Nothing Dict.empty True)
            , test "PrevSection decrements the index" <|
                \_ ->
                    let
                        content =
                            "Setup\nSome setup.\nTurn # 1 - A's Turn\nA drew a card.\n"

                        replay =
                            Replay.parse content
                    in
                    update PrevSection (Loaded "url" replay 1 0 Nothing Dict.empty True)
                        |> Tuple.first
                        |> Expect.equal (Loaded "url" replay 0 0 Nothing Dict.empty True)
            , test "PrevSection does not go below zero" <|
                \_ ->
                    let
                        content =
                            "Setup\nSome setup.\n"

                        replay =
                            Replay.parse content
                    in
                    update PrevSection (Loaded "url" replay 0 0 Nothing Dict.empty True)
                        |> Tuple.first
                        |> Expect.equal (Loaded "url" replay 0 0 Nothing Dict.empty True)
            , test "NextSection reveals next group within section when more groups exist" <|
                \_ ->
                    let
                        -- One section with 2 action groups
                        content =
                            "Setup\nA did something.\nB did something.\n"

                        replay =
                            Replay.parse content
                    in
                    update NextSection (Loaded "url" replay 0 0 Nothing Dict.empty True)
                        |> Tuple.first
                        |> Expect.equal (Loaded "url" replay 0 1 Nothing Dict.empty True)
            , test "PrevSection hides last group within section when groupIndex > 0" <|
                \_ ->
                    let
                        content =
                            "Setup\nA did something.\nB did something.\n"

                        replay =
                            Replay.parse content
                    in
                    update PrevSection (Loaded "url" replay 0 1 Nothing Dict.empty True)
                        |> Tuple.first
                        |> Expect.equal (Loaded "url" replay 0 0 Nothing Dict.empty True)
            ]
        , describe "card popup"
            [ test "CardClicked sets FetchingCard popup for a valid id" <|
                \_ ->
                    let
                        replay =
                            Replay.parse "Setup\nSome setup.\n"
                    in
                    update (CardClicked "sv4_160_ph") (Loaded "url" replay 0 0 Nothing Dict.empty True)
                        |> Tuple.first
                        |> Expect.equal (Loaded "url" replay 0 0 (Just (FetchingCard "sv4_160_ph")) Dict.empty True)
            , test "CardClicked with unparseable id shows CardNotFound immediately" <|
                \_ ->
                    let
                        replay =
                            Replay.parse "Setup\nSome setup.\n"
                    in
                    update (CardClicked "nounderscore") (Loaded "url" replay 0 0 Nothing Dict.empty True)
                        |> Tuple.first
                        |> Expect.equal (Loaded "url" replay 0 0 (Just (CardNotFound "nounderscore")) Dict.empty True)
            , test "GotCardImage with valid JSON shows card image" <|
                \_ ->
                    let
                        replay =
                            Replay.parse "Setup\nSome setup.\n"

                        json =
                            "{\"id\":\"swsh1-1\",\"image\":\"https://assets.tcgdex.net/en/swsh/swsh1/1\"}"
                    in
                    -- FetchingCard is set by CardClicked before the HTTP response arrives
                    update (GotCardImage "swsh1-1" (Ok json)) (Loaded "url" replay 0 0 (Just (FetchingCard "swsh1-1")) Dict.empty True)
                        |> Tuple.first
                        |> Expect.equal (Loaded "url" replay 0 0 (Just (ShowingCard "swsh1-1" (cardDataWithImage "https://assets.tcgdex.net/en/swsh/swsh1/1"))) (Dict.fromList [ ( "swsh1-1", cardDataWithImage "https://assets.tcgdex.net/en/swsh/swsh1/1" ) ]) True)
            , test "GotCardImage with invalid JSON shows CardNotFound" <|
                \_ ->
                    let
                        replay =
                            Replay.parse "Setup\nSome setup.\n"

                        emptyCardData =
                            { imageUrl = Nothing, attacks = [], abilities = [] }
                    in
                    update (GotCardImage "swsh1-1" (Ok "{\"error\":\"not found\"}")) (Loaded "url" replay 0 0 (Just (FetchingCard "swsh1-1")) Dict.empty True)
                        |> Tuple.first
                        |> Expect.equal (Loaded "url" replay 0 0 (Just (CardNotFound "swsh1-1")) (Dict.fromList [ ( "swsh1-1", emptyCardData ) ]) True)
            , test "GotCardImage with HTTP error shows CardNotFound" <|
                \_ ->
                    let
                        replay =
                            Replay.parse "Setup\nSome setup.\n"
                    in
                    update (GotCardImage "swsh1-1" (Err Http.NetworkError)) (Loaded "url" replay 0 0 (Just (FetchingCard "swsh1-1")) Dict.empty True)
                        |> Tuple.first
                        |> Expect.equal (Loaded "url" replay 0 0 (Just (CardNotFound "swsh1-1")) Dict.empty True)
            , test "GotCardImage as background hand fetch does not open popup" <|
                \_ ->
                    let
                        replay =
                            Replay.parse "Setup\nSome setup.\n"

                        json =
                            "{\"id\":\"swsh1-1\",\"image\":\"https://assets.tcgdex.net/en/swsh/swsh1/1\"}"
                    in
                    -- No FetchingCard popup = background prefetch; should just update cache silently
                    update (GotCardImage "swsh1-1" (Ok json)) (Loaded "url" replay 0 0 Nothing Dict.empty True)
                        |> Tuple.first
                        |> Expect.equal (Loaded "url" replay 0 0 Nothing (Dict.fromList [ ( "swsh1-1", cardDataWithImage "https://assets.tcgdex.net/en/swsh/swsh1/1" ) ]) True)
            , test "CloseCard removes the popup" <|
                \_ ->
                    let
                        replay =
                            Replay.parse "Setup\nSome setup.\n"
                    in
                    update CloseCard (Loaded "url" replay 0 0 (Just (CardNotFound "swsh1-1")) Dict.empty True)
                        |> Tuple.first
                        |> Expect.equal (Loaded "url" replay 0 0 Nothing Dict.empty True)
            , test "MoveClicked on cache hit shows ShowingMove" <|
                \_ ->
                    let
                        replay = Replay.parse "Setup\nSome setup.\n"
                        ability = { abilityType = "Ability", name = "Recon Directive", effect = "Once during your turn..." }
                        cardData = { imageUrl = Just "https://assets.tcgdex.net/en/sv/sv08.5/072", attacks = [], abilities = [ ability ] }
                        cache = Dict.fromList [ ( "sv8-5_72_sph", cardData ) ]
                    in
                    update (MoveClicked "sv8-5_72_sph" "Recon Directive") (Loaded "url" replay 0 0 Nothing cache True)
                        |> Tuple.first
                        |> Expect.equal (Loaded "url" replay 0 0 (Just (ShowingMove cardData "Recon Directive")) cache True)
            , test "MoveClicked on cache miss shows FetchingMove" <|
                \_ ->
                    let
                        replay = Replay.parse "Setup\nSome setup.\n"
                    in
                    update (MoveClicked "sv04_160" "Tackle") (Loaded "url" replay 0 0 Nothing Dict.empty True)
                        |> Tuple.first
                        |> Expect.equal (Loaded "url" replay 0 0 (Just (FetchingMove "sv04_160" "Tackle")) Dict.empty True)
            , test "GotCardImage when FetchingMove resolves to ShowingMove" <|
                \_ ->
                    let
                        replay = Replay.parse "Setup\nSome setup.\n"
                        json = "{\"image\":\"https://assets.tcgdex.net/en/sv/sv04/160\",\"attacks\":[{\"name\":\"Tackle\",\"cost\":[\"Colorless\"],\"damage\":10}],\"abilities\":[]}"
                        expectedData = { imageUrl = Just "https://assets.tcgdex.net/en/sv/sv04/160", attacks = [ { name = "Tackle", cost = [ "Colorless" ], damage = "10", effect = "" } ], abilities = [] }
                    in
                    update (GotCardImage "sv04_160" (Ok json)) (Loaded "url" replay 0 0 (Just (FetchingMove "sv04_160" "Tackle")) Dict.empty True)
                        |> Tuple.first
                        |> Expect.equal (Loaded "url" replay 0 0 (Just (ShowingMove expectedData "Tackle")) (Dict.fromList [ ( "sv04_160", expectedData ) ]) True)
            , test "GotCardImage stores attacks and abilities in cache" <|
                \_ ->
                    let
                        replay = Replay.parse "Setup\nSome setup.\n"
                        json = "{\"image\":\"https://example.com/img\",\"attacks\":[{\"name\":\"Scratch\",\"cost\":[\"Colorless\"],\"damage\":10}],\"abilities\":[{\"type\":\"Ability\",\"name\":\"Swift Run\",\"effect\":\"Once per turn.\"}]}"
                        expectedData =
                            { imageUrl = Just "https://example.com/img"
                            , attacks = [ { name = "Scratch", cost = [ "Colorless" ], damage = "10", effect = "" } ]
                            , abilities = [ { abilityType = "Ability", name = "Swift Run", effect = "Once per turn." } ]
                            }
                    in
                    update (GotCardImage "sv04_001" (Ok json)) (Loaded "url" replay 0 0 (Just (FetchingCard "sv04_001")) Dict.empty True)
                        |> Tuple.first
                        |> Expect.equal (Loaded "url" replay 0 0 (Just (ShowingCard "sv04_001" expectedData)) (Dict.fromList [ ( "sv04_001", expectedData ) ]) True)
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
                    update (CardClicked "sv04_160") (Loaded "url" replay 0 0 Nothing cache True)
                        |> Tuple.first
                        |> Expect.equal (Loaded "url" replay 0 0 (Just (ShowingCard "sv04_160" (cardDataWithImage imageUrl))) cache True)
            , test "CardClicked on cache miss shows FetchingCard" <|
                \_ ->
                    let
                        replay =
                            Replay.parse "Setup\nSome setup.\n"
                    in
                    update (CardClicked "sv04_160") (Loaded "url" replay 0 0 Nothing Dict.empty True)
                        |> Tuple.first
                        |> Expect.equal (Loaded "url" replay 0 0 (Just (FetchingCard "sv04_160")) Dict.empty True)
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
                    update NextSection (Loaded "url" replay 0 0 Nothing cache True)
                        |> Tuple.first
                        |> Expect.equal (Loaded "url" replay 1 0 Nothing cache True)
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
                    update (GotCardImage "swsh1-1" (Ok json)) (Loaded "url" replay 0 0 (Just (FetchingCard "swsh1-1")) priorCache True)
                        |> Tuple.first
                        |> Expect.equal (Loaded "url" replay 0 0 (Just (ShowingCard "swsh1-1" (cardDataWithImage newUrl))) expectedCache True)
            , test "GotCardImage network error does not populate the cache" <|
                \_ ->
                    let
                        replay =
                            Replay.parse "Setup\nSome setup.\n"
                    in
                    update (GotCardImage "sv04_160" (Err Http.NetworkError)) (Loaded "url" replay 0 0 (Just (FetchingCard "sv04_160")) Dict.empty True)
                        |> Tuple.first
                        |> Expect.equal (Loaded "url" replay 0 0 (Just (CardNotFound "sv04_160")) Dict.empty True)
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
                    update (CardClicked "sv04_160") (Loaded "url" replay 0 0 Nothing cache True)
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
            [ test "emptyPiles starts with 60 in each deck and 0 in each discard" <|
                \_ ->
                    emptyPiles
                        |> Expect.equal { deckRed = 60, deckBlue = 60, discardRed = 0, discardBlue = 0, prizesRed = 6, prizesBlue = 6 }

            , test "OpeningDraw decreases the drawing player's deck" <|
                \_ ->
                    let
                        group =
                            { raw = "A drew 7 cards for the opening hand."
                            , action = Action.OpeningDraw { player = "A", count = 7 }
                            , details = []
                            }
                    in
                    applyGroupToPiles "A" emptyPiles group
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
                    applyGroupToPiles "A" emptyPiles group
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
                    applyGroupToPiles "A" emptyPiles group
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
                    applyGroupToPiles "A" emptyPiles group
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
                    applyGroupToPiles "A" emptyPiles group
                        |> .deckRed
                        |> Expect.equal 61
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
                                , red = { discarded = [], shuffled = [], drawn = [ Just card1, Just card2 ] }
                                , blue = { discarded = [], shuffled = [], drawn = [] }
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
                                , red = { discarded = [], shuffled = [], drawn = [ Nothing, Nothing ] }
                                , blue = { discarded = [], shuffled = [], drawn = [] }
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
                                , red = { discarded = [], shuffled = [], drawn = [] }
                                , blue = { discarded = [], shuffled = [], drawn = [] }
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
                                , red = { discarded = [ Just discardedCard ], shuffled = [], drawn = [] }
                                , blue = { discarded = [], shuffled = [], drawn = [] }
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
                                , red = { discarded = [ Just energy1, Just energy2 ], shuffled = [], drawn = [] }
                                , blue = { discarded = [], shuffled = [], drawn = [] }
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
                                , red = { discarded = [], shuffled = [], drawn = [ Just drew1, Just drew2 ] }
                                , blue = { discarded = [], shuffled = [], drawn = [] }
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
                                , red = { discarded = [], shuffled = [ Just shuffled1 ], drawn = [] }
                                , blue = { discarded = [], shuffled = [], drawn = [] }
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
                                , red = { discarded = [], shuffled = [ Just dark, Just ultraBall ], drawn = [] }
                                , blue = { discarded = [], shuffled = [], drawn = [] }
                                }
                            )
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
                            applyGroupToBench "A" emptyBench group
                    in
                    bench.red
                        |> Expect.equalLists [ fanRotom, froakie ]
            ]
        ]



-- HELPERS


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
