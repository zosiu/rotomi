module MainTest exposing (suite)

import Dict
import Expect
import Http
import Main exposing (CardData, CardPopup(..), Model(..), Msg(..), init, update)
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
                    init { replayUrl = "", sectionIndex = 0 }
                        |> Tuple.first
                        |> Expect.equal (EnteringUrl "")
            , test "whitespace flags start in EnteringUrl" <|
                \_ ->
                    init { replayUrl = "   ", sectionIndex = 0 }
                        |> Tuple.first
                        |> Expect.equal (EnteringUrl "")
            , test "url flags start in Loading" <|
                \_ ->
                    init { replayUrl = "https://example.com/replay.txt", sectionIndex = 0 }
                        |> Tuple.first
                        |> Expect.equal (Loading "https://example.com/replay.txt" 0)
            , test "url flags are trimmed" <|
                \_ ->
                    init { replayUrl = "  https://example.com/replay.txt  ", sectionIndex = 0 }
                        |> Tuple.first
                        |> Expect.equal (Loading "https://example.com/replay.txt" 0)
            , test "section index is preserved in Loading state" <|
                \_ ->
                    init { replayUrl = "https://example.com/replay.txt", sectionIndex = 3 }
                        |> Tuple.first
                        |> Expect.equal (Loading "https://example.com/replay.txt" 3)
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
                    update (GotReplay (Ok content)) (Loading url 0)
                        |> Tuple.first
                        |> Expect.equal (Loaded url (Replay.parse content) 0 Nothing Dict.empty)
            , test "NetworkError triggers proxy retry" <|
                \_ ->
                    update (GotReplay (Err Http.NetworkError)) (Loading "https://example.com" 0)
                        |> Tuple.first
                        |> Expect.equal (Retrying "https://example.com" 0)
            , test "404 transitions to Failed with friendly message" <|
                \_ ->
                    update (GotReplay (Err (Http.BadStatus 404))) (Loading "https://example.com" 0)
                        |> Tuple.first
                        |> Expect.equal (Failed "https://example.com" "No replay content found — check the URL")
            , test "other errors transition to Failed without retrying" <|
                \_ ->
                    update (GotReplay (Err Http.Timeout)) (Loading "https://example.com" 0)
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
                    update (GotReplay (Ok content)) (Retrying url 0)
                        |> Tuple.first
                        |> Expect.equal (Loaded url (Replay.parse content) 0 Nothing Dict.empty)
            , test "proxy 404 transitions to Failed with friendly message" <|
                \_ ->
                    update (GotReplay (Err (Http.BadStatus 404))) (Retrying "https://example.com" 0)
                        |> Tuple.first
                        |> Expect.equal (Failed "https://example.com" "No replay content found — check the URL")
            , test "proxy error transitions to Failed" <|
                \_ ->
                    update (GotReplay (Err Http.Timeout)) (Retrying "https://example.com" 0)
                        |> Tuple.first
                        |> Expect.equal (Failed "https://example.com" "Request timed out")
            , test "empty content transitions to Failed" <|
                \_ ->
                    update (GotReplay (Ok "")) (Loading "https://example.com" 0)
                        |> Tuple.first
                        |> Expect.equal (Failed "https://example.com" "No replay content found — check the URL")
            , test "unrecognised content from proxy transitions to Failed" <|
                \_ ->
                    update (GotReplay (Ok "<html>404 Not Found</html>")) (Retrying "https://example.com" 0)
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
                    update (GotReplay (Ok curlyContent)) (Loading url 0)
                        |> Tuple.first
                        |> Expect.equal (Loaded url (Replay.parse normalizedContent) 0 Nothing Dict.empty)
            , test "deep-linked section index is restored on load" <|
                \_ ->
                    let
                        url =
                            "https://example.com/replay.txt"

                        content =
                            "Setup\nSome setup.\nTurn # 1 - A's Turn\nA drew a card.\n"
                    in
                    update (GotReplay (Ok content)) (Loading url 1)
                        |> Tuple.first
                        |> Expect.equal (Loaded url (Replay.parse content) 1 Nothing Dict.empty)
            , test "out-of-range section index is clamped to last section" <|
                \_ ->
                    let
                        url =
                            "https://example.com/replay.txt"

                        content =
                            "Setup\nSome setup.\n"
                    in
                    update (GotReplay (Ok content)) (Loading url 99)
                        |> Tuple.first
                        |> Expect.equal (Loaded url (Replay.parse content) 0 Nothing Dict.empty)
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
                    update FirstSection (Loaded "url" replay 1 Nothing Dict.empty)
                        |> Tuple.first
                        |> Expect.equal (Loaded "url" replay 0 Nothing Dict.empty)
            , test "LastSection jumps to the last index" <|
                \_ ->
                    let
                        content =
                            "Setup\nSome setup.\nTurn # 1 - A's Turn\nA drew a card.\n"

                        replay =
                            Replay.parse content
                    in
                    update LastSection (Loaded "url" replay 0 Nothing Dict.empty)
                        |> Tuple.first
                        |> Expect.equal (Loaded "url" replay 1 Nothing Dict.empty)
            , test "NextSection increments the index" <|
                \_ ->
                    let
                        content =
                            "Setup\nSome setup.\nTurn # 1 - A's Turn\nA drew a card.\n"

                        replay =
                            Replay.parse content
                    in
                    update NextSection (Loaded "url" replay 0 Nothing Dict.empty)
                        |> Tuple.first
                        |> Expect.equal (Loaded "url" replay 1 Nothing Dict.empty)
            , test "NextSection does not go past the last section" <|
                \_ ->
                    let
                        content =
                            "Setup\nSome setup.\n"

                        replay =
                            Replay.parse content
                    in
                    update NextSection (Loaded "url" replay 0 Nothing Dict.empty)
                        |> Tuple.first
                        |> Expect.equal (Loaded "url" replay 0 Nothing Dict.empty)
            , test "PrevSection decrements the index" <|
                \_ ->
                    let
                        content =
                            "Setup\nSome setup.\nTurn # 1 - A's Turn\nA drew a card.\n"

                        replay =
                            Replay.parse content
                    in
                    update PrevSection (Loaded "url" replay 1 Nothing Dict.empty)
                        |> Tuple.first
                        |> Expect.equal (Loaded "url" replay 0 Nothing Dict.empty)
            , test "PrevSection does not go below zero" <|
                \_ ->
                    let
                        content =
                            "Setup\nSome setup.\n"

                        replay =
                            Replay.parse content
                    in
                    update PrevSection (Loaded "url" replay 0 Nothing Dict.empty)
                        |> Tuple.first
                        |> Expect.equal (Loaded "url" replay 0 Nothing Dict.empty)
            ]
        , describe "card popup"
            [ test "CardClicked sets FetchingCard popup for a valid id" <|
                \_ ->
                    let
                        replay =
                            Replay.parse "Setup\nSome setup.\n"
                    in
                    update (CardClicked "sv4_160_ph") (Loaded "url" replay 0 Nothing Dict.empty)
                        |> Tuple.first
                        |> Expect.equal (Loaded "url" replay 0 (Just (FetchingCard "sv4_160_ph")) Dict.empty)
            , test "CardClicked with unparseable id shows CardNotFound immediately" <|
                \_ ->
                    let
                        replay =
                            Replay.parse "Setup\nSome setup.\n"
                    in
                    update (CardClicked "nounderscore") (Loaded "url" replay 0 Nothing Dict.empty)
                        |> Tuple.first
                        |> Expect.equal (Loaded "url" replay 0 (Just (CardNotFound "nounderscore")) Dict.empty)
            , test "GotCardImage with valid JSON shows card image" <|
                \_ ->
                    let
                        replay =
                            Replay.parse "Setup\nSome setup.\n"

                        json =
                            "{\"id\":\"swsh1-1\",\"image\":\"https://assets.tcgdex.net/en/swsh/swsh1/1\"}"
                    in
                    update (GotCardImage "swsh1-1" (Ok json)) (Loaded "url" replay 0 Nothing Dict.empty)
                        |> Tuple.first
                        |> Expect.equal (Loaded "url" replay 0 (Just (ShowingCard "swsh1-1" (cardDataWithImage "https://assets.tcgdex.net/en/swsh/swsh1/1"))) (Dict.fromList [ ( "swsh1-1", cardDataWithImage "https://assets.tcgdex.net/en/swsh/swsh1/1" ) ]))
            , test "GotCardImage with invalid JSON shows CardNotFound" <|
                \_ ->
                    let
                        replay =
                            Replay.parse "Setup\nSome setup.\n"

                        emptyCardData =
                            { imageUrl = Nothing, attacks = [], abilities = [] }
                    in
                    update (GotCardImage "swsh1-1" (Ok "{\"error\":\"not found\"}")) (Loaded "url" replay 0 Nothing Dict.empty)
                        |> Tuple.first
                        |> Expect.equal (Loaded "url" replay 0 (Just (CardNotFound "swsh1-1")) (Dict.fromList [ ( "swsh1-1", emptyCardData ) ]))
            , test "GotCardImage with HTTP error shows CardNotFound" <|
                \_ ->
                    let
                        replay =
                            Replay.parse "Setup\nSome setup.\n"
                    in
                    update (GotCardImage "swsh1-1" (Err Http.NetworkError)) (Loaded "url" replay 0 Nothing Dict.empty)
                        |> Tuple.first
                        |> Expect.equal (Loaded "url" replay 0 (Just (CardNotFound "swsh1-1")) Dict.empty)
            , test "CloseCard removes the popup" <|
                \_ ->
                    let
                        replay =
                            Replay.parse "Setup\nSome setup.\n"
                    in
                    update CloseCard (Loaded "url" replay 0 (Just (CardNotFound "swsh1-1")) Dict.empty)
                        |> Tuple.first
                        |> Expect.equal (Loaded "url" replay 0 Nothing Dict.empty)
            , test "MoveClicked on cache hit shows ShowingMove" <|
                \_ ->
                    let
                        replay = Replay.parse "Setup\nSome setup.\n"
                        ability = { abilityType = "Ability", name = "Recon Directive", effect = "Once during your turn..." }
                        cardData = { imageUrl = Just "https://assets.tcgdex.net/en/sv/sv08.5/072", attacks = [], abilities = [ ability ] }
                        cache = Dict.fromList [ ( "sv8-5_72_sph", cardData ) ]
                    in
                    update (MoveClicked "sv8-5_72_sph" "Recon Directive") (Loaded "url" replay 0 Nothing cache)
                        |> Tuple.first
                        |> Expect.equal (Loaded "url" replay 0 (Just (ShowingMove cardData "Recon Directive")) cache)
            , test "MoveClicked on cache miss shows FetchingMove" <|
                \_ ->
                    let
                        replay = Replay.parse "Setup\nSome setup.\n"
                    in
                    update (MoveClicked "sv04_160" "Tackle") (Loaded "url" replay 0 Nothing Dict.empty)
                        |> Tuple.first
                        |> Expect.equal (Loaded "url" replay 0 (Just (FetchingMove "sv04_160" "Tackle")) Dict.empty)
            , test "GotCardImage when FetchingMove resolves to ShowingMove" <|
                \_ ->
                    let
                        replay = Replay.parse "Setup\nSome setup.\n"
                        json = "{\"image\":\"https://assets.tcgdex.net/en/sv/sv04/160\",\"attacks\":[{\"name\":\"Tackle\",\"cost\":[\"Colorless\"],\"damage\":10}],\"abilities\":[]}"
                        expectedData = { imageUrl = Just "https://assets.tcgdex.net/en/sv/sv04/160", attacks = [ { name = "Tackle", cost = [ "Colorless" ], damage = "10", effect = "" } ], abilities = [] }
                    in
                    update (GotCardImage "sv04_160" (Ok json)) (Loaded "url" replay 0 (Just (FetchingMove "sv04_160" "Tackle")) Dict.empty)
                        |> Tuple.first
                        |> Expect.equal (Loaded "url" replay 0 (Just (ShowingMove expectedData "Tackle")) (Dict.fromList [ ( "sv04_160", expectedData ) ]))
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
                    update (GotCardImage "sv04_001" (Ok json)) (Loaded "url" replay 0 Nothing Dict.empty)
                        |> Tuple.first
                        |> Expect.equal (Loaded "url" replay 0 (Just (ShowingCard "sv04_001" expectedData)) (Dict.fromList [ ( "sv04_001", expectedData ) ]))
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
                    update (CardClicked "sv04_160") (Loaded "url" replay 0 Nothing cache)
                        |> Tuple.first
                        |> Expect.equal (Loaded "url" replay 0 (Just (ShowingCard "sv04_160" (cardDataWithImage imageUrl))) cache)
            , test "CardClicked on cache miss shows FetchingCard" <|
                \_ ->
                    let
                        replay =
                            Replay.parse "Setup\nSome setup.\n"
                    in
                    update (CardClicked "sv04_160") (Loaded "url" replay 0 Nothing Dict.empty)
                        |> Tuple.first
                        |> Expect.equal (Loaded "url" replay 0 (Just (FetchingCard "sv04_160")) Dict.empty)
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
                    update NextSection (Loaded "url" replay 0 Nothing cache)
                        |> Tuple.first
                        |> Expect.equal (Loaded "url" replay 1 Nothing cache)
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
                    update (GotCardImage "swsh1-1" (Ok json)) (Loaded "url" replay 0 Nothing priorCache)
                        |> Tuple.first
                        |> Expect.equal (Loaded "url" replay 0 (Just (ShowingCard "swsh1-1" (cardDataWithImage newUrl))) expectedCache)
            , test "GotCardImage network error does not populate the cache" <|
                \_ ->
                    let
                        replay =
                            Replay.parse "Setup\nSome setup.\n"
                    in
                    update (GotCardImage "sv04_160" (Err Http.NetworkError)) (Loaded "url" replay 0 Nothing Dict.empty)
                        |> Tuple.first
                        |> Expect.equal (Loaded "url" replay 0 (Just (CardNotFound "sv04_160")) Dict.empty)
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
                    update (CardClicked "sv04_160") (Loaded "url" replay 0 Nothing cache)
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
