module MainTest exposing (suite)

import Expect
import Http
import Main exposing (Model(..), Msg(..), init, update)
import Replay exposing (ReplayLine(..), Section(..))
import Test exposing (Test, describe, test)


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
                        |> Expect.equal (Loaded url (Replay.parse content) 0)
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
                        |> Expect.equal (Loaded url (Replay.parse content) 0)
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
                        |> Expect.equal (Loaded url (Replay.parse content) 1)
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
                        |> Expect.equal (Loaded url (Replay.parse content) 0)
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
                    update FirstSection (Loaded "url" replay 1)
                        |> Tuple.first
                        |> Expect.equal (Loaded "url" replay 0)
            , test "LastSection jumps to the last index" <|
                \_ ->
                    let
                        content =
                            "Setup\nSome setup.\nTurn # 1 - A's Turn\nA drew a card.\n"

                        replay =
                            Replay.parse content
                    in
                    update LastSection (Loaded "url" replay 0)
                        |> Tuple.first
                        |> Expect.equal (Loaded "url" replay 1)
            , test "NextSection increments the index" <|
                \_ ->
                    let
                        content =
                            "Setup\nSome setup.\nTurn # 1 - A's Turn\nA drew a card.\n"

                        replay =
                            Replay.parse content
                    in
                    update NextSection (Loaded "url" replay 0)
                        |> Tuple.first
                        |> Expect.equal (Loaded "url" replay 1)
            , test "NextSection does not go past the last section" <|
                \_ ->
                    let
                        content =
                            "Setup\nSome setup.\n"

                        replay =
                            Replay.parse content
                    in
                    update NextSection (Loaded "url" replay 0)
                        |> Tuple.first
                        |> Expect.equal (Loaded "url" replay 0)
            , test "PrevSection decrements the index" <|
                \_ ->
                    let
                        content =
                            "Setup\nSome setup.\nTurn # 1 - A's Turn\nA drew a card.\n"

                        replay =
                            Replay.parse content
                    in
                    update PrevSection (Loaded "url" replay 1)
                        |> Tuple.first
                        |> Expect.equal (Loaded "url" replay 0)
            , test "PrevSection does not go below zero" <|
                \_ ->
                    let
                        content =
                            "Setup\nSome setup.\n"

                        replay =
                            Replay.parse content
                    in
                    update PrevSection (Loaded "url" replay 0)
                        |> Tuple.first
                        |> Expect.equal (Loaded "url" replay 0)
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
