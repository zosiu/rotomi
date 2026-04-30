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
                    init ""
                        |> Tuple.first
                        |> Expect.equal (EnteringUrl "")
            , test "whitespace flags start in EnteringUrl" <|
                \_ ->
                    init "   "
                        |> Tuple.first
                        |> Expect.equal (EnteringUrl "")
            , test "url flags start in Loading" <|
                \_ ->
                    init "https://example.com/replay.txt"
                        |> Tuple.first
                        |> Expect.equal (Loading "https://example.com/replay.txt")
            , test "url flags are trimmed" <|
                \_ ->
                    init "  https://example.com/replay.txt  "
                        |> Tuple.first
                        |> Expect.equal (Loading "https://example.com/replay.txt")
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
                    update (GotReplay (Ok content)) (Loading url)
                        |> Tuple.first
                        |> Expect.equal (Loaded url (Replay.parse content))
            , test "Err result transitions to Failed" <|
                \_ ->
                    update (GotReplay (Err Http.NetworkError)) (Loading "https://example.com")
                        |> Tuple.first
                        |> Expect.equal (Failed "https://example.com" "Network error — check the URL and CORS headers")
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
