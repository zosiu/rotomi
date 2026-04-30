module Replay exposing
    ( MatchResult
    , Players
    , Replay
    , ReplayLine(..)
    , Section(..)
    , Turn
    , parse
    )


type alias Replay =
    { sections : List Section
    , players : Maybe Players
    }


type alias Players =
    { red : String
    , blue : String
    }


type Section
    = SetupSection (List ReplayLine)
    | TurnSection Turn (List ReplayLine)
    | CheckupSection (List ReplayLine)
    | ResultSection MatchResult


type alias MatchResult =
    { reason : String
    , winner : String
    }


type alias Turn =
    { number : Int
    , player : String
    }


type ReplayLine
    = TopLine String
    | DetailLine String
    | BulletLine String



-- PARSER


parse : String -> Replay
parse raw =
    let
        sections =
            raw
                |> String.lines
                |> List.map (String.replace "\u{000D}" "")
                |> List.foldl processLine emptyState
                |> finalizeSections
                |> extractResult
    in
    { sections = sections
    , players = identifyPlayers sections
    }


type SectionKind
    = KindSetup
    | KindTurn Turn
    | KindCheckup


type alias ParseState =
    { sections : List Section
    , currentKind : Maybe SectionKind
    , currentLines : List ReplayLine
    , turnCount : Int
    }


emptyState : ParseState
emptyState =
    { sections = [], currentKind = Nothing, currentLines = [], turnCount = 0 }


processLine : String -> ParseState -> ParseState
processLine line state =
    if line == "Setup" then
        startSection KindSetup state

    else if line == "Pokémon Checkup" then
        startSection KindCheckup state

    else
        case parseTurnHeader line of
            Just { player, maybeNumber } ->
                let
                    newCount =
                        state.turnCount + 1

                    turn =
                        { number = Maybe.withDefault newCount maybeNumber
                        , player = player
                        }
                in
                { sections = commitSection state
                , currentKind = Just (KindTurn turn)
                , currentLines = []
                , turnCount = newCount
                }

            Nothing ->
                if String.isEmpty (String.trim line) then
                    state

                else
                    { state | currentLines = classifyLine line :: state.currentLines }


startSection : SectionKind -> ParseState -> ParseState
startSection kind state =
    { sections = commitSection state
    , currentKind = Just kind
    , currentLines = []
    , turnCount = state.turnCount
    }


commitSection : ParseState -> List Section
commitSection state =
    case state.currentKind of
        Nothing ->
            state.sections

        Just kind ->
            state.sections ++ [ buildSection kind (List.reverse state.currentLines) ]


finalizeSections : ParseState -> List Section
finalizeSections state =
    case state.currentKind of
        Nothing ->
            state.sections

        Just kind ->
            state.sections ++ [ buildSection kind (List.reverse state.currentLines) ]


-- Handles both "Turn # N - Player's Turn" and "Player's Turn"
parseTurnHeader : String -> Maybe { player : String, maybeNumber : Maybe Int }
parseTurnHeader line =
    if String.startsWith "Turn # " line then
        case String.split " - " line of
            [ turnPart, playerPart ] ->
                turnPart
                    |> String.dropLeft 7
                    |> String.toInt
                    |> Maybe.map
                        (\n ->
                            { player = String.dropRight 7 playerPart
                            , maybeNumber = Just n
                            }
                        )

            _ ->
                Nothing

    else if String.endsWith "'s Turn" line && not (String.contains "." line) then
        Just { player = String.dropRight 7 line, maybeNumber = Nothing }

    else
        Nothing


classifyLine : String -> ReplayLine
classifyLine line =
    if String.startsWith "   •" line then
        BulletLine (line |> String.dropLeft 4 |> String.trim)

    else if String.startsWith "- " line then
        DetailLine (String.dropLeft 2 line)

    else
        TopLine line


buildSection : SectionKind -> List ReplayLine -> Section
buildSection kind lines =
    case kind of
        KindSetup ->
            SetupSection lines

        KindTurn turn ->
            TurnSection turn lines

        KindCheckup ->
            CheckupSection lines



-- RESULT EXTRACTION


extractResult : List Section -> List Section
extractResult sections =
    case List.reverse sections of
        (TurnSection turn lines) :: rest ->
            case List.reverse lines of
                (TopLine text) :: otherLines ->
                    case parseMatchResult text of
                        Just result ->
                            List.reverse rest
                                ++ [ TurnSection turn (List.reverse otherLines)
                                   , ResultSection result
                                   ]

                        Nothing ->
                            sections

                _ ->
                    sections

        _ ->
            sections


parseMatchResult : String -> Maybe MatchResult
parseMatchResult text =
    if String.endsWith " wins." text then
        let
            withoutWins =
                String.dropRight 6 text

            words =
                String.words withoutWins
        in
        case List.reverse words of
            winner :: reasonWords ->
                if String.isEmpty winner then
                    Nothing

                else
                    Just
                        { reason = String.join " " (List.reverse reasonWords)
                        , winner = winner
                        }

            [] ->
                Nothing

    else
        Nothing



-- PLAYER IDENTIFICATION
-- Red player: the one whose "drew 7 cards for the opening hand." line
-- is followed (possibly via a detail line) by a bullet line revealing their hand.


identifyPlayers : List Section -> Maybe Players
identifyPlayers sections =
    case sections of
        (SetupSection lines) :: _ ->
            let
                drawSuffix =
                    " drew 7 cards for the opening hand."

                drewPlayers =
                    List.filterMap
                        (\line ->
                            case line of
                                TopLine text ->
                                    if String.endsWith drawSuffix text then
                                        Just (String.dropRight (String.length drawSuffix) text)

                                    else
                                        Nothing

                                _ ->
                                    Nothing
                        )
                        lines
            in
            case ( identifyRedPlayer drawSuffix lines, drewPlayers ) of
                ( Just red, p1 :: p2 :: _ ) ->
                    Just
                        { red = red
                        , blue =
                            if p1 == red then
                                p2

                            else
                                p1
                        }

                _ ->
                    Nothing

        _ ->
            Nothing


identifyRedPlayer : String -> List ReplayLine -> Maybe String
identifyRedPlayer drawSuffix lines =
    let
        suffixLen =
            String.length drawSuffix

        step line ( waitingFor, found ) =
            case found of
                Just _ ->
                    ( waitingFor, found )

                Nothing ->
                    case line of
                        TopLine text ->
                            if String.endsWith drawSuffix text then
                                ( Just (String.dropRight suffixLen text), Nothing )

                            else
                                ( Nothing, Nothing )

                        BulletLine _ ->
                            case waitingFor of
                                Just player ->
                                    ( Nothing, Just player )

                                Nothing ->
                                    ( Nothing, Nothing )

                        _ ->
                            ( waitingFor, Nothing )
    in
    lines
        |> List.foldl step ( Nothing, Nothing )
        |> Tuple.second
