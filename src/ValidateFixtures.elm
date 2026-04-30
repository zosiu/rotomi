port module ValidateFixtures exposing (main)

import Platform
import Replay exposing (MatchResult, Players, Section(..))


port done : { output : String, allOk : Bool } -> Cmd msg


type alias FileInput =
    { name : String
    , content : String
    }


type alias FileResult =
    { name : String
    , setup : Int
    , turns : Int
    , checkups : Int
    , players : Maybe Players
    , result : Maybe MatchResult
    , issues : List String
    }


main : Platform.Program (List FileInput) () Never
main =
    Platform.worker
        { init =
            \files ->
                let
                    results =
                        List.map validateFile files

                    allOk =
                        List.all (.issues >> List.isEmpty) results

                    output =
                        String.join "\n" (List.map formatResult results)
                            ++ "\n\n"
                            ++ (if allOk then
                                    "All files OK"

                                else
                                    "Some files have issues"
                               )
                            ++ "\n"
                in
                ( (), done { output = output, allOk = allOk } )
        , update = \_ model -> ( model, Cmd.none )
        , subscriptions = \_ -> Sub.none
        }


validateFile : FileInput -> FileResult
validateFile { name, content } =
    let
        replay =
            Replay.parse content

        counts =
            countSections replay.sections

        emptyTurnIssues =
            List.filterMap
                (\section ->
                    case section of
                        TurnSection turn lines ->
                            if List.isEmpty lines then
                                Just ("turn " ++ String.fromInt turn.number ++ " (" ++ turn.player ++ ") has no lines")

                            else
                                Nothing

                        _ ->
                            Nothing
                )
                replay.sections

        matchResult =
            List.filterMap
                (\section ->
                    case section of
                        ResultSection r ->
                            Just r

                        _ ->
                            Nothing
                )
                replay.sections
                |> List.head

        issues =
            emptyTurnIssues
                ++ (if replay.players == Nothing then
                        [ "could not identify red/blue players" ]

                    else
                        []
                   )
                ++ (if matchResult == Nothing then
                        [ "could not find match result" ]

                    else
                        []
                   )
    in
    { name = name
    , setup = counts.setup
    , turns = counts.turns
    , checkups = counts.checkups
    , players = replay.players
    , result = matchResult
    , issues = issues
    }


countSections : List Section -> { setup : Int, turns : Int, checkups : Int }
countSections =
    List.foldl
        (\section acc ->
            case section of
                SetupSection _ ->
                    { acc | setup = acc.setup + 1 }

                TurnSection _ _ ->
                    { acc | turns = acc.turns + 1 }

                CheckupSection _ ->
                    { acc | checkups = acc.checkups + 1 }

                ResultSection _ ->
                    acc
        )
        { setup = 0, turns = 0, checkups = 0 }


formatResult : FileResult -> String
formatResult r =
    let
        ok =
            List.isEmpty r.issues

        tag =
            if ok then
                "OK  "

            else
                "FAIL"

        checkupStr =
            if r.checkups > 0 then
                "  checkups:" ++ String.fromInt r.checkups

            else
                ""

        playerStr =
            case r.players of
                Just p ->
                    "  red:" ++ p.red ++ "  blue:" ++ p.blue

                Nothing ->
                    "  players:unknown"

        resultStr =
            case r.result of
                Just res ->
                    "  winner:" ++ res.winner

                Nothing ->
                    "  result:unknown"

        mainLine =
            tag
                ++ "  "
                ++ r.name
                ++ "  setup:"
                ++ String.fromInt r.setup
                ++ "  turns:"
                ++ String.fromInt r.turns
                ++ checkupStr
                ++ playerStr
                ++ resultStr

        issueLines =
            List.map (\issue -> "       " ++ issue) r.issues
    in
    String.join "\n" (mainLine :: issueLines)
