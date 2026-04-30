port module ValidateFixtures exposing (main)

import Platform
import Replay exposing (MatchResult, Players, Section(..))


port done : { output : String, allOk : Bool, cardRefs : List { id : String, name : String } } -> Cmd msg


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

                    cardRefs =
                        files
                            |> List.concatMap (.content >> extractCardRefs)
                            |> uniqueRefsByIdSorted
                in
                ( (), done { output = output, allOk = allOk, cardRefs = cardRefs } )
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


-- CARD REFERENCE EXTRACTION


extractCardRefs : String -> List { id : String, name : String }
extractCardRefs content =
    content
        |> String.split "("
        |> List.drop 1
        |> List.concatMap
            (\s ->
                case String.split ")" s of
                    id :: rest ->
                        if isCardId id then
                            let
                                remainder =
                                    String.join ")" rest

                                ( name, _ ) =
                                    extractCardName remainder
                            in
                            [ { id = id, name = name } ]

                        else
                            []

                    [] ->
                        []
            )


isCardId : String -> Bool
isCardId s =
    not (String.isEmpty s)
        && String.contains "_" s
        && String.all (\c -> Char.isAlpha c || Char.isDigit c || c == '_' || c == '-') s


uniqueRefsByIdSorted : List { id : String, name : String } -> List { id : String, name : String }
uniqueRefsByIdSorted refs =
    refs
        |> List.sortBy .id
        |> List.foldr
            (\x acc ->
                case acc of
                    first :: _ ->
                        if x.id == first.id then
                            acc

                        else
                            x :: acc

                    [] ->
                        [ x ]
            )
            []


-- CARD NAME EXTRACTION (mirrors extractCardName / helpers in Main.elm)


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
                ( [ stripped ], hadPunct )

            else if isConnector word then
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


safeDigits : String -> Bool
safeDigits word =
    let
        digits =
            String.filter Char.isDigit word
    in
    String.isEmpty digits
        || (String.length digits == 1 && String.endsWith digits word)


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


isConnector : String -> Bool
isConnector word =
    word == "of" || word == "at"


isVersionToken : String -> Bool
isVersionToken word =
    case String.uncons word of
        Nothing ->
            False

        Just ( c, _ ) ->
            Char.isDigit c
                && String.contains "." word
                && String.all (\ch -> Char.isDigit ch || ch == '.') word


stripTerminalPunct : String -> ( String, Bool )
stripTerminalPunct word =
    if String.endsWith "," word || String.endsWith "." word then
        ( String.dropRight 1 word, True )

    else
        ( word, False )


trimTrailingColon : String -> String
trimTrailingColon s =
    if String.endsWith ":" s then
        String.dropRight 1 s

    else
        s
