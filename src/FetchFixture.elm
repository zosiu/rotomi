port module FetchFixture exposing (main)

import Platform
import Replay exposing (Players, Section(..))


port done : { log : String, ok : Bool, summary : String } -> Cmd msg


type alias Flags =
    { url : String
    , log : String
    }


main : Platform.Program Flags () Never
main =
    Platform.worker
        { init =
            \{ url, log } ->
                let
                    replay =
                        Replay.parse log

                    sectionCount =
                        List.length replay.sections

                    turnCount =
                        replay.sections
                            |> List.filter
                                (\s ->
                                    case s of
                                        TurnSection _ _ ->
                                            True

                                        _ ->
                                            False
                                )
                            |> List.length

                    playersLine =
                        case replay.players of
                            Just { red, blue } ->
                                red ++ " (red) vs " ++ blue ++ " (blue)"

                            Nothing ->
                                "Players: unknown"

                    hasResult =
                        replay.sections
                            |> List.any
                                (\s ->
                                    case s of
                                        ResultSection _ ->
                                            True

                                        _ ->
                                            False
                                )

                    issues =
                        [ if replay.players == Nothing then
                            Just "Could not identify players"

                          else
                            Nothing
                        , if sectionCount == 0 then
                            Just "No sections parsed"

                          else
                            Nothing
                        , if not hasResult then
                            Just "No match result found"

                          else
                            Nothing
                        ]
                            |> List.filterMap identity

                    ok =
                        List.isEmpty issues

                    summary =
                        String.join "\n"
                            ([ url
                             , playersLine
                             , String.fromInt turnCount ++ " turns, " ++ String.fromInt sectionCount ++ " sections"
                             ]
                                ++ (if ok then
                                        [ "OK" ]

                                    else
                                        List.map (\i -> "WARNING: " ++ i) issues
                                   )
                            )
                in
                ( (), done { log = log, ok = ok, summary = summary } )
        , update = \_ m -> ( m, Cmd.none )
        , subscriptions = \_ -> Sub.none
        }
