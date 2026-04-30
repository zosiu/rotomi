module MainTest exposing (suite)

import Expect
import Main exposing (Model, Msg(..), init, update)
import Test exposing (Test, describe, test)


suite : Test
suite =
    describe "Main"
        [ describe "update"
            [ test "Increment increases count by 1" <|
                \_ ->
                    init
                        |> update Increment
                        |> .count
                        |> Expect.equal 1
            , test "Decrement decreases count by 1" <|
                \_ ->
                    init
                        |> update Decrement
                        |> .count
                        |> Expect.equal -1
            , test "Reset returns to initial state" <|
                \_ ->
                    { init | count = 42 }
                        |> update Reset
                        |> Expect.equal init
            , test "multiple updates compose correctly" <|
                \_ ->
                    init
                        |> update Increment
                        |> update Increment
                        |> update Increment
                        |> update Decrement
                        |> .count
                        |> Expect.equal 2
            ]
        ]
