module Main exposing (suite, testJoin, testMap, testSave)

import Expect exposing (Expectation)
import Fuzz exposing (Fuzzer, int, list, string)
import Test exposing (..)
import Update.Deep exposing (..)


testSave : Test
testSave =
    let
        state =
            5

        ( a, _, e ) =
            save state
    in
    describe "save"
        [ test "state" <|
            \_ -> Expect.equal a state
        , test "events" <|
            \_ -> Expect.equal [] e
        ]


testMap : Test
testMap =
    let
        state =
            5

        ( a, _, e ) =
            map (\x -> x + 1) (save state)
    in
    describe "map"
        [ test "this" <|
            \_ -> Expect.equal a (state + 1)
        ]


testJoin : Test
testJoin =
    let
        ( a, _, e ) =
            join (save (save 5))
    in
    describe "join"
        [ test "this" <|
            \_ -> Expect.equal a 5
        ]


suite : Test
suite =
    describe "Update Deep"
        [ testSave
        , testMap
        ]
