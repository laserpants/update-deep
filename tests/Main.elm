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
        , test "callbacks" <|
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
        [ test "increment" <|
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


testFold : Test
testFold =
    let
        someUpdate { callback1, callback2 } state =
          state
            |> applyCallback callback1
            |> andApplyCallback callback2

        setValueTo value state = save { state | value = value }

        ( a, _, _ ) = 
            fold (someUpdate { callback1 = setValueTo 1, callback2 = setValueTo 2 } { value = 0 })

    in
    describe "fold"
        [ test "execution order" <|
            \_ -> Expect.equal a.value 2 
        ]





testMap2 : Test
testMap2 =

    let
        a = save 5
        b = save 8
        (c, _, _) = map2 (\x y -> x + y) a b

    in
    describe "map2"
        [ test "this" <|
            \_ -> Expect.equal c 13
        ]



testAndMap : Test
testAndMap =

    let
        f x y z = x + y + z
        a = save 5
        b = save 6
        c = save 7
        (d, _, _) = 
            map f a
                |> andMap b
                |> andMap c

    in
    describe "andMap"
        [ test "this" <|
            \_ -> Expect.equal d 18
        ]



suite : Test
suite =
    describe "Update Deep"
        [ testSave
        , testMap
        , testJoin
        , testFold
        , testMap2
        , testAndMap
        ]
