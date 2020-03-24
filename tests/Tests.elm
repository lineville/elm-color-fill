module Tests exposing (..)

import Test exposing (..)
import Main exposing (..)
import Expect


-- Check out https://package.elm-lang.org/packages/elm-explorations/test/latest to learn more about testing in Elm!


all : Test
all =
    describe "A Test Suite"
        [ test "Addition" <|
            \_ ->
                Expect.equal 10 (3 + 7)
        , test "String.left" <|
            \_ ->
                Expect.equal "a" (String.left 1 "abcdefg")
        , test "These should not be equal" <|
            \_ ->
                Expect.notEqual 25 26
        , test "This test should pass" <|
            \_ ->
                Expect.equal 500 (5 * 10 * 10)
        ]

colorFillTests : Test
colorFillTests = 
    describe "Color Fill"
        [ test "getEmoji should get an Alien" <|
            \_ ->
                Expect.equal (getEmoji (0, colors)) Alien

        , test "getEmoji should get a Unicorn" <|
            \_ ->
                Expect.equal (getEmoji (1, colors)) Unicorn
        
        , test "generateList should create a new list" <|
            \_ ->
                Expect.equalLists (generateList 10 Unicorn) [Unicorn, Unicorn, Unicorn, Unicorn, Unicorn, Unicorn, Unicorn, Unicorn, Unicorn, Unicorn]

        , test "generateGrid should create a new grid" <|
            \_ ->
                Expect.equalLists (generateGrid 10) [
                    [Black, Black, Black, Black, Black, Black, Black, Black, Black,Black]
                ,   [Purple, Purple, Purple, Purple, Purple, Purple, Purple, Purple, Purple,Purple]
                ,   [Yellow, Yellow, Yellow, Yellow, Yellow, Yellow, Yellow, Yellow, Yellow,Yellow]
                ,   [Blue, Blue, Blue, Blue, Blue, Blue, Blue, Blue, Blue,Blue]
                ,   [Red, Red, Red, Red, Red, Red, Red, Red, Red,Red]
                ,   [Clover, Clover, Clover, Clover, Clover, Clover, Clover, Clover, Clover,Clover]
                ,   [Tiger, Tiger, Tiger, Tiger, Tiger, Tiger, Tiger, Tiger, Tiger,Tiger]
                ,   [Wave, Wave, Wave, Wave, Wave, Wave, Wave, Wave, Wave,Wave]
                ,   [Unicorn, Unicorn, Unicorn, Unicorn, Unicorn, Unicorn, Unicorn, Unicorn, Unicorn,Unicorn]
                ,   [Alien , Alien, Alien, Alien, Alien, Alien, Alien, Alien, Alien, Alien]
                ]


        ]

