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
        [ test "getEmoji should get a dog" <|
            \_ ->
                Expect.equal (getEmoji (0, colors)) "🦮"

        , test "getEmoji should get an otter" <|
            \_ ->
                Expect.equal (getEmoji (1, colors)) "🦦"
        
        , test "generateList should create a new list" <|
            \_ ->
                Expect.equalLists (generateList (10, 10)) ["🔶", "🔶", "🔶", "🔶", "🔶", "🔶", "🔶", "🔶", "🔶", "🔶"]

        , test "generateGrid should create a new grid" <|
            \_ ->
                Expect.equalLists (generateGrid 10) [
                    ["🔶", "🔶", "🔶", "🔶", "🔶", "🔶", "🔶", "🔶", "🔶","🔶"]
                ,   ["⬛️","⬛️","⬛️","⬛️","⬛️","⬛️","⬛️","⬛️","⬛️","⬛️"]
                ,   ["💜","💜","💜","💜","💜","💜","💜","💜","💜","💜"]
                ,   ["💛","💛","💛","💛","💛","💛","💛","💛","💛","💛"]
                ,   ["💙","💙","💙","💙","💙","💙","💙","💙","💙","💙"]
                ,   ["🔴","🔴","🔴","🔴","🔴","🔴","🔴","🔴","🔴","🔴"]
                ,   ["☘️","☘️","☘️","☘️","☘️","☘️","☘️","☘️","☘️","☘️"]
                ,   ["🪓","🪓","🪓","🪓","🪓","🪓","🪓","🪓","🪓","🪓"]
                ,   ["🪐","🪐","🪐","🪐","🪐","🪐","🪐","🪐","🪐","🪐"]
                ,   ["🧇","🧇","🧇","🧇","🧇","🧇","🧇","🧇","🧇","🧇"]]

        ]