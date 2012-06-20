module Aufgabe5Exercise4Test where

import HUnit
import Aufgabe5

unixrevForTwoLines =
    TestCase (assertEqual "Reversed characters" "raBooF\nsualC" (unixrev "FooBar\nClaus"))

wordrevForTwoLines =
    TestCase (assertEqual "Reversed words" "Bar Foo\nPolanka Claus" (wordrev "Foo Bar\nClaus Polanka"))

allTests = 
    TestList [
        TestLabel 
        " Reversed all characters of all lines." 
        unixrevForTwoLines,
        TestLabel 
        " Reversed all words of all lines." 
        wordrevForTwoLines
    ]

main = do runTestTT allTests
