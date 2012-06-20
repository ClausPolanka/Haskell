module Aufgabe5Exercise5Test where

import HUnit
import Aufgabe5

unixwcwOneWord =
    TestCase (assertEqual "Nr of words" 1 (unixwcw "FooBar"))

unixwcwTwoWords =
    TestCase (assertEqual "Nr of words" 2 (unixwcw "FooBar FooBar"))

unixwcwTwoWordsOnTwoLines =
    TestCase (assertEqual "Nr of words" 2 (unixwcw "FooBar\nFooBar"))

unixwcwFourWordsOnTwoLines =
    TestCase (assertEqual "Nr of words" 4 (unixwcw "FooBar Claus\nFooBar Polanka"))

unixwcOneWordInOneLine = 
    TestCase (assertEqual "Lines, words, characters" (1, 1, 6) (unixwc "FooBar"))

unixwcTwoWordsInOneLine = 
    TestCase (assertEqual "Lines, words, characters" (1, 2, 7) (unixwc "Foo Bar"))

unixwcTwoWordsOnTwoLines = 
    TestCase (assertEqual "Lines, words, characters" (2, 2, 6) (unixwc "Foo\nBar"))

unixwcFourWordsOnTwoLines = 
    TestCase (assertEqual "Lines, words, characters" (2, 4, 14) (unixwc "Foo Bar\nBar Foo"))

allTests = 
    TestList [
        TestLabel 
        " Given one word, word count must be one." 
        unixwcwOneWord,

        TestLabel 
        " Given two words, word count must be two." 
        unixwcwTwoWords,

        TestLabel 
        " Given two words on two lines, word count must be two." 
        unixwcwTwoWordsOnTwoLines,

        TestLabel 
        " Given four words on two lines, word count must be four." 
        unixwcwFourWordsOnTwoLines,

        TestLabel 
        " Given two words in one line, wc returns 1 (line) 2 (word) 7 (characters)." 
        unixwcTwoWordsInOneLine,

        TestLabel 
        " Given two words on two lines, wc returns 2 (line) 2 (word) 6 (characters)." 
        unixwcTwoWordsInOneLine,

        TestLabel 
        " Given four words on two lines, wc returns 2 (line) 2 (word) 14 (characters)." 
        unixwcFourWordsOnTwoLines
    ]

main = do runTestTT allTests