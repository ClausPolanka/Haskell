module Aufgabe5Exercise1Test where

import HUnit
import Aufgabe5

linesWith1Line = TestCase (assertEqual "Lines" 1 (length . lines $ "Claus"))
linesWith2Line = TestCase (assertEqual "Lines" 2 (length . lines $ "Claus\nPolanka"))
wordsWith2Words = TestCase (assertEqual "Lines" 2 (length . words $ "Claus Polanka"))
wordsWith2WordsAnd2Lines = TestCase (assertEqual "Lines" 2 (length . words $ "Claus\nPolanka"))
wordsWith3WordsAnd2Lines = TestCase (assertEqual "Lines" 3 (length . words $ "Claus\nPolanka Babs"))

unlinesLines = 
    TestCase (assertEqual "Not equal" True (all (\x->(unlines.lines)x/=x) ctxlines))

lengthLowerThan2 = 
    TestCase (assertEqual "More than 2 examples" True ((((<)2).length) ctxlines))

anyStringLenghtLowerThan2 =
    TestCase (assertEqual "More than 2 lines" True (any (((<)2).length.lines) ctxlines))

unlinesLinesForCtxUnlines = 
    TestCase (assertEqual "Not equal" True (all (\x->(lines.unlines)x/=x) ctxunlines))

lengthLowerThan2ForCtxUnlines = 
    TestCase (assertEqual "More than 2 examples" True ((((<)2).length) ctxunlines))

anyStringLenghtLowerThan2ForCtxUnlines =
    TestCase (assertEqual "More than 2 characters" True (any (((<)2).length.unlines) ctxunlines))

unwordsWords = 
    TestCase (assertEqual "Not equal" True (all (\x->(unwords.words)x/=x) ctxwords))

moreThan3StringsForWords = 
    TestCase (assertEqual "More than 2 examples" True ((((<)2).length) ctxwords))

oneStringMustContainAtLeast3WordsForWords =
    TestCase (assertEqual "More than 2 words" True (any (((<)2).length.words) ctxwords))


allTests = 
    TestList [
        TestLabel 
        " Reading 1 word in 1 line, length of list must be 1." 
        linesWith1Line,
        
        TestLabel 
        " Reading 1 word in 2 lines, length of list must be 2." 
        linesWith2Line,
        
        TestLabel 
        " Reading 2 words, length of list must be 2." 
        wordsWith2Words,
        
        TestLabel 
        " Reading 3 words on two lines, length of list must be 2." 
        wordsWith2WordsAnd2Lines,
        
        TestLabel 
        " Reading 3 words on two lines, length of list must be 3." 
        wordsWith3WordsAnd2Lines,

        TestLabel 
        " After calling unlines and lines result ist not the same." 
        unlinesLines,
        
        TestLabel 
        " Length must be lower than 2." 
        lengthLowerThan2,
        
        TestLabel 
        " Any of all String-Lengths must be lower than 2." 
        anyStringLenghtLowerThan2,

        TestLabel 
        " After calling lines and unlines result ist not the same." 
        unlinesLinesForCtxUnlines,
        
        TestLabel 
        " Length must be lower than 2 for ctxunlines." 
        lengthLowerThan2ForCtxUnlines,
        
        TestLabel
        " Any of all String-Lengths must be lower than 2 for ctxunlines." 
        anyStringLenghtLowerThan2ForCtxUnlines,

        TestLabel " More than one white space will be ignored." 
        unwordsWords,

        TestLabel 
        " More than 3 string examples must be passed for words." 
        moreThan3StringsForWords,
        
        TestLabel 
        " One String must contain at least 3 words." 
        oneStringMustContainAtLeast3WordsForWords
    ]

main = do runTestTT allTests