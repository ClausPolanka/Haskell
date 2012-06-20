module Aufgabe3Exercise1Test where

import HUnit
import Aufgabe3

emptyList = 
    TestCase (assertEqual "Matrix" [[1]] (anp1 []))

emptyComponentLists =
    TestCase (assertEqual "Matrix" [[], [], []] (anp1 [[], [], []]))

oneComponentListWithTwoElements = 
    TestCase (assertEqual "Matrix" [[5, 7]] (anp1 [[5, 7]]))

threeComponentListsWithOneElement = 
    TestCase (assertEqual "Matrix" [[5], [4], [1]] (anp1 [[5], [4], [1]]))

threeComponentLists_1Has1Element_1Has2Elements_1Has3Elements = 
    TestCase (assertEqual "Matrix" [[5, 0, 0], [4, 2, 0], [1, 3, 7]] (anp1 [[5], [4, 2], [1, 3, 7]]))

funkProgAcceptanceTest1 = 
    TestCase (assertEqual "Matrix" [[1, 2, 3, 0, 0], 
                                    [1, 2, 0, 0, 0], 
                                    [1, 2, 3, 4, 5], 
                                    [1, 0, 0, 0, 0]] (anp1 [[1, 2, 3], [1, 2], [1, 2, 3, 4, 5], [1]]))

funkProgAcceptanceTest2 = 
    TestCase (assertEqual "Matrix" [[1, 2, 3], [4, 5, 6]] (anp1 [[1, 2, 3], [4, 5, 6]]))

allTests = 
    TestList [
        TestLabel 
        " For an empty list a component list containing 1 must be returned." 
        emptyList,

        TestLabel 
        " For an empty list containing empty component lists return exactly the same as Matrix." 
        emptyComponentLists,

        TestLabel 
        " For 1 component list return the same component list." 
        oneComponentListWithTwoElements,

        TestLabel
        " For 3 component lists all containing one element return the same lists."
        threeComponentListsWithOneElement,

        TestLabel
        "For 3 component lists 1 has 1, 1 has 2 and 1 has 3 elements add necessary 0s to all lists."
        threeComponentLists_1Has1Element_1Has2Elements_1Has3Elements,

        TestLabel
        "FunkProg Acceptance Test 1"
        funkProgAcceptanceTest1,

        TestLabel
        "FunkProg Acceptance Test 2"
        funkProgAcceptanceTest2
    ]

main = do runTestTT allTests