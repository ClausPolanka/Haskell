module Aufgabe4Exercise3Test where

import HUnit
import Aufgabe4

funkProgAcceptanceTest1 = 
    TestCase (assertEqual "Matrices Sum" [[2, 4, 6], [8, 10, 12]] (ms [[1, 2, 3], [4, 5, 6]] [[1, 2, 3], [4, 5, 6]] (2, 3, 9)))

funkProgAcceptanceTest2 = 
    TestCase (assertEqual "Matrices Sum" [[2, 4], [8, 10], [2, 2]] (ms [[1, 2, 3], [4, 5, 6]] [[1, 2, 3], [4, 5, 6]] (3, 2, 1)))

funkProgAcceptanceTest3 = 
    TestCase (assertEqual "Matrices Sum" [[7, 7]] (ms [[1, 2, 3], [4, 5, 6]] [[6, 5, 4], [3, 2, 1]] (1, 2, 3)))

allTests = 
    TestList [
        TestLabel 
        " FunkProg acceptance test 1." 
        funkProgAcceptanceTest1,

		TestLabel 
        " FunkProg acceptance test 2." 
        funkProgAcceptanceTest2,

        TestLabel 
        " FunkProg acceptance test 3." 
        funkProgAcceptanceTest3
    ]

main = do runTestTT allTests