module Aufgabe4Exercise4Test where

import HUnit
import Aufgabe4

funkProgAcceptanceTest1 = 
    TestCase (assertEqual "Matrix Power" [[1, 0], [0, 1]] (mp [[1, 2], [3, 4]] (2, 9) 0))

funkProgAcceptanceTest2 = 
    TestCase (assertEqual "Matrix Power" [[1, 2], [3, 4]] (mp [[1, 2], [3, 4]] (2, 9) 1))

funkProgAcceptanceTest3 = 
    TestCase (assertEqual "Matrix Power" [[7, 10], [15, 22]] (mp [[1, 2], [3, 4]] (2, 9) 2))

funkProgAcceptanceTest4 = 
    TestCase (assertEqual "Matrix Power" [[37, 54], [81, 118]] (mp [[1, 2], [3, 4]] (2, 9) 3))

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
        funkProgAcceptanceTest3,

        TestLabel 
        " FunkProg acceptance test 4." 
        funkProgAcceptanceTest4
    ]

main = do runTestTT allTests