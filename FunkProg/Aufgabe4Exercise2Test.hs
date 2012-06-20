module Aufgabe4Exercise2Test where

import HUnit
import Aufgabe4

productOfTwoEqualOneTimesOneMatrices = 
    TestCase (assertEqual "Matrices-Product" [[1]] (mm [[1]] [[1]] (1, 1, 1, 2)))

productOfTwoUnequalOneTimesOneMatrices = 
    TestCase (assertEqual "Matrices-Product" [[2]] (mm [[2]] [[1]] (1, 1, 1, 2)))

funkProgAcceptanceTest1 =
    TestCase (assertEqual "Matrices-Product" [[22, 28], [49, 64]] (mm [[1, 2, 3], [4, 5, 6]] [[1, 2], [3, 4], [5, 6]] (2, 3, 2, 9)))

funkProgAcceptanceTest2 =
    TestCase (assertEqual "Matrices-Product" [[ 7, 10, 3, 3], 
                                              [19, 28, 9, 9], 
                                              [ 4,  6, 2, 2]] (mm [[1, 2, 3], [4, 5, 6]] [[1, 2], [3, 4], [5, 6]] (3, 2, 4, 1)))

funkProgAcceptanceTest3 =
    TestCase (assertEqual "Matrices-Product" [[22]] (mm [[1, 2, 3], [4, 5, 6]] [[1, 2], [3, 4], [5, 6]] (1, 3, 1, 9)))

allTests = 
    TestList [
        TestLabel 
        " Given two equal 1x1 matrices multiply them." 
        productOfTwoEqualOneTimesOneMatrices,

        TestLabel 
        " Given two unequal 1x1 matrices multiply them." 
        productOfTwoUnequalOneTimesOneMatrices,

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