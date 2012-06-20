module Aufgabe2Exercise1Test where

import HUnit
import Aufgabe2

assureThatZeroIsNotAPrimal = 
    TestCase (assertEqual "Not a primal" False (istPrimal 0))

assureThatOneIsNotAPrimal = 
    TestCase (assertEqual "Not a primal" False (istPrimal 1))

assureThat38IsNotAPrimal = 
    TestCase (assertEqual "Not a primal" False (istPrimal 38))

assureThat173IsNotAPrimal = 
    TestCase (assertEqual "Not a primal" False (istPrimal 173))

funkProgAcceptanceTest1 = 
    TestCase (assertEqual "Not a primal" False (istPrimal (-1)))

funkProgAcceptanceTest2 = 
    TestCase (assertEqual "Not a primal" False (istPrimal 1377))

funkProgAcceptanceTest3 = 
    TestCase (assertEqual "Not a primal" False (istPrimal 41))

allTests = 
    TestList [
        TestLabel " 0 is not a primal" assureThatZeroIsNotAPrimal,
        TestLabel " 2 is not a primal" assureThatOneIsNotAPrimal,
        TestLabel " 38 is not a primal" assureThat38IsNotAPrimal,
        TestLabel " 173 is not a primal" assureThat173IsNotAPrimal,
        TestLabel " -1 is not a primal" funkProgAcceptanceTest1,
        TestLabel " -1 is not a primal" funkProgAcceptanceTest2,
        TestLabel " -1 is not a primal" funkProgAcceptanceTest3
    ]

main = do runTestTT allTests