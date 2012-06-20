module Aufgabe2Exercise2Test where

import HUnit
import Aufgabe2
import Control.OldException

funkProgAcceptanceTest1 = 
    TestCase (assertEqual "Factors" [(1, 13), (13, 1)] (faktorisiere 13))

funkProgAcceptanceTest2 = 
    TestCase (assertEqual "Factors" [(1, 1377), (9, 153), (17, 81), (81, 17), (153, 9), (1377, 1)] (faktorisiere 1377))

errorMessageForNumberNotInP = TestCase $ do
  handleJust errorCalls (\_ -> return ()) performCall where
    performCall = do
      evaluate ( faktorisiere 2 )
      assertFailure "Unzulaessig"

allTests = 
    TestList [
        TestLabel " 13 must have 2 factor-pairs." funkProgAcceptanceTest1,
        TestLabel " 1377 must have 6 factor-pairs." funkProgAcceptanceTest2,
        TestLabel " Give a number not in P an error message must be shown." errorMessageForNumberNotInP
    ]

main = do runTestTT allTests