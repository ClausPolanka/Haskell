module Aufgabe3Exercise3Test where

import HUnit
import Aufgabe3
import Control.OldException

errorMessageForNegativeRowLength = TestCase $ do
  handleJust errorCalls (\_ -> return ()) performCall where
    performCall = do
      evaluate ( transp [[1]] (-1) 5 5 )
      assertFailure "unzulaessig"

errorMessageForNegativeColumnLength = TestCase $ do
  handleJust errorCalls (\_ -> return ()) performCall where
    performCall = do
      evaluate ( transp [[1]] 5 (-1) 5 )
      assertFailure "unzulaessig"

funkProgAcceptanceTest1 = 
    TestCase (assertEqual "Transponierte Matrix" [[0, 0], 
                                                  [0, 0], 
                                                  [0, 0]] (transp [] 2 3 0))

funkProgAcceptanceTest2 = 
    TestCase (assertEqual "Transponierte Matrix" [[1, 1, 1], 
                                                  [2, 2, 2], 
                                                  [3, 9, 3]] (transp [[1, 2, 3], 
                                                                      [1, 2], 
                                                                      [1, 2, 3, 4, 5], 
                                                                      [1]] 3 3 9))
funkProgAcceptanceTest3 = 
    TestCase (assertEqual "Transponierte Matrix" [[1, 4, (-1)], 
                                                  [2, 5, (-1)], 
                                                  [3, 6, (-1)], 
                                                  [(-1), (-1), (-1)]] (transp [[1, 2, 3],
                                                                               [4, 5, 6]] 3 4 (-1)))

allTests = 
    TestList [
        TestLabel 
        " For a negative row length an error message must be shown." 
        errorMessageForNegativeRowLength,

        TestLabel 
        " For a negative column length an error message must be shown." 
        errorMessageForNegativeColumnLength,

        TestLabel 
        " FunkProg Acceptance Test 1." 
        funkProgAcceptanceTest1,

        TestLabel 
        " FunkProg Acceptance Test 2." 
        funkProgAcceptanceTest2,

        TestLabel 
        " FunkProg Acceptance Test 3." 
        funkProgAcceptanceTest3
    ]

main = do runTestTT allTests