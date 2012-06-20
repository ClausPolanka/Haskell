module Aufgabe4Exercise1Test where

import HUnit
import Aufgabe4
import Control.OldException

errorMessageForNegativeRowLength = TestCase $ do
  handleJust errorCalls (\_ -> return ()) performCall where
    performCall = do
      evaluate ( msk ([], (-1), 1, 1) 1 )
      assertFailure "unzulaessig"

errorMessageForNegativeColumnLength = TestCase $ do
  handleJust errorCalls (\_ -> return ()) performCall where
    performCall = do
      evaluate ( msk ([], 1, (-1), 1) 1 )
      assertFailure "unzulaessig"

matrixWithOneColumnAndOneRow = 
    TestCase (assertEqual "Matrix * Skalar" [[2]] (msk ([[1]], 1, 1, 1) 2))

funkProgAcceptanceTest1 = 
    TestCase (assertEqual "Matrix * Skalar" [[5, 10, 15], [20, 25, 30]] (msk ([[1, 2, 3], [4, 5, 6]], 2, 3, 9 ) 5))    

funkProgAcceptanceTest2 = 
    TestCase (assertEqual "Matrix * Skalar" [[5, 10, 15, 5], [20, 25, 30, 5], [5, 5, 5, 5]] (msk ([[1, 2, 3], [4, 5, 6]], 3, 4, 1 ) 5))    

funkProgAcceptanceTest3 = 
    TestCase (assertEqual "Matrix * Skalar" [[5, 10], [20, 25]] (msk ([[1, 2, 3], [4, 5, 6]], 2, 2, 9 ) 5))    

allTests = 
    TestList [
        TestLabel 
        " Given a negative row value an error message must be shown." 
        errorMessageForNegativeRowLength,

        TestLabel 
        " Given a negative column value an error message must be shown." 
        errorMessageForNegativeColumnLength,

        TestLabel 
        " Given a matrix with one row and one column it must be multyplied with the skalar." 
        matrixWithOneColumnAndOneRow,

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