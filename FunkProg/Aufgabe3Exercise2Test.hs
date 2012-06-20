module Aufgabe3Exercise2Test where

import HUnit
import Aufgabe3
import Control.OldException

errorMessageForNegativeRowLength = TestCase $ do
  handleJust errorCalls (\_ -> return ()) performCall where
    performCall = do
      evaluate ( anp2 [] (-1) 1 0 )
      assertFailure "unzulaessig"

errorMessageForNegativeColumnLength = TestCase $ do
  handleJust errorCalls (\_ -> return ()) performCall where
    performCall = do
      evaluate ( anp2 [] 1 (-1) 0 )
      assertFailure "unzulaessig"

emptyListWith2RowsAnd3ColumnsFilledUpWithZeros = 
    TestCase (assertEqual "Matrix" [[0, 0, 0], 
                                    [0, 0, 0]] (anp2 [] 2 3 0))

emptyListWith2RowsAnd2ColumnsFilledUpWith5s = 
    TestCase (assertEqual "Matrix" [[5], 
                                    [5]] (anp2 [] 2 1 5))

emptyListOneRow = 
    TestCase (assertEqual "Matrix" [] (anp2 [] 1 0 0))

emptyListOneColumn = 
    TestCase (assertEqual "Matrix" [] (anp2 [] 0 1 0))

emptyListZeroRowsAndColumns = 
    TestCase (assertEqual "Matrix" [] (anp2 [] 0 0 1))

matrix = [[1, 3, 4],
          [2, 7, 8],
          [8, 10, 7]]
componentListsMatchesExactlyRequirements = 
    TestCase (assertEqual "Matrix" matrix (anp2 matrix 3 3 1))

componentListsCroppedToGivenNrOfColumns = 
    TestCase (assertEqual "Matrix" [[1, 3],
                                    [2, 7],
                                    [8, 10]] (anp2 matrix 3 2 1))

componentListsCroppedOrExtendedToGivenRequirements = 
    TestCase (assertEqual "Matrix" [[1, 1],
                                    [2, 7],
                                    [8, 1]] (anp2 [[1], [2, 7, 8], [8]] 3 2 1))

rowsCroppedToGivenRequirements = 
    TestCase (assertEqual "Matrix" [[1, 1],
                                    [2, 7]] (anp2 [[1], [2, 7, 8], [8]] 2 2 1))

rowsExtendedToGivenRequirements = 
    TestCase (assertEqual "Matrix" [[1, 1],
                                    [2, 7],
                                    [1, 1]] (anp2 [[1], [2, 7, 8]] 3 2 1))

funkProgAcceptanceTest1 = 
    TestCase (assertEqual "Matrix" [[0,0,0],[0,0,0]] (anp2 [] 2 3 0))

funkProgAcceptanceTest2 = 
    TestCase (assertEqual "Matrix" [[1, 2, 3], 
                                    [1, 2, 9], 
                                    [1, 2, 3]] (anp2 [[1, 2, 3], [1, 2], [1, 2, 3, 4, 5], [1]] 3 3 9))

funkProgAcceptanceTest3 = 
    TestCase (assertEqual "Matrix" [[1, 2, 3, (-1)], 
                                    [4, 5, 6, (-1)], 
                                    [(-1), (-1), (-1), (-1)]] (anp2 [[1, 2, 3], [4, 5, 6]] 3 4 (-1)))

allTests = 
    TestList [
        TestLabel 
        " For a negative row length an error message must be shown." 
        errorMessageForNegativeRowLength,

        TestLabel 
        " For a negative column length an error message must be shown." 
        errorMessageForNegativeColumnLength,

        TestLabel 
        " Empty list with 2 rows and 3 columns filled up with zeros." 
        emptyListWith2RowsAnd3ColumnsFilledUpWithZeros,

        TestLabel 
        " Empty list with 2 rows and 2 columns filled up with 5s." 
        emptyListWith2RowsAnd2ColumnsFilledUpWith5s,

        TestLabel 
        " Empty list with 1 row and 0 columns must return empty list." 
        emptyListOneRow,

        TestLabel 
        " Empty list with 0 rows and 1 column must return empty list." 
        emptyListOneColumn,
        
        TestLabel 
        " Empty list with 0 rows and 0 columns must return empty list." 
        emptyListZeroRowsAndColumns,

        TestLabel 
        " Component lists matches exactly given requirements." 
        componentListsMatchesExactlyRequirements,

        TestLabel 
        " Component lists which have more columns than required are cropped." 
        componentListsCroppedToGivenNrOfColumns,

        TestLabel 
        " Component lists which are cropped or extended to match given requirements." 
        componentListsCroppedOrExtendedToGivenRequirements,

        TestLabel 
        " Rows of component lists are cropped to match given requirements." 
        rowsCroppedToGivenRequirements,

        TestLabel 
        " Rows of component lists are extended to match given requirements." 
        rowsExtendedToGivenRequirements,

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