module Main where

import Aufgabe5
import Test.HUnit
import Control.Monad

isValidTest1 = TestCase (assertEqual "for isValid (0, [])"
  True (isValid (0, [])))

isValidTest2 = TestCase (assertEqual "for isValid (100, [])"
  True (isValid (100, [])))

-- Negative maximale knoten
isValidTest3 = TestCase (assertEqual "for isValid (-1, [])"
  False (isValid (-1, [])))

isValidTest4 = TestCase (assertEqual "for isValid (0, [(0, 0, 1)])"
  False (isValid (0, [(0, 0, 1)])))

isValidTest5 = TestCase (assertEqual "for isValid (1, [(1, 0, 1)])"
  True (isValid (1, [(1, 0, 1)])))

-- Graph hat nur Knoten #0
isValidTest6 = TestCase (assertEqual "for isValid (0, [(1, 0, 1)])"
  False (isValid (0, [(1, 0, 1)])))

-- Graph hat nur Knoten #0
isValidTest7 = TestCase (assertEqual "for isValid (0, [(0, 1, 1)])"
  False (isValid (0, [(0, 1, 1)])))

-- Kosten muessen positiv sein
isValidTest8 = TestCase (assertEqual "for isValid (0, [(0, 0, 0)])"
  False (isValid (1, [(0, 0, 0)])))

-- Kanten muessen unique sein
isValidTest9 = TestCase (assertEqual "for isValid (0, [(0, 0, 1), (0, 0, 2)])"
  False (isValid (2, [(0, 0, 1), (0, 0, 2)])))

isValidTest10 = TestCase (assertEqual "for isValid (1, [(0, 1, 1), (1, 0, 2)])"
  True (isValid (2, [(0, 1, 1), (1, 0, 2)])))

isValidTest11 = TestCase (assertEqual "for isValid (2, [(0, 1, 1), (1, 0, 2))"
  True (isValid (2, [(0, 1, 1), (1, 0, 2)])))

isValidTest12 = TestCase (assertEqual "for isValid (4,[(1,2,500), (0,3,100), (0,2,200)])"
  True (isValid (4,[(1,2,500), (0,3,100), (0,2,200)])))

isValidTests = TestList [isValidTest1, isValidTest2, isValidTest3, isValidTest4, 
    isValidTest5, isValidTest6, isValidTest7, isValidTest8,
    isValidTest9, isValidTest10, isValidTest11]

-----------------------------------------------------------------------------

-- konvTest1 = TestCase (assertEqual "for LOOK_AT_TESTCASE_CODE#1"
  -- (ELg 4 [(0,200,2),(0,100,3),(1,500,2)])
  -- (am2el (el2am (al2el (am2al (al2am (el2al (inp2el (4, [(1,2,500), (0,3,100), (0,2,200)])))))))))

-- konvTest2 = TestCase (assertEqual "for LOOK_AT_TESTCASE_CODE#2"
  -- (ELg 40 [(x, 200, y) | x <- [0 .. 19], y <- [0 .. 19]])
  -- (am2el (el2am (al2el (am2al (al2am (el2al (inp2el (40, [(x, y, 200) | x <- [0 .. 19], y <- [0 .. 19]])))))))))

-- konvTests = TestList [konvTest1]--, konvTest2]

-----------------------------------------------------------------------------

-- isNeighbourOfTest1 = TestCase (assertEqual "for isNeighbourOf (ELg 0 []) 0 0"
  -- False (isNeighbourOf (ELg 0 []) 0 0))

-- isNeighbourOfTest2 = TestCase (assertEqual "for isNeighbourOf (ELg 0 []) 4 10"
  -- False (isNeighbourOf (ELg 0 []) 4 10))

-- isNeighbourOfTest3 = TestCase (assertEqual "for isNeighbourOf (ELg 0 [(0, 1, 0)]) 0 0"
  -- True (isNeighbourOf (ELg 0 [(0, 1, 0)]) 0 0))

-- isNeighbourOfTest4 = TestCase (assertEqual "for isNeighbourOf (ELg 1 [(0, 1, 1)]) 0 1"
  -- True (isNeighbourOf (ELg 1 [(0, 1, 1)]) 0 1))

-- isNeighbourOfTest5 = TestCase (assertEqual "for isNeighbourOf (ELg 1 [(1, 1, 0)]) 0 1"
  -- False (isNeighbourOf (ELg 1 [(1, 1, 0)]) 0 1))

-- allNeighboursOfTest1 = TestCase (assertEqual "for allNeighboursOf (ELg 0 []) 0"
  -- [] (allNeighboursOf (ELg 0 []) 0))

-- allNeighboursOfTest2 = TestCase (assertEqual "for allNeighboursOf (ELg 0 []) 10"
  -- [] (allNeighboursOf (ELg 0 []) 10))

-- allNeighboursOfTest3 = TestCase (assertEqual "for allNeighboursOf (ELg 0 [(0, 1, 0)]) 0"
  -- [0] (allNeighboursOf (ELg 0 [(0, 1, 0)]) 0))

-- allNeighboursOfTest4 = TestCase (assertEqual "for allNeighboursOf (ELg 1 [(0, 1, 1)]) 1"
  -- [] (allNeighboursOf (ELg 1 [(0, 1, 1)]) 1))

-- allNeighboursOfTest5 = TestCase (assertEqual "for allNeighboursOf (ELg 2 [(0, 1, 0), (0, 1, 1), (0, 1, 2)]) 0"
  -- [0, 1, 2] (allNeighboursOf (ELg 2 [(0, 1, 0), (0, 1, 1), (0, 1, 2)]) 0))

-- numberOfEdgesTest1 = TestCase (assertEqual "for numberOfEdges (el2am (ELg 0 []))"
  -- 0 (numberOfEdges (el2am (ELg 0 []))))

-- numberOfEdgesTest2 = TestCase (assertEqual "for numberOfEdges (el2am (ELg 0 [(0, 1, 0)]))"
  -- 1 (numberOfEdges (el2am (ELg 0 [(0, 1, 0)]))))

-- isOnCycleTest1 = TestCase (assertEqual "for isOnCycle (ALg [(0, [])]) 0 0"
  -- False (isOnCycle (ALg [(0, [])]) 0 0))

-- isOnCycleTest2 = TestCase (assertEqual "for isOnCycle (ALg [(0, [(0, 1)])]) 0 0"
  -- False (isOnCycle (ALg [(0, [(0, 1)])]) 0 0))

-- isOnCycleTest3 = TestCase (assertEqual "for isOnCycle (ALg [(0, [(0, 1)])]) 0 1"
  -- True (isOnCycle (ALg [(0, [(0, 1)])]) 0 1))

-- isOnCycleTest4 = TestCase (assertEqual "for isOnCycle (ALg [(0, [(1, 1), (2, 1)]), (1, [(2, 1)]), (2, [(0, 1)])]) 0 2"
  -- True (isOnCycle (ALg [(0, [(1, 1), (2, 1)]), (1, [(2, 1)]), (2, [(0, 1)])]) 0 2))

-- miscTests = TestList [isNeighbourOfTest1, isNeighbourOfTest2, isNeighbourOfTest3,
  -- isNeighbourOfTest4, isNeighbourOfTest5, allNeighboursOfTest1, allNeighboursOfTest2,
  -- allNeighboursOfTest3, allNeighboursOfTest4, allNeighboursOfTest5, numberOfEdgesTest1,
  -- numberOfEdgesTest2, isOnCycleTest1, isOnCycleTest2, isOnCycleTest3, isOnCycleTest4]

-----------------------------------------------------------------------------

tests :: [Test]
tests = [isValidTests]--, konvTests]--, miscTests]

main = do
  forM tests $ \test ->
    runTestTT test