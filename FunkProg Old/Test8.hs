module Main where

-- Imports ---------------------------------------------------------------------

import Aufgabe8
import Test.HUnit
import List

-- Functions -------------------------------------------------------------------

assertTrue  msg = assertEqual msg True
assertFalse msg = assertEqual msg False

-- Testcases ------------------------------------------------------------------- 
-- Testfaelle zu Aufgabenteil 1
-- Beachten Sie, dass das Resultat i.a. nicht eindeutig festgelegt ist
b1_t1 = TestCase (assertEqual "b1_t1" (Just (Ug [(0,Red,[1,2,3]),(1,Blue,[0,2,3]),(2,Green,[]),(3,Green,[])])) (color (Ug [(0,Red,[1,2,3]), (1,Red,[0,2,3]), (2,Red,[]), (3,Red,[])])))
b1_t2 = TestCase (assertEqual "b1_t2" Nothing (color (Ug [ (i,Red,[0..5]\\[i]) | i <- [0..5]])))
b1_t3 = TestCase (assertEqual "b1_t3" (Just (Ug [(0,Red,[1]),(1,Blue,[2]),(2,Red,[3]),(3,Blue,[4]),(4,Red,[5]),(5,Blue,[0])]) ) (color (Ug [ (i,Red,[(i+1) `mod` 6]) | i <- [0..5]])))

tests_color = TestList
	[
	b1_t1,
	b1_t2,
	b1_t3
	]

-- Testfaelle zu Aufgabenteil 2
-- basic start sudokus
 
basic_start1 = [[9,1,6,0,0,4,0,7,2],
                [8,0,0,6,2,0,0,5,0],
                [5,0,0,0,0,8,9,3,0],
                [0,6,0,0,0,0,2,0,0],
                [0,0,0,2,0,7,0,0,0],
                [0,0,5,0,0,0,0,9,0],
                [0,9,7,8,0,0,0,0,3],
                [0,8,0,0,7,6,0,0,9],
                [4,5,0,1,0,0,6,8,7]]

basic_start2 = [[6,0,0,3,0,0,1,0,0],
                [0,7,1,6,2,0,0,0,0],
                [8,0,5,0,0,1,0,0,0],
                [5,0,0,8,7,0,9,0,1],
                [0,0,9,0,0,0,6,0,0],
                [4,0,7,0,6,9,0,0,8],
                [0,0,0,2,0,0,8,0,7],
                [0,0,0,0,8,6,4,1,0],
                [0,0,8,0,0,3,0,0,2]]


basic_start3 = [[9,0,6,0,1,3,0,0,8],
                [0,5,8,0,0,0,0,9,0],
                [0,3,0,0,0,0,0,1,0],
                [0,6,0,8,0,0,9,2,0],
                [0,0,3,4,0,9,1,0,0],
                [0,4,9,0,0,6,0,3,0],
                [0,9,0,0,0,0,0,8,0],
                [0,1,0,0,0,0,6,7,0],
                [4,0,0,9,6,0,3,0,1]]


basic_inv1 =   [[9,0,6,0,1,3,0,0,8],
                [0,5,8,0,0,0,0,9,0],
                [0,3,0,0,0,0,0,1,0],
                [0,6,0,8,0,0,9,2,0],
                [0,0,3,4,0,9,9,0,0],
                [0,4,9,0,0,6,0,3,0],
                [0,9,0,0,0,0,0,8,0],
                [0,1,0,0,0,0,6,7,0],
                [4,0,0,9,6,0,3,0,1]]

basic_inv2 =   [[9,0,6,0,1,3,0,0,8],
                [1,5,8,0,0,0,0,9,0],
                [0,3,1,0,0,0,0,1,0],
                [0,6,0,8,0,0,9,2,0],
                [0,0,3,4,0,9,1,0,0],
                [0,4,9,0,0,6,0,3,0],
                [0,9,0,0,0,0,0,8,0],
                [0,1,0,0,0,0,6,7,0],
                [4,0,0,9,6,0,3,0,1]]

-- cross start sudokus

cross_start1 =  [[0,0,3,4,0,0,0,8,0], 
                 [4,5,6,7,8,9,1,2,0],
                 [7,8,0,1,2,3,0,5,0], 
                 [0,6,4,2,1,7,9,3,0], 
                 [2,9,1,5,3,8,0,7,4], 
                 [3,0,8,6,9,4,0,0,0], 
                 [0,3,5,9,0,2,0,4,1], 
                 [0,1,7,8,0,0,0,6,2], 
                 [0,0,2,0,6,1,0,0,0]]

cross_start2 =  [[0,0,3,4,0,0,0,0,0], 
                 [4,5,0,7,8,9,1,0,0],
                 [7,8,0,1,2,3,0,5,0], 
                 [0,6,4,2,1,7,9,0,0], 
                 [2,9,1,5,0,0,0,7,4], 
                 [0,0,8,6,9,4,0,0,0], 
                 [0,0,5,9,0,2,0,4,1], 
                 [0,0,7,8,0,0,0,6,2], 
                 [0,0,2,0,0,1,0,0,0]]


cross_inv1 =    [[0,0,3,4,0,0,0,0,0], 
                 [4,5,0,7,8,9,1,0,0],
                 [7,8,0,1,2,3,0,5,0], 
                 [0,6,4,2,1,7,9,0,0], 
                 [2,9,1,5,0,0,0,7,4], 
                 [0,0,8,6,9,4,0,0,0], 
                 [0,0,5,9,0,2,0,4,1], 
                 [0,0,7,8,0,0,0,6,2], 
                 [0,0,2,0,0,1,0,0,6]]

-- color start sudokus

color_start1 = [[1,2,3,0,5,0,0,0,9], 
                [0,0,0,0,0,0,0,0,3], 
                [7,0,9,0,2,3,4,5,6], 
                [2,0,1,0,0,4,8,9,5],
                [8,0,5,9,1,2,0,0,0], 
                [0,0,0,5,3,8,2,7,1], 
                [0,0,0,3,6,7,5,4,8], 
                [5,4,8,2,9,0,0,0,0], 
                [0,6,7,8,4,0,0,0,0]]

color_start2 = [[0,0,3,4,5,0,0,0,0], 
                [0,0,6,0,0,0,1,2,3], 
                [0,8,9,1,2,3,4,5,6], 
                [0,3,1,6,7,4,8,9,5],
                [0,7,5,0,0,0,3,0,0], 
                [6,9,4,5,3,0,2,0,0], 
                [9,1,2,3,0,0,0,0,0], 
                [0,0,0,2,9,0,6,0,0], 
                [0,0,0,8,4,0,0,0,0]]

color_inv1   = [[0,0,3,4,5,0,0,0,0], 
                [0,0,6,0,0,6,1,2,3], 
                [0,8,9,1,2,3,4,5,6], 
                [0,3,1,6,7,4,8,9,5],
                [0,7,5,0,0,0,3,0,0], 
                [6,9,4,5,3,0,2,0,0], 
                [9,1,2,3,0,0,0,0,0], 
                [0,0,0,2,9,0,6,0,0], 
                [0,0,0,8,4,0,0,0,0]]


-- 2.a

t_val1 = TestCase (assertTrue "basic_start1" (isValid basic_start1 Basic))
t_val2 = TestCase (assertTrue "basic_start2" (isValid basic_start2 Basic))
t_val3 = TestCase (assertTrue "basic_start3" (isValid basic_start3 Basic))
t_val4 = TestCase (assertTrue "cross_start1" (isValid cross_start1 Cross))
t_val5 = TestCase (assertTrue "cross_start2" (isValid cross_start2 Cross))
t_val6 = TestCase (assertTrue "color_start1" (isValid color_start1 Color))
t_val7 = TestCase (assertTrue "color_start2" (isValid color_start2 Color))
t_val8 = TestCase (assertFalse "basic_inv1" (isValid basic_inv1 Basic))
t_val9 = TestCase (assertFalse "basic_inv2"  (isValid basic_inv2 Basic))
t_val10 = TestCase (assertFalse "cross_inv1 Cross" (isValid cross_inv1 Cross))
t_val11 = TestCase (assertFalse "cross_inv1 Color" (isValid color_inv1 Color))


tests_2a_isValid = TestList
	[
        t_val1,t_val2,t_val3,t_val4,t_val5,t_val6,t_val7,
        t_val8,t_val9,t_val10,t_val11	]

-- 2.b
-- * Beachten Sie, dass die voll ausgefuellte Sudoku zu einer Anfangs-Sudoku-Matrix i.a nicht eindeutig ist

-- Basis-Instanzen

t_basic1 = TestCase (assertEqual "t_basic1" (Just [[9,1,6,3,5,4,8,7,2],[8,7,3,6,2,9,1,5,4],[5,2,4,7,1,8,9,3,6],[7,6,8,9,3,5,2,4,1],[1,4,9,2,8,7,3,6,5],[2,3,5,4,6,1,7,9,8],[6,9,7,8,4,2,5,1,3],[3,8,1,5,7,6,4,2,9],[4,5,2,1,9,3,6,8,7]]) (solve basic_start1 Basic))
t_basic2 = TestCase (assertEqual "t_basic2" (Just [[6,2,4,3,5,7,1,8,9],[9,7,1,6,2,8,3,5,4],[8,3,5,4,9,1,7,2,6],[5,6,3,8,7,2,9,4,1],[2,8,9,1,3,4,6,7,5],[4,1,7,5,6,9,2,3,8],[3,4,6,2,1,5,8,9,7],[7,5,2,9,8,6,4,1,3],[1,9,8,7,4,3,5,6,2]]) (solve basic_start2 Basic))
t_basic3 = TestCase (assertEqual "t_basic3" (Just [[9,7,6,5,1,3,2,4,8],[1,5,8,6,4,2,7,9,3],[2,3,4,7,9,8,5,1,6],[7,6,1,8,3,5,9,2,4],[8,2,3,4,7,9,1,6,5],[5,4,9,1,2,6,8,3,7],[6,9,7,3,5,1,4,8,2],[3,1,5,2,8,4,6,7,9],[4,8,2,9,6,7,3,5,1]]) (solve basic_start3 Basic))
t_basic4 = TestCase (assertEqual "t_basic4" Nothing (solve basic_inv1 Basic))
t_basic5 = TestCase (assertEqual "t_basic5" Nothing (solve basic_inv2 Basic))

tests_2b_basic = TestList [t_basic1, t_basic2, t_basic3, t_basic4, t_basic5]

-- Kreuz-Instanzen

t_cross1 = TestCase (assertEqual "t_cross1" (Just [[1,2,3,4,5,6,7,8,9],[4,5,6,7,8,9,1,2,3],[7,8,9,1,2,3,4,5,6],[5,6,4,2,1,7,9,3,8],[2,9,1,5,3,8,6,7,4],[3,7,8,6,9,4,2,1,5],[6,3,5,9,7,2,8,4,1],[9,1,7,8,4,5,3,6,2],[8,4,2,3,6,1,5,9,7]]) (solve cross_start1 Cross))
t_cross2 = TestCase (assertEqual "t_cross2" (Just [[1,2,3,4,5,6,7,8,9],[4,5,6,7,8,9,1,2,3],[7,8,9,1,2,3,4,5,6],[5,6,4,2,1,7,9,3,8],[2,9,1,5,3,8,6,7,4],[3,7,8,6,9,4,2,1,5],[6,3,5,9,7,2,8,4,1],[9,1,7,8,4,5,3,6,2],[8,4,2,3,6,1,5,9,7]]) (solve cross_start2 Cross))
t_cross3 = TestCase (assertEqual "t_cross3" Nothing (solve cross_inv1 Cross))

tests_2b_cross = TestList [t_cross1, t_cross2, t_cross3]

-- Farb-Instanzen
-- Die Loesungen zu Color Instanzen sind nicht eindeutig

t_color1 = TestCase (assertEqual "t_color1" (Just [[1,2,3,4,5,6,7,8,9],[4,5,6,7,8,9,1,2,3],[7,8,9,1,2,3,4,5,6],[2,3,1,6,7,4,8,9,5],[8,7,5,9,1,2,3,6,4],[6,9,4,5,3,8,2,7,1],[9,1,2,3,6,7,5,4,8],[5,4,8,2,9,1,6,3,7],[3,6,7,8,4,5,9,1,2]]) (solve color_start1 Color))
t_color2 = TestCase (assertEqual "t_color2" (Just [[1,2,3,4,5,6,7,8,9],[4,5,6,7,8,9,1,2,3],[7,8,9,1,2,3,4,5,6],[2,3,1,6,7,4,8,9,5],[8,7,5,9,1,2,3,6,4],[6,9,4,5,3,8,2,7,1],[9,1,2,3,6,7,5,4,8],[5,4,8,2,9,1,6,3,7],[3,6,7,8,4,5,9,1,2]]) (solve color_start2 Color))
t_color3 = TestCase (assertEqual "t_color3" Nothing (solve color_inv1 Color))

tests_2b_color = TestList [t_color1, t_color2, t_color3]

-- Testlist --------------------------------------------------------------------

everything = TestList
	[
	tests_color,
    tests_2a_isValid,
    tests_2b_basic,
    tests_2b_cross,
    tests_2b_color
	]

-- Main ----------------------------------------------------------------------

main = do
		test <- (runTestTT everything)
		print (test)