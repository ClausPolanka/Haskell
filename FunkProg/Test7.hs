#!/usr/bin/runhugs

-- Module ---------------------------------------------------------------------

module Main where

-- Imports ---------------------------------------------------------------------

import Aufgabe7K
import Test.HUnit

-- Functions -------------------------------------------------------------------

assertTrue  msg = assertEqual msg True
assertFalse msg = assertEqual msg False

-- Testcases ------------------------------------------------------------------- 

b1_a1 = (AMg [
				(["de","a","c","b"]),
				(["de","a","c","b"]),
				(["de","a","c",""]),
				(["de","a","","b"])
			])

b1_t1 = TestCase (assertTrue "ab" (isPostfix b1_a1 0 [0,2,3] "ab"))
b1_t2 = TestCase (assertFalse "aba" (isPostfix b1_a1 0 [0,2,3] "aba"))

-- Beachten Sie, dass das Resultat i.a. nicht eindeutig festgelegt ist
b2_t1 = TestCase (assertEqual "givePrefix b1_a1 0 [0,2,3] \"ab\" == (Just \"a\")" (Just "a") (givePrefix b1_a1 0 [0,2,3] "ab"))
b2_t2 = TestCase (assertEqual "givePrefix b1_a1 0 [0,2,3] \"aba\" == Nothing" Nothing (givePrefix b1_a1 0 [0,2,3] "aba"))


b1_a2 = (AMg [	(["","F","","","","","","","",""]),
				(["","","a","","","","","","",""]),
				(["","","","h","","","","","",""]),
				(["","","","","r","","","","",""]),
				(["","","","","","e","","","",""]),
				(["","","","","","","n","","",""]),
				(["","","","","","","","h","",""]),
				(["","","","","","","","","e",""]),
				(["","","","","","","","","","i"]),
				(["t","","","","","","","","",""])
		])

b1_t3 = TestCase (assertTrue "" (isPostfix b1_a2 0 [0] ""))
b1_t4 = TestCase (assertTrue "heit" (isPostfix b1_a2 0 [0] "heit"))
b1_t5 = TestCase (assertFalse "heite" (isPostfix b1_a2 0 [0] "heite"))
b1_t6 = TestCase (assertTrue "Fahrenheit" (isPostfix b1_a2 0 [0] "Fahrenheit"))
b2_t3 = TestCase (assertEqual "" (Just "") (givePrefix b1_a2 0 [0] ""))
b2_t4 = TestCase (assertEqual "heit" (Just "Fahren") (givePrefix b1_a2 0 [0] "heit" ))
b2_t5 = TestCase (assertEqual "heite" Nothing (givePrefix b1_a2 0 [0] "heite"))
b2_t6 = TestCase (assertEqual "Fahrenheit" (Just "") (givePrefix b1_a2 0 [0] "Fahrenheit"))

test_prefix = TestList
	[
	b1_t1,
	b1_t2,
	b1_t3,
	b1_t4,
	b1_t5,
	b1_t6,
	b2_t1,
	b2_t2,
	b2_t3,
	b2_t4,
	b2_t5,
	b2_t6
	]

-- traverse --------------------------------------------------------------------

b3_g1 = (ALbg [(0,5,[0..3]), (1,4,[0..3]), (2,333,[]), (3,2,[])])

albGraph1 = 
	ALbg [
		(0,[],[10,3]), 
		(2,[1,2],[]), 
		(3,[1,2,3],[]), 
		(10,[1..9],[2])
	]

b3_t1 
	= traverse ((flip (-)) 1) odd b3_g1 
		== ALbg [(0,4,[0,1,2,3]),(1,4,[0,1,2,3]),(2,332,[]),(3,2,[])]
b3_t2
	= (traverse ((*) 3) ((/=) 0 . (flip mod) 3) b3_g1) 
		== ALbg [(0,15,[0,1,2,3]),(1,12,[0,1,2,3]),(2,333,[]),(3,6,[])]

test_traverse1 = TestCase (assertTrue "traverse - Official Testcase 1" b3_t1)
test_traverse2 = TestCase (assertTrue "traverse - Official Testcase 2" b3_t2)
test_traverse3 = TestCase (assertEqual 
	"traverse - Add 66 to every key (key=list of integers) in a graph where \
	\the keylength is below 5"
	(ALbg [
		(0,[66],[10,3]), 
		(2,[66,1,2],[]), 
		(3,[66,1,2,3],[]), 
		(10,[1..9],[2])
	])
	(traverse (66:) ((<5) . length) albGraph1)
	)
test_traverse4 = TestCase (assertEqual 
	"traverse - Replace every key (key=list of integers) in a graph where \
	\the keylength is greater than 6 with an empty list"
	(ALbg [
		(0,[],[10,3]),
		(2,[1,2],[]), 
		(3,[1,2,3],[]), 
		(10,[],[2])
	])
	(traverse ((\a b -> a) []) ((>6) . length) albGraph1)
	)
test_traverse5 = TestCase (assertEqual 
	"traverse - Replace every value in a key (key=list of integers) in a graph\ 
	\ with its negative value if the list does not contain 9"
	(ALbg [
		(0,[],[10,3]),
		(2,[-1,-2],[]), 
		(3,[-1,-2,-3],[]), 
		(10,[1..9],[2])
	])
	(traverse (map (* (-1))) (notElem 9) albGraph1)
	)

test_traverse = TestList
	[
	test_traverse1,
	test_traverse2,
	test_traverse3,
	test_traverse4,
	test_traverse5
	]

-- isWellColored ---------------------------------------------------------------

b4_g1 = (Ug [(0,Red,[1..3]), (1,Blue,[0,2,3]), (2,Green,[]), (3,Yellow,[])])
b4_g2 = (Ug [(0,Red,[1..3]), (1,Blue,[0,2,3]), (2,Yellow,[]), (3,Yellow,[])])
b4_g3 = (Ug [(0,Blue,[1..3]), (1,Blue,[0,2,3]), (2,Yellow,[]), (3,Yellow,[])])

b4_t1 = isWellColored b4_g1 == True
b4_t2 = isWellColored b4_g2 == True
b4_t3 = isWellColored b4_g3 == False

graph_rows1 = 
	[
	(1, 	Red, 		[2,3,4]),
	(2, 	Blue, 		[4,1]),
	(3, 	Green, 		[1,5]),
	(4, 	Yellow, 	[2,1,5,6])
	]
graph_rows2 =
	[
	(5,		Red, 		[3,4]),
	(6, 	Blue,		[4]),
	(11, 	Yellow, 	[10]),
	(10, 	Green, 		[11])
	]	
graph_rows3 =
	[
	(30, 	Red, 		[20]),
	(20, 	Red, 		[30])
	]
graph_rows4 =
	[
	(5,		Red, 		[3,4,7]),
	(6, 	Blue,		[4,7]),
	(7, 	Yellow, 	[8,5,6]),
	(8,		Green,		[7])
	]
graph_rows5 =
	[
	(5,		Red, 		[7]),
	(6, 	Blue,		[7]),
	(7, 	Yellow, 	[5,6,8]),
	(8,		Green,		[7])
	]

uGraph1 = Ug []
uGraph2 = Ug (graph_rows1 ++ graph_rows2)
uGraph3 = Ug (graph_rows1 ++ graph_rows2 ++ graph_rows3)
uGraph4 = Ug graph_rows3
uGraph5 = Ug (graph_rows1 ++ graph_rows4)
uGraph6 = Ug graph_rows5

wellColored1 = isWellColored uGraph1 == True
wellColored2 = isWellColored uGraph2 == True
wellColored3 = isWellColored uGraph3 == False
wellColored4 = isWellColored uGraph4 == False
wellColored5 = isWellColored uGraph5 == True
wellColored6 = isWellColored uGraph6 == True

test_isWellColored1 = TestCase (assertTrue 
	"isWellColored - Official Testcase 1" b4_t1)
test_isWellColored2 = TestCase (assertTrue 
	"isWellColored - Official Testcase 2" b4_t2)
test_isWellColored3 = TestCase (assertTrue 
	"isWellColored - Official Testcase 3" b4_t3)
test_isWellColored4 = TestCase (assertTrue
 
	"An empty graph is well colored" wellColored1)
test_isWellColored5 = TestCase (assertTrue 
	"A graph which is well colored" wellColored2)
test_isWellColored6 = TestCase (assertTrue 
	"A graph which is not well colored" wellColored3)
test_isWellColored7 = TestCase (assertTrue 
	"A graph which is not well colored" wellColored4)
test_isWellColored8 = TestCase (assertTrue 
	"A graph which is well colored" wellColored5)
test_isWellColored9 = TestCase (assertTrue 
	"A graph which is well colored" wellColored6)

test_isWellColored = TestList 
	[
	test_isWellColored1,
	test_isWellColored2,
	test_isWellColored3,
	test_isWellColored4,
	test_isWellColored5,
	test_isWellColored6,
	test_isWellColored7,
	test_isWellColored8,
	test_isWellColored9
	]

-- Testlist --------------------------------------------------------------------

everything = TestList
	[
	test_prefix,
	test_traverse,
	test_isWellColored
	]

-- Main ----------------------------------------------------------------------

main = do
		test <- (runTestTT everything)
		print (test)