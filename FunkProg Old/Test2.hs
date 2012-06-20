#! runhugs

-- Module ---------------------------------------------------------------------

module Main where

-- Imports ---------------------------------------------------------------------

import Aufgabe2
import Test.HUnit
import IO (stderr, hPutStr)

-- Functions -------------------------------------------------------------------

printTest :: Bool -> IO ()
printTest False = putStr (" ;-(")
printTest True	= putStr (" :-)")

-- Testcases -------------------------------------------------------------------

-- diffFolge -------------------------------------------------------------------

m1 = 7834758347587
n1 = 235462354

test_diffFolge1 = TestCase (assertEqual "for" 
	[20,13,6,-1] ((diffFolge :: (Integer,Integer) -> [Integer]) (20,7)))
test_diffFolge2 = TestCase (assertEqual "for" 
	[21045,(21045-21)..(-20)] (diffFolge (21045,21)))
test_diffFolge3 = TestCase (assertEqual "for" [0] (diffFolge (0,9)))
test_diffFolge4 = TestCase (assertEqual "for" 
	[m1, m1-n1..(-n1+1)] (diffFolge (m1,n1)))
test_diffFolge5 = TestCase (assertEqual "for" [n1,0] (diffFolge (n1,n1)))

test_diffFolge = TestList
	[
	TestLabel "diffFolge - Test 1" test_diffFolge1,
	TestLabel "diffFolge - Test 2" test_diffFolge2,
	TestLabel "diffFolge - Test 3" test_diffFolge3,
	TestLabel "diffFolge - Test 4" test_diffFolge4,
	TestLabel "diffFolge - Test 5" test_diffFolge5
	]

-- teilt -----------------------------------------------------------------------

divisor1 = 67
number1 = 32396*divisor1

divisor2 = 23224
number2 = 45*divisor2-1

test_teilt1 = TestCase (assertEqual "for" True 
	((teilt::(Integer,Integer) -> Bool) (21,7)))
test_teilt2 = TestCase (assertEqual "for" False (teilt (24,10)))
test_teilt3 = TestCase (assertEqual "for" True (teilt (number1,divisor1)))
test_teilt4 = TestCase (assertEqual "for" False (teilt (number2,divisor2)))
test_teilt5 = TestCase (assertEqual "for" True (teilt (10^3,10)))

test_teilt = TestList
	[
	TestLabel "teilt - Test 1" test_teilt1,
	TestLabel "teilt - Test 2" test_teilt2,
	TestLabel "teilt - Test 3" test_teilt3,
	TestLabel "teilt - Test 4" test_teilt4,
	TestLabel "teilt - Test 5" test_teilt5
	]

-- zahlenBlock -----------------------------------------------------------------

test_zahlenBlock1 = TestCase (assertEqual "for" 
	((zahlenBlock :: Integer -> [Integer]) 24889375) [375,889,24])
test_zahlenBlock2 = TestCase (assertEqual "for" (zahlenBlock 99) [99])
test_zahlenBlock3 = TestCase (assertEqual "for" (zahlenBlock 237223) [223,237])
test_zahlenBlock4 = TestCase (assertEqual "for" (zahlenBlock 37223) [223,37])

test_zahlenBlock = TestList
	[
	TestLabel "zahlenBlock - Test 1" test_zahlenBlock1,
	TestLabel "zahlenBlock - Test 2" test_zahlenBlock2,
	TestLabel "zahlenBlock - Test 3" test_zahlenBlock3,
	TestLabel "zahlenBlock - Test 4" test_zahlenBlock4	
	]

-- zeichenreihenBlock ----------------------------------------------------------

test_zeichenreihenBlock1 = TestCase (assertEqual "for" 
	["375","889","024"] ((zeichenreihenBlock :: Integer -> [String]) 24889375))
test_zeichenreihenBlock2 = TestCase (assertEqual "for" 
	["346","627","674","023"] (zeichenreihenBlock 23674627346))
test_zeichenreihenBlock3 = TestCase (assertEqual "for" 
	["824","547","037"] (zeichenreihenBlock 037547824))
test_zeichenreihenBlock4 = TestCase (assertEqual "for" 
	["005"] (zeichenreihenBlock 5))
test_zeichenreihenBlock5 = TestCase (assertEqual "for" 
	["099"] (zeichenreihenBlock 99))

test_zeichenreihenBlock = TestList
	[
	TestLabel "zeichenreihenBlock - Test 1" test_zeichenreihenBlock1,
	TestLabel "zeichenreihenBlock - Test 2" test_zeichenreihenBlock2,
	TestLabel "zeichenreihenBlock - Test 3" test_zeichenreihenBlock3,
	TestLabel "zeichenreihenBlock - Test 4" test_zeichenreihenBlock4,
	TestLabel "zeichenreihenBlock - Test 5" test_zeichenreihenBlock5
	]

-- as --------------------------------------------------------------------------

test_as1 = TestCase (assertEqual "for"
	(1212-23-237263-2903+23+1)
	((as :: [Integer] -> Integer) [1212,23,-237263,2903,23,-1]))
test_as2 = TestCase (assertEqual "for" 0 (as [99..0]))
test_as3 = TestCase (assertEqual "for" 
	(sum [374,4,10] - sum [2,56]) (as [374,2,4,56,10]) )

test_as = TestList
	[
	TestLabel "as - Test 1" test_as1,
	TestLabel "as - Test 2" test_as2,
	TestLabel "as - Test 3" test_as3
	]

-- siebenTeilbar ---------------------------------------------------------------

test_siebenTeilbar1 = TestCase (assertEqual "for" True (siebenTeilbar 77))
test_siebenTeilbar2 = TestCase (assertEqual "for" 
	False (siebenTeilbar (77*1000-1)))
test_siebenTeilbar3 = TestCase (assertEqual "for" 
	True (siebenTeilbar (-77*1000)))
test_siebenTeilbar4 = TestCase (assertEqual "for" 
	[True,False,False,True] (map siebenTeilbar [-77*10,20,10^2,7*(-34)]))


test_siebenTeilbar = TestList
	[
	TestLabel "siebenTeilbar - Test 1" test_siebenTeilbar1,	
	TestLabel "siebenTeilbar - Test 2" test_siebenTeilbar2,
	TestLabel "siebenTeilbar - Test 3" test_siebenTeilbar3,
	TestLabel "siebenTeilbar - Test 4" test_siebenTeilbar4
	]

-- Testlist --------------------------------------------------------------------

everything = TestList
	[
	test_diffFolge,
	test_teilt,
	test_zahlenBlock,
	test_zeichenreihenBlock,
	test_as,
	test_siebenTeilbar
	]

-- Main ------------------------------------------------------------------------

main = do
		test <- (runTestTT everything)
		print (test)
		{--
		putStr ("\n-- Test negative numbers --\n")
		putStr ("\n-> diffFolge\n")
		putStr ("(-12,12):\t")
		print (diffFolge (-12, 12))
		putStr ("(-134,-10):\t")
		print (diffFolge (-34,-10))
		putStr ("\n-> teilt ")
		printTest (map teilt [(-920,23),(69997,-30),(-25,-25),(20*264,-264)] == 
			[True, False, True, True]) 
		putStr ("\n\n-> zahlenBlock\n")
		putStr ("-1234:\t")
		print (zahlenBlock (-1234))
		putStr ("-12:\t")
		print (zahlenBlock (-12))
		putStr ("\n-> zeichenreihenBlock\n")
		putStr ("-1234:\t")
		print (zeichenreihenBlock (-1234))
		putStr ("-12:\t")
		print (zeichenreihenBlock (-12))
		--}