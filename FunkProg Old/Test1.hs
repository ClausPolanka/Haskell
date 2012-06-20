#! runhugs

-- Module ----------------------------------------------------------------------

module Main where

-- Imports ---------------------------------------------------------------------

import Aufgabe1
import IO (stderr, hPutStr)

-- Functions -------------------------------------------------------------------

printTest :: Bool -> IO ()
printTest False = putStr (" ;-(")
printTest True	= putStr (" :-)")

-- Main ------------------------------------------------------------------------

main =
	do
		putStr ("Start Testing\n-------------\n\n")
		putStr ("-- 1: convert -----------------------------------------\n\n")
			
		putStr ("Given Testcases: ")
		printTest (and ((convert 2010 == [2,0,1,0]) :
					[convert 185161 == [1,8,5,1,6,1]]))
		
		putStr ("\nOther Testcases: ")
		printTest ((convert :: Integer -> [Integer]) 12345678910 == 
			[1..9]++[1,0])
		printTest (convert 0 == [0])
		printTest (convert 981223434 == [9,8,1,2,2,3,4,3,4])
		printTest (convert 3848334 == [3,8,4,8,3,3,4])
		putStr ("\n\nTrying negative numbers - output should not matter:\n")
		putStr ("\n-1:\t ")
		print (convert (-1))
		putStr ("-34356:\t ")
		print (convert (-34356))
		
		putStr ("\n-- 2: quersumme -------------------------------------\n\n")
			
		putStr ("Given Testcases: ")
		printTest (and ((quersumme 2010 == 3) : [quersumme 185161 == 22]))
		putStr ("\nOther Testcases: ")
		printTest ((quersumme :: Integer -> Integer) 32489234 == 35)
		printTest (quersumme 12345783 == 33)
		printTest ((quersumme 3278478473465789234958349583495983) == 
			3+2+7+8+4+7+8+4+7+3+4+6+5+7+8+9+2+3+4+9+5+8+3+4+9+5+8+3+4+9+5+9+8+3)
		putStr ("\n\nTrying negative numbers - output should not matter:\n")
		putStr ("\n-1:\t")
		print (quersumme (-1))
		putStr ("-34356:\t ")
		print (quersumme (-34356))	
		
		putStr ("\n-- 3: dreiTeilbar -----------------------------------\n\n")
		
		putStr ("Testcases: ")
		printTest ((dreiTeilbar :: Integer -> Bool) 2010 == True)
		printTest (dreiTeilbar 185161 == False)
		printTest (dreiTeilbar 3 == True)
		printTest (dreiTeilbar 14 == False)
		printTest (map dreiTeilbar 
			[33,34345,1015302,76666854,123,32742384827589234,6373467364] ==
			[True,False,True,True,True,True,False])
		putStr ("\n\nTrying negative numbers ")
		putStr ("- result of this test should not matter: ")
		printTest (map dreiTeilbar 
			[-33,-34345,-1015302,-76666854,-123,-32742384827589234,-6373467364] 
			== [True,False,True,True,True,True,False])
		
		putStr ("\n\n-- 4: sechsTeilbar ----------------------------------\n\n")
			
		putStr ("Testcases: ")
		printTest ((sechsTeilbar :: Integer -> Bool) 2010 == True)
		printTest (sechsTeilbar 185161 == False)
		printTest (sechsTeilbar 3 == False)
		printTest (sechsTeilbar 18 == True)
		printTest (map sechsTeilbar 
			[60,1950,32742384827589234,6373467364, 234936] ==
			[True,True,True,False,True])
		putStr ("\n\nTrying negative numbers ")
		putStr ("- result of this test should not matter: ")
		printTest (map sechsTeilbar
			[-60,-1950,-32742384827589234,-6373467364,-234936] ==
			[True,True,True,False,True])
		
		putStr ("\n\n-- 5: elfTeilbar ----------------------------------\n\n")
			
		putStr ("Given Testcases: ")
		printTest (elfTeilbar 56518 == True)
		
		putStr ("\nOther Testcases: ")
		printTest ((elfTeilbar :: Integer -> Bool) 2010 == False)
		printTest (elfTeilbar (-185161) == False)
		printTest (elfTeilbar 3 == False)
		printTest (elfTeilbar (11*374634) == True)
		printTest (map elfTeilbar 
			[56518,11,10,12,91601820717,423,3565,37774] == 
			[True,True,False,False,True,False,False,True])
		putStr ("\n\nTrying negative numbers ")
		putStr ("- result of this test should not matter: ")
		printTest (map elfTeilbar 
			[-56518,-11,-10,-12,-91601820717,-423,-3565,-37774] == 
			[True,True,False,False,True,False,False,True])