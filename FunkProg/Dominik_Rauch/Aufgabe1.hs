--------------------------------------------------------------------------------
-- 1. Aufgabenblatt zu Funktionale Programmierung vom 05.10.2010
-- 
-- Gruppe: 145
-- Mitglieder: Felix MAYERHUBER (0825283) und Dominik RAUCH (0825084)
-- 
-- Created: 06.10.2010
--------------------------------------------------------------------------------

import Prelude hiding (sum)

--------------------------------------------------------------------------------
-- 1.1 convert
-- Zerlegt eine ganze Zahl in die Folge seiner Ziffern
--------------------------------------------------------------------------------

convert :: Integer -> [Integer]
convert n
	| n == 0 = [0]
	| otherwise = convertRec n

convertRec :: Integer -> [Integer]
convertRec n
	| n == 0 = []
	| otherwise = convertRec(div n 10) ++ [mod n 10]
	
--------------------------------------------------------------------------------
-- 1.2 quersumme
-- Berechnet die Quersumme einer ganzen Zahl
--------------------------------------------------------------------------------

quersumme :: Integer -> Integer
quersumme n = sum digits
	where
	digits = convert n

sum :: [Integer] -> Integer
sum [] = 0
sum (x:xs) = x + sum xs

--------------------------------------------------------------------------------
-- 1.3 dreiTeilbar
-- True falls die Zahl ohne Rest durch drei teilbar ist, False anderenfalls
--------------------------------------------------------------------------------

dreiTeilbar :: Integer -> Bool
dreiTeilbar n = mod qs 3 == 0
	where
	qs = quersumme n

--------------------------------------------------------------------------------
-- 1.4 sechsTeilbar
-- True falls die Zahl ohne Rest durch sechs teilbar ist, False anderenfalls
--------------------------------------------------------------------------------

sechsTeilbar :: Integer -> Bool
sechsTeilbar n = zweiTeilbar n && dreiTeilbar n
	
zweiTeilbar :: Integer -> Bool
zweiTeilbar n = mod n 2 == 0

--------------------------------------------------------------------------------
-- 1.5 elfTeilbar
-- True falls die Zahl ohne Rest durch elf teilbar ist, False anderenfalls
--------------------------------------------------------------------------------

elfTeilbar :: Integer -> Bool
elfTeilbar n = mod digitsum 11 == 0
	where
	digits = convert n
	digitsum = alterSum digits

-- Assoziativität falsch!!	10 - 10 + 10 - 0 = 10 hier alerdings: 10 - (10 + (10 - 0)) = -10 !!
-- alterSum :: [Integer] -> Integer
-- alterSum xs = alterSumRec xs True

-- alterSumRec :: [Integer] -> Bool -> Integer
-- alterSumRec [] _ = 0
-- alterSumRec (x:xs) b
	-- | b == True = x - alterSumRec xs (not b)
	-- | otherwise = x + alterSumRec xs (not b)

alterSum :: [Integer] -> Integer
alterSum [] = 0
alterSum (x:xs) = x + alterSum2 xs

alterSum2 :: [Integer] -> Integer
alterSum2 [] = 0
alterSum2 (x:xs) = (-x) + alterSum xs

--------------------------------------------------------------------------------
