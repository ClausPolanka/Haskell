--------------------------------------------------------------------------------
-- 2. Aufgabenblatt zu Funktionale Programmierung vom 12.10.2010
-- 
-- Gruppe: 145
-- Mitglieder: Felix MAYERHUBER (0825283) und Dominik RAUCH (0825084)
-- 
-- Created: 12.10.2010
--------------------------------------------------------------------------------

import Prelude
import Text.Printf

--------------------------------------------------------------------------------
-- 2.1 diffFolge
-- Liefert die Differenzwertfolge von m und n bis zum ersten Wert <= 0
--------------------------------------------------------------------------------

diffFolge :: (Integer, Integer) -> [Integer]
diffFolge (m, n)
	| m > 0 = m : diffFolge (m-n, n)
	| otherwise = [m]
	
--------------------------------------------------------------------------------
-- 2.2 teilt
-- True falls m durch n ohne Rest teilbar ist, False anderenfalls
--------------------------------------------------------------------------------

teilt :: (Integer, Integer) -> Bool
--teilt (m, n) = last (diffFolge (m, n)) == 0
teilt (m, n) = last (diffFolge (abs m, n)) == 0

--------------------------------------------------------------------------------
-- 2.3 zahlenBlock
-- Zerlegt eine ganze Zahl in Dreierfolgen seiner Ziffern (Rückgabe der
-- einzelnen Blöcke als Zahl)
--------------------------------------------------------------------------------

zahlenBlock :: Integer -> [Integer]
zahlenBlock m
	| m > 0 = mod m 1000 : zahlenBlock (div m 1000)
	| otherwise = []

--------------------------------------------------------------------------------
-- 2.4 zeichenreihenBlock
-- Zerlegt eine ganze Zahl in Dreierfolgen seiner Ziffern (Rückgabe der
-- einzelnen Blöcke als String)
--------------------------------------------------------------------------------

-- Mit Hilfe der Standard-Bibliothek:
-- zeichenreihenBlock :: Integer -> [String]
-- zeichenreihenBlock m = map (Text.Printf.printf "%03d") (zahlenBlock m)

zeichenreihenBlock :: Integer -> [String]
zeichenreihenBlock m
	| m > 999 = zeichenreihenBlock(mod m 1000) ++ zeichenreihenBlock(div m 1000)
    -- | m > 999 = [show (mod m 1000)] ++ zeichenreihenBlock(div m 1000)
	| m < 10 = ["00" ++ show m]
	| m < 100 = ["0" ++ show m]
	| otherwise = [show m]

--------------------------------------------------------------------------------
-- 2.5 siebenTeilbar
-- True falls die Zahl ohne Rest durch sieben teilbar ist, False anderenfalls
--------------------------------------------------------------------------------

siebenTeilbar :: Integer -> Bool
siebenTeilbar m = teilt (as (zahlenBlock m), 7)

as :: [Integer] -> Integer
as [] = 0
as (x:xs) = x + as2 xs

as2 :: [Integer] -> Integer
as2 [] = 0
as2 (x:xs) = (-x) + as xs

--------------------------------------------------------------------------------
