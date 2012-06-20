module Aufgabe2 ( istPrimal, faktorisiere ) where

-- Beispiel 1
istPrimal :: Integer -> Bool
istPrimal n
    | n `notElem` (p n) = False
    | otherwise = length (factorPairsFor n) == 0

p :: Integer -> [Integer]
p n = [1 + x * 4 | x <- [0..n]]

factorPairsFor :: Integer -> [(Integer, Integer)]            
factorPairsFor n = [(x, y) | x <- factorsFor n, y <- factorsFor n, x * y == n]

factorsFor :: Integer -> [Integer]
factorsFor n = [x | x <- (p n), n `mod` x == 0]

-- Beispiel 2
faktorisiere :: Integer -> [(Integer, Integer)]
faktorisiere n
    | istPrimal n = []
    | n `notElem` (p n) = error "Unzulaessig"
    | otherwise = factorPairsFor n

-- Beispiel 3
type Editor = String
type Suchzeichenreihe = String
type Index = Integer
type Vorkommen = Integer
type Alt = String
type Neu = String

suche :: Editor -> Suchzeichenreihe -> Index
suche e s = -1

-- Beispiel 4
sucheAlle :: Editor -> Suchzeichenreihe -> [Index]
sucheAlle e s = []

-- Beispiel 5
ersetze :: Editor -> Vorkommen -> Alt -> Neu -> Editor
ersetze e i _ _
    | i < 0 = e
ersetze e i s t = []