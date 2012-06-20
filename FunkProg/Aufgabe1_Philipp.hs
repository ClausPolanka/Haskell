-- 
-- Aufgabe 1, Gruppe 262, Polanka Claus und Rinner Philipp
-- 

import Prelude
--import Data.List -- doch nicht benötigt ?

pick :: Integer -> [Integer] -> [Integer]

-- Eingabe: Zahl, Liste von Zahlen
-- Ergebnis: die Liste, in der alle Vorkommnisse von Zahl entfernt wurden

pick _ [] = []
pick zahl (x:xs)
        | x==zahl       = [zahl] ++ (pick zahl xs)
        | otherwise     = (pick zahl xs)

-- Eingabe: 2 Listen, die erste enthält die Liste der Zahlen, die ausgegeben werden soll,
-- und zwar genau dann, wenn sie in der ersten Liste enthalten sind

pickAll :: [Integer] -> [Integer] -> [Integer]
pickAll [] _ = []
pickAll _ [] = []

--alte Ideen, funktioniert so nicht
--pickAll zahlen liste = concat ([ x | x::Integer , elem x zahlen, elem (head zahlen) liste ] ) ++ (pickAll (tail zahlen) liste)
--pickAll zahlen liste = concat ([ pickAll (tail zahlen) liste | elem (head zahlen) liste ] )

pickAll zahlen liste = [ x | x<-liste, elem x zahlen ]

-- Eingabe: 2 Zahlen. Mathematisch verstehe ich zwar nicht ganz, was wir hier eigentlich machen, 
-- aber einfach die Formel vom Übungszettel umzusetzen war nicht sonderlich schwer
-- wir nehmen die vorletzte Form, dadurch brauchen wir keine eigene Funktion für den Binomialkoeffizienten, 
-- sondern nur eine Kombination verschiedener Faktorieller

variations :: Integer -> Integer -> Integer
variations n r
        | (n >= r) && (r >= 0)    = div (fak n) (fak (n-r))
        | otherwise               = -1
        
fak :: Integer -> Integer
fak 0 = 1
fak n = n * (fak (n-1) )

-- nicht mehr notwendig, aber ich lass es erstmal stehen. Ungetestet, könnte falsch sein
-- binom :: Integer -> Integer -> Integer
-- binom n k
--        | k==0          = 1
--        | n==k          = 1
--        | otherwise     = ( (binom n-1 k-1) + (binom n-1 k) )

type Symbol = Char
type Text = String
type NumberOf = Integer

-- Zählt die Vorkommnisse von eines Zeichens im Text, +1 falls vorhanden, andernfalls neuer rekursiver Aufruf

numberOfOcc :: Symbol -> Text -> NumberOf
numberOfOcc _ [] = 0
numberOfOcc sym (t:ts)
        | sym==t        = 1 + numberOfOcc sym ts
        | otherwise     = numberOfOcc sym ts


-- FIXME also das muss doch auch einfacher gehn, aber zumindest scheint es zu funktionieren:

-- verschiedene Überlegungen: das häufigste Symbol ist dasjenige, das das eindeutige Maximum ist, also das 
-- (1) am häufigsten vorkommt, (2) UND dabei diesen Platz nicht mit einem anderen Symbol teilt
-- also: 4 entscheidende Schritte:
-- zähle alle Zeichen
-- bestimme, _wir oft_ das häufigste Zeichen vorkommt
-- schau, ob ein anderes Zeichen genau so oft vorkommt
-- wenn genau einmal, dann gültiges eindeutiges Maximum

mostCommonSymbol :: Text -> Symbol
mostCommonSymbol [] = error "kein Resultat"
mostCommonSymbol text = head ( [ x | x<-text , (numberOfOcc x text) == (eindeutigesMaximum text) ] )

eindeutigesMaximum :: Text -> Integer
eindeutigesMaximum text = bestimmeMaximum (countCharactersListe text)

countCharactersListe :: Text -> [Integer]
countCharactersListe [] = []
countCharactersListe (x:xs) = [numberOfOcc x (x:xs)] ++ countCharactersListe xs

bestimmeMaximum :: [Integer] -> Integer
bestimmeMaximum liste
  | ((counter (maximum liste) liste ) == 1) = maximum liste
  | otherwise                               = error "kein Resultat"
  
counter :: Integer -> [Integer] -> Integer
counter _ [] = 0
counter a b 
        | a==(head b)   = 1 + counter a (tail b)
        | otherwise     = counter a (tail b)
        
-- prinzipiell könnte ich das sicher auch in weniger Funktionen zusammenfassen, aber ob es dadurch einfacher wird, bezweifle ich

