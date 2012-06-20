module Aufgabe2 where
{-
+-------------------------------------------------------+
|		      Aufgabenblatt 2			|
|			18.10.2010			        |
|           							|
|		Kamil Sierzant - 0526973		|
|		Claus Polanka  - 0225648		|
+-------------------------------------------------------+
-}

{-1. Schreiben Sie eine Haskell-Rechenvorschrift diffFolge mit der Signatur
diffFolge :: (Integer,Integer) -> [Integer], die angewendet auf das Argumentpaar (m, n),
m, n > 0, die (Anfangs-) Folge der Differenzwerte von m und n liefert. Das erste Element der
Resultatliste ist dabei m, das vorletzte Element stets echt gr¨oßer als 0 und das letzte Element
entweder 0 oder ein Wert echt kleiner als 0.-}

diffFolge :: (Integer,Integer) -> [Integer]
diffFolge (m,n)
    | m <= 0 = [m]
    | otherwise = m : diffFolge(m-n, n)


{-2. Schreiben Sie eine Haskell-Rechenvorschrift teilt mit der Signatur teilt :: (Integer,Integer)
-> Bool, die angewendet auf das Argumentpaar (m, n), m, n > 0, angibt, ob m ohne Rest durch
n teilbar ist. St¨utzen Sie die Implementierung der Funktion teilt auf die Funktion diffFolge ab,
d.h. teilt ruft diffFolge auf.-}

teilt :: (Integer, Integer) -> Bool
teilt (x, y) = 0 `elem` diffFolge (x, y)

{-
teilt :: (Integer,Integer)-> Bool
teilt (m,n) = product(diffFolge(m,n)) == 0
-}

{-3. Schreiben Sie eine Haskell-Rechenvorschrift zahlenBlock mit der Signatur
zahlenBlock :: Integer -> [Integer], die angewendet auf ein Argument m, m > 0, die Dreierfolge
vonmin Form einer Liste ganzer Zahlen ausgibt. Angewendet auf 24.889.375 liefert zahlenBlock
also die Resultatliste [375,889,24].-}

zahlenBlock :: Integer -> [Integer]
zahlenBlock 0 = []
zahlenBlock x = (x `mod` 1000) : zahlenBlock(x `div` 1000)


{-4. Schreiben Sie analog zur Rechenvorschrift zahlenBlock eine Haskell-Rechenvorschrift
zeichenreihenBlock mit der Signatur zeichenreihenBlock :: Integer -> [String], die angewendet
auf ein Argument m, m > 0, die Dreierfolge von m in Form einer Liste von Zeichenreichen
jeweils der L¨ange 3 ausgibt. Angewendet auf 24.889.375 liefert zeichenreihenBlock also die Resultatliste
["375","889","024"].-}

pad char len str = replicate (len - length str) char ++ str

zeichenreihenBlock :: Integer -> [String]
zeichenreihenBlock = map (pad '0' 3 . show) . zahlenBlock

{-
zeichenreihenBlock :: Integer -> [String]
zeichenreihenBlock x = toStringArray(zahlenBlock(x))

toStringArray :: [Integer] -> [String]
toStringArray [] = []
toStringArray x = fillNull(show(head(x))) : toStringArray(tail(x))

fillNull :: String -> String
fillNull x | length(x)==3 = x
	   | otherwise = fillNull("0" ++ x)
-}

{-5. Schreiben Sie eine Haskell-Rechenvorschrift siebenTeilbar mit der Signatur siebenTeilbar ::
Integer -> Bool, die feststellt, ob das Argument m, m > 0, angibt, ob m ohne Rest durch 7
teilbar ist. Schreiben Sie dazu eine Haskell-Rechenvorschrift as mit Signatur as :: [Integer]
-> Integer, die die alternierende Summe der Argumentliste berechnet. Angewendet auf die Liste
[375,889,24] liefert as also den Wert -490. St¨utzen Sie anschließend die Implementierung von
siebenTeilbar auf die Funktionen zahlenBlock, as und teilt ab. Beachten Sie dabei, dass teilt
nur f¨ur positive Argumente wohldefiniert ist und ¨uberlegen Sie sich deshalb eine L¨osung f¨ur den
Umgang mit m¨oglicherweise negativen Argumenten.-}
as :: [Integer] -> Integer
as [] = 0
as x = head (x) + negate (as (tail x))

siebenTeilbar :: Integer -> Bool
siebenTeilbar x = teilt (abs (as (zahlenBlock x)), 7)
