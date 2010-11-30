-- Berechnung von Volumen (V) und Fläche (F) einer Kugel

-- Mittels lokaler Deklarationen

-- where-Konstrukt
kVA :: Float -> (Float, Float)
kVA r = 
	((4 / 3) * myPi * rcube r, 4 * myPi + square r)
	where
		myPi = 3.14
		rcube x = x * square x
		square x = x * x

-- let-Konstrukt
kVA1 :: Float -> (Float, Float)
kVA1 r = 
	let
		myPi = 3.14
		rcube x = x * square x
		square x = x * x
	in
		((4 / 3) * myPi * rcube r, 4 * myPi + square r)
		
-- in einer Zeile
kVA2 :: Float -> (Float, Float)
kVA2 r = 
	((4 / 3) * myPi * rcube r, 4 * myPi + square r)
	where
		myPi = 3.14; rcube x = x * square x; square x = x * x
		
kVA3 :: Float -> (Float, Float)
kVA3 r = 
	let myPi = 3.14; rcube x = x * square x; square x = x * x
	in
		((4 / 3) * myPi * rcube r, 4 * myPi + square r)
	