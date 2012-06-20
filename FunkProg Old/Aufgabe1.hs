module Aufgabe1 where

{-
Schreiben Sie eine Haskell-Rechenvorschrift convert mit der Signatur 
convert :: Integer -> [Integer], die eine ganze Zahl in die Folge ihrer 
Ziffern zerlegt. Die folgenden Beispiele illustrieren das Ein-/Ausgabeverhalten: 
convert 2010 => [2,0,1,0], 
convert 185161 => [1,8,5,1,6,1]
-}
reverseConvert :: Integer -> [Integer]
reverseConvert 0 = []
reverseConvert x = (x `mod` 10) : reverseConvert(x `div` 10)

convert :: Integer -> [Integer]
convert 0 = [0]
convert x = reverse $ reverseConvert x

{-
Implementieren Sie mithilfe der Funktion convert eine Haskell-Rechenvorschrift
quersumme mit der Signatur quersumme :: Integer -> Integer, die die Quersumme
der Argumentzahl berechnet. Die Aufrufe quersumme 2010 und quersumme
185161 sollen also die Resultate 3 und 22 liefern.
-}
quersumme :: Integer -> Integer
quersumme x = sum(convert x)

{-
Benutzen Sie die Rechenvorschriften convert und quersumme, um folgende Teilbarkeitstests
von Zahlen zu implementieren. Sie können dabei davon ausgehen, dass alle
Funktionen, einschließlich convert und quersumme nur auf positive Zahlen größer oder
gleich 0 angewendet werden.
-}

{-
3. Schreiben Sie eine Haskell-Rechenvorschrift dreiTeilbar mit der Signatur
dreiTeilbar :: Integer -> Bool, die feststellt, ob die Argumentzahl ohne Rest
durch 3 teilbar ist. Die Implementierung von dreiTeilbar soll dabei ausnutzen, dass
eine Zahl genau dann durch 3 teilbar ist, wenn ihre Quersumme durch 3 teilbar ist.
Stützen Sie die Implementierung von dreiTeilbar auf convert und quersumme ab.
-}
dreiTeilbar :: Integer -> Bool 
dreiTeilbar x = quersumme(x) `mod` 3 == 0

{-
4. Schreiben Sie eine Haskell-Rechenvorschrift sechsTeilbar mit der Signatur
sechsTeilbar :: Integer -> Bool, die feststellt, ob die Argumentzahl ohne Rest
durch 6 teilbar ist. Die Implementierung von sechsTeilbar soll ausnutzen, dass eine
Zahl genau dann durch 6 teilbar ist, wenn sie jeweils ohne Rest durch 2 und durch 3
teilbar ist.
-}
sechsTeilbar :: Integer -> Bool
sechsTeilbar x = dreiTeilbar(x) && x `mod` 2 == 0

{-
5. Schreiben Sie eine Haskell-Rechenvorschrift elfTeilbar mit der Signatur elfTeilbar
:: Integer -> Bool, die feststellt, ob die Argumentzahl ohne Rest durch 11 teilbar
ist. Die Implementierung von elfTeilbar soll ausnutzen, dass eine Zahl genau dann
durch 11 teilbar ist, wenn die Summe ihrer Ziffern mit jeweils wechselndem Vorzeichen
durch 11 teilbar ist. Folgendes Beispiel illustriert die Berechnungsidee: 56.518 ist
durch 11 teilbar, weil folgende Ziffernsumme durch 11 teilbar ist: 8-1+5-6+5 = 11
-}
count :: [Integer] -> Integer
count [] = 0
count x = head(x) + negate(count(tail(x)))

elfTeilbar :: Integer -> Bool
elfTeilbar x = count(reverseConvert(x)) `mod` 11 == 0

every n xs = 
	case drop (n-1) xs of
        (y:ys) -> y : every n ys
        [] -> []