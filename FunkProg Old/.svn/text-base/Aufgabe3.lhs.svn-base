Aufgabe 3.1 Zykluslaenge von n

> dreiNplusEins :: Integer -> [Integer]
> dreiNplusEins n | n<=0 = []
>		  | n==1 = [n]
>		  | even(n) = n : dreiNplusEins(n `div` 2)
>		  | otherwise = n : dreiNplusEins((n * 3)+1)

dreiNplusEins :: Integer -> [Integer]
dreiNplusEins n = addToList [n]

addToList :: [Integer] -> [Integer]
addToList x
  | (last x) == 1 = x
  | (last x) `mod` 2 == 0 = addToList $ x ++ last x `div` 2 : []
  | otherwise = addToList $ x ++ last x * 3 + 1 : []

Aufgabe 3.2 Bestimmen der maximalen Zykluslaenge

> type UntereGrenze = Integer
> type ObereGrenze = Integer
> type MaxZykLaenge = Integer

> maxZyklus :: UntereGrenze -> ObereGrenze -> (UntereGrenze, ObereGrenze, MaxZykLaenge)
> maxZyklus m n
>   | m > n || n >= 100000 = (m, n, 0)
> maxZyklus m n = (m, n, max)
>   where max = fromIntegral $ maximum $ map length $ map dreiNplusEins [m..n]

Aufgabe 3.3 Wahrheitstabelle - Bestimmen aller Nachbarn mit Wert True

> anzNachbarn :: [[Bool]] -> (Integer, Integer) -> Integer
> anzNachbarn x (n, m)
>   | n < 0 || n > fromIntegral (length x) - 1 = -1
>   | m < 0 || m > fromIntegral (length (x !! fromIntegral n) -1) = -1
> anzNachbarn x (n, m) = linkerNachbar + rechterNachbar + obereNachbarn + untereNachbarn
>   where linkerNachbar
>           | m >= 1 && (x !! fromIntegral n) !! (fromIntegral m - 1) = 1
>           | otherwise = 0
>         rechterNachbar
>           | m < fromIntegral (length (x !! fromIntegral n)) - 1 && (x !! fromIntegral n) !! (fromIntegral m + 1) = 1
>           | otherwise = 0
>         obereNachbarn = obenLinks + obenMitte + obenRechts
>           where obenLinks 
>                       | (n - 1) >= 0 && m >= 1 && (x !! fromIntegral (n - 1)) !! (fromIntegral m - 1) = 1
>                       | otherwise = 0
>                 obenMitte 
>                       | (n - 1) >= 0 && x !! ((fromIntegral n) - 1) !! fromIntegral m = 1
>                       | otherwise = 0
>                 obenRechts 
>                       | (n - 1) >= 0 && m < fromIntegral (length (x !! fromIntegral (n - 1))) - 1 && (x !! fromIntegral (n - 1)) !! (fromIntegral m + 1) = 1
>                       | otherwise = 0
>         untereNachbarn = untenLinks + untenMitte + untenRechts
>           where untenLinks 
>                       | (n + 1) <= fromIntegral (length x) - 1 && m >= 1 && (x !! fromIntegral (n + 1)) !! (fromIntegral m - 1) = 1
>                       | otherwise = 0
>                 untenMitte 
>                       | (n + 1) <= fromIntegral (length x) - 1 && x !! ((fromIntegral n) + 1) !! fromIntegral m = 1
>                       | otherwise = 0
>                 untenRechts
>                       | (n + 1) <= fromIntegral (length x) - 1 && m < fromIntegral (length (x !! fromIntegral (n + 1))) - 1 && (x !! fromIntegral (n + 1)) !! (fromIntegral m + 1) = 1
>                       | otherwise = 0

4. Schreiben Sie eine Haskell-Rechenvorschrift transform mit der Signatur transform :: [[Bool]]
-> [[Integer]], die angewendet auf eine M × N-Wahrheitswertmatrix eine M × N-Matrix ganzer
Zahlen ausgibt, wobei jede Komponente der Ergebnismatrix die Anzahl der mit True benannten
Nachbarfelder der entsprechenden Komponente der Argumentmatrix angibt. Angewendet
auf [[True,False,False],[True,False,False],[False,True,False]] liefert die Funktion
transform die Ergebnismatrix [[1,2,0],[2,3,1],[2,1,1]].

> transform :: [[Bool]] -> [[Integer]]
> transform mn = [transformRow mn (toInteger x) | x <- [0..((length mn)-1)]]

> transformRow :: [[Bool]] -> Integer -> [Integer]
> transformRow mn m = [anzNachbarn mn (m,toInteger(n)) | n <- [0..((length ((!!) mn (fromInteger m)))-1)]]
