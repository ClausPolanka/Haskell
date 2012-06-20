import Data.List

-- Aufgabe 1
pick :: Integer -> [Integer] -> [Integer]
pick n l = [x | x <- l, x == n]

-- Aufgabe 2
pickAll :: [Integer] -> [Integer] -> [Integer]
pickAll _ [] = []
pickAll [] _ = []
pickAll l1 (x:xs) = (pick x l1) ++ pickAll l1 xs

-- Aufgabe 3
variations :: Integer -> Integer -> Integer
variations n r
    | n < r || r < 0 = -1
    | otherwise = product . take (fromIntegral r) $ [n, n-1..0]

-- Aufgabe 4
type Symbol = Char
type Text = String
type NumberOf = Int

numberOfOcc :: Symbol -> Text -> NumberOf
numberOfOcc s t = length [x | x <- t, x == s]

-- Aufgabe 5
mostCommonSymbol :: Text -> Symbol
mostCommonSymbol [] = error "kein Resultat"
mostCommonSymbol t
    | countOfFirstSymbol > countOfSecondSymbol = mostCommonSymb 
    | otherwise = error "kein Resultat"
        where 
          symbols = sortedByMax t
          countOfFirstSymbol = snd (symbols !! 0)
          countOfSecondSymbol = snd (symbols !! 1)
          mostCommonSymb = fst (symbols !! 0)

sortedByMax :: Text -> [(Symbol, NumberOf)]
sortedByMax t = reverse . nub . sortedByOcc . countedSymbols $ t

countedSymbols :: Text -> [(Symbol, NumberOf)]
countedSymbols t = [(x, (numberOfOcc x t)) | x <- t]

sortedByOcc :: [(Symbol, NumberOf)] -> [(Symbol, NumberOf)]
sortedByOcc symbols = sortBy (\ x y -> compare (snd x) (snd y)) symbols