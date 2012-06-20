module Aufgabe3 ( anp1, anp2, transp, sp ) where

type Matrix = [[Integer]]
type Zeilen = Integer
type Spalten = Integer
type Fuellwert = Integer
type Laenge = Integer

-- Beispiel 1
anp1 :: [[Integer]] -> Matrix
anp1 [] = [[1]]
anp1 compLists = [x ++ replicate (maxLength - length x) 0 | x <- compLists]
    where maxLength = maximum (map length compLists)

-- Beispiel 2
anp2 :: [[Integer]] -> Zeilen -> Spalten -> Fuellwert -> Matrix
anp2 _ 0 _ _ = []
anp2 _ _ 0 _ = []
anp2 l z s w 
    | z < 0 || s < 0 = error "unzulaessig"
    | l == [] = replicate (fromIntegral z) componentListTemplate
    | length l < (fromInteger z) = extended_L
    | otherwise = cropped_L
        where
            componentListTemplate = (replicate (fromIntegral s) w)
            extended_L = customizedComponentLists ++ requiredListsFilledWithValue
            cropped_L = take (fromInteger z) customizedComponentLists
            customizedComponentLists = [cropOrExtendCols x | x <- l]
                where 
                    cropOrExtendCols x
                        | length x < (fromIntegral s) = x ++ replicate ((fromIntegral s) - length x) w
                        | otherwise = take (fromIntegral s) x
            requiredListsFilledWithValue = (replicate ((fromIntegral z) - (length l)) componentListTemplate)

-- Beispiel 3
transp :: [[Integer]] -> Zeilen -> Spalten -> Fuellwert -> Matrix
transp l z s w
    | z < 0 || s < 0 = error "unzulaessig"
    | l == [] = anp2 l s z w
    | otherwise = anp2 (buildTransponierteMatrixWithIndex 0) s z w
        where buildTransponierteMatrixWithIndex i
                | i < (fromIntegral z) = [x !! i | x <- (anp2 l z s w)] : buildTransponierteMatrixWithIndex (i + 1)
                | otherwise = []

-- Beispiel 4
sp :: [[Integer]] -> [[Integer]] -> Laenge -> Fuellwert -> Integer
sp l1 l2 vl w
    | vl < 0 = error "unzulaessig"
    | otherwise = sum [(rowVector !! element) * (columnVector !! element) | element <- [0..(fromIntegral vl)-1]]
        where 
            rowVector = [x | x <- (anp2 l1 1 vl w) !! 0]
            columnVector = [ y !! 0 | y <- (anp2 l2 vl 1 w)]
