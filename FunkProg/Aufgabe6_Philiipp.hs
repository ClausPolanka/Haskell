data DOrd = Infix | Praefix | Postfix | GInfix | GPraefix | GPostfix

data BTree = Nil | BNode Int BTree BTree deriving (Eq,Ord,Show)


-- BEISPIEL 1

flatten :: BTree -> DOrd -> [Int]
flatten (Nil) _ = []
flatten (BNode wurzel links rechts) Infix    = (flatten links Infix) ++ [wurzel] ++ (flatten rechts Infix) 
flatten (BNode wurzel links rechts) Praefix  = [wurzel] ++ (flatten links Praefix) ++ (flatten rechts Praefix)
flatten (BNode wurzel links rechts) Postfix  = (flatten links Postfix) ++ (flatten rechts Postfix) ++ [wurzel]
flatten (BNode wurzel links rechts) GInfix   = (flatten rechts GInfix) ++ [wurzel] ++ (flatten links GInfix)
flatten (BNode wurzel links rechts) GPraefix = (flatten rechts GPraefix) ++ (flatten links GPraefix) ++ [wurzel]
flatten (BNode wurzel links rechts) GPostfix = [wurzel] ++ (flatten rechts GPostfix) ++ (flatten links GPostfix)


-- ich finde, es wäre so schöner, geht aber eben nur iff DOrd deriving Eq:
--
-- flatten (BNode wurzel links rechts) order
--        | order == Infix    = (flatten links Infix) ++ [wurzel] ++ (flatten rechts Infix)
--        | order == Praefix  = [wurzel] ++ (flatten links Praefix) ++ (flatten rechts Praefix)
--        usw






-- BEISPIEL 2

-- ein Suchbaum liegt dann vor, wenn alle Knoten im linken Teilbaum echt kleiner
-- und alle Knoten im rechten Teilbaum größer als die Wurzel sind

isST :: BTree -> Bool
isST Nil = True
isST (BNode wurzel links rechts)
        | linksechtkleinerals && rechtsechtgroesserals = (isST links) && (isST rechts)
        | otherwise                                    = False
        where
                linksliste =  (flatten links Infix)
                rechtsliste = (flatten rechts Infix)
                linksechtkleinerals   = vergleiche (<) linksliste wurzel
                rechtsechtgroesserals = vergleiche (>) rechtsliste wurzel

vergleiche :: (Int->Int->Bool) -> [Int] -> Int -> Bool
vergleiche _ [] _ = True
vergleiche func (x:xs) n
        | (func x n)    = vergleiche func xs n
        | otherwise     = False


-- zuerst hatte ich das so, aber dann hab ich es auf diese schönere funktionalere Art "vergleiche" umgeschrieben,
-- bei der die Vergleichsoperation als Funktion übergeben wird
--
--  | (linksliste `kleinerals` wurzel) && (rechtsliste `groesserals` wurzel) = (isST links) && (isST rechts)
--kleinerals :: [Int] -> Int -> Bool
--kleinerals [] _ = True
--kleinerals (x:xs) a
--        | (x<a)     = (kleinerals xs a)
--        | otherwise = False
--groesserals ditto
--gut möglich, dass es für so etwas eine Library-Funktion gibt, ich hab nicht extra nachgeschaut
