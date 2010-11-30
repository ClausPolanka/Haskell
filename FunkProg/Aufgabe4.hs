import Prelude hiding (Ord)

data Ord = BottomUp | TopDown

type Layer = [Integer]

data Tree = Leaf Integer |
            Node Integer Tree Tree

-- writeLayer (Node 0 (Leaf 1) (Leaf 2)) TopDown
writeLayer :: Tree -> Ord -> [Layer]
writeLayer (Leaf n) _ = [[n]]
writeLayer (Node n tr1 tr2) ord = [n] : (writeLayer tr1 ord) ++ (writeLayer tr2 ord)

flatten :: [[a]] -> [a]
flatten = foldl (++) []

-- Egebnis => [[0],[1, 2]]

-- levels' (Node a bs) = [a] : foldr (zipWith' (++)) [] (map levels' bs)

-- zipWith' f xs [] = xs
-- zipWith' f [] xs = xs
-- zipWith' f (x:xs) (y:ys) = f x y : zipWith' f xs ys

bylevel :: Tree a -> [[a]]
bylevel t = step [t]
    where step [] = []
          step ts = map element ts : step (concatMap subtrees ts)
