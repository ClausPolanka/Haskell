module Aufgabe6 where

data DOrd = Infix | Praefix | Postfix | GInfix | GPraefix | GPostfix
data BTree = Nil | BNode Int BTree BTree deriving (Eq, Ord, Show)

-- Beispiel 1
flatten :: BTree -> DOrd -> [Int]
flatten Nil _ = []


-- Beispiel 2
isST :: BTree -> Bool
isST Nil = False

-- Beispiel 3
type Control = String
type Func = Integer -> Integer
type Data = Integer
data Tree = Leaf Func | Node Func Tree Tree Tree

mkControl :: String -> Control
mkControl s = [x | x <- s, x == 'l' || x == 'm' || x == 'r']

apply :: Control -> Data -> Tree -> Integer
apply c d (Leaf f) = f d
apply [] d (Node f _ _ _) = f d
apply c d (Node f t1 t2 t3)
    | (head $ mkControl c) == 'l' = apply (drop 1 c) (f d) t1 
    | (head $ mkControl c) == 'm' = apply (drop 1 c) (f d) t2
    | (head $ mkControl c) == 'r' = apply (drop 1 c) (f d) t3

-- Beispiel 4
data LTree = LNode Integer [LTree] deriving (Ord, Show, Eq)

mapLT :: Func -> LTree -> LTree
mapLT f (LNode v []) = (LNode (f v) [])
mapLT f (LNode v t) = (LNode (f v) (execFunction f t))

execFunction :: Func -> [LTree] -> [LTree]
execFunction _ [] = []
execFunction f ((LNode v xs):ts) = (LNode (f v) (execFunction f xs)) : execFunction f ts