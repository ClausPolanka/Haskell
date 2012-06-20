-- Achtung: die finale Version folgt demnaechst ;-)

eins :: Integer
eins = 1

addiere :: Integer -> Integer -> Integer
addiere x y = x + y

addiere_fuenf :: Integer -> Integer
addiere_fuenf x = addiere 5 x

ist1 :: Integer -> Bool
ist1 1 = True -- reihenfolge beachten! (pattern matching)
ist1 x = False

my_head :: [Integer] -> Integer
my_head [] = 0
my_head (x:[]) = x + 1 -- reihenfolge beachten! (pattern matching)
my_head (x:xs) = x

laf1 :: [Integer] -> [Integer] -- list_addiere_fuenf
laf1 [] = []
laf1 (x:xs) = (addiere_fuenf x):(laf1 xs)

laf2 :: [Integer] -> [Integer]
laf2 l = [ addiere_fuenf x | x<-l, x > 10] -- list comprehension

laf3 :: [Integer] -> [Integer]
laf3 l = map (addiere_fuenf) l -- map power

-- visit tryhaskell.org
