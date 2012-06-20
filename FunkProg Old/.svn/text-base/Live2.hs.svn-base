import Data.Char

l1 :: Integer -> Integer
l1 x = let
            f :: Integer -> Integer
            f y = y + 3 + x -- 'x' auch hier verwendbar
            g :: Integer
            g = 1337
        in
            f (g + x)

w1 :: Integer -> Integer
w1 x = f (g + x)
    where
        f :: Integer -> Integer
        f y = y + 3 -- 'x' auch hier verwendbar
        g :: Integer
        g = 1337
        
g1 :: Integer -> String
g1 x = if x < 0 then
            "negative"
        else
            if x < 10 then
                "kleiner zehn"
            else
                "groesser gleich zehn"
                
g2 :: Integer -> String
g2 777 = "777"
g2 x -- Reihenfolge beachten!
    | x < 0 = "negativ"
    | x < 10 = "kleiner zehn"
    | otherwise = "groesser gleich zehn"
    
t1 :: (Integer, Integer) -> Integer
t1 x = (fst x) + (snd x)

t2 :: (Integer, Integer) -> Integer
t2 (x, y) = x + y

myChar :: Integer -> Char
myChar x = chr (fromInteger x)

-- Licht, ehr Licht!
type Lampe = Bool

durchschalten :: Lampe -> [Bool] -> Lampe
durchschalten akt [] = akt
durchschalten akt (x:xs)
    | x = durchschalten (not akt) xs
    | otherwise = durchschalten akt xs
    
switch :: Integer -> Lampe
switch n = head (reverse (switch' 1))
    where 
        switch' :: Integer -> [Lampe]
        switch' pos
            | (pos - 1) == n = []
            | otherwise = lampe_pos : (switch' (pos + 1))
                where 
                    lampe_pos :: Lampe
                    lampe_pos = durchschalten False durchgaenge
                    durchgaenge :: [Bool]
                    durchgaenge = [pos `mod` i == 0 | i <- [1..n]]
                    
licht :: Integer -> String
licht n
    | t = "aus"
    | otherwise = "an"
    where
        t = ((length [x | x <- [1..n], n `mod` x == 0]) `mod` 2) == 0
                