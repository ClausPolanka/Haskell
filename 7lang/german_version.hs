module Main where

    double x = x + x

-- (Num a) => a -> a
-- a entspricht dabei einer Typvariable.

    factorial :: Integer -> Integer
    factorial 0 = 1
    factorial x = x * factorial (x - 1)

    factorialGuarded :: Integer -> Integer
    factorialGuarded x 
        | x > 1 = x * factorialGuarded (x - 1)
        | otherwise = 1

--filter fold foldr