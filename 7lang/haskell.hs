module Main where

    doub :: Integer -> Integer
    doub x = x + x
    
    factorial :: Integer -> Integer
    factorial 0 = 1
    factorial x = x * factorial (x - 1)
    
    factorial' :: Integer -> Integer
    factorial' x
        | x > 1 = x * factorial' (x - 1)
        | otherwise = 1
     
    -- Inefficient
    fib :: Integer -> Integer
    fib 0 = 1
    fib 1 = 1
    fib x = fib (x - 1) + fib (x - 2)
    
    -- More efficient (answer in the first position)
    fibTuple :: (Integer, Integer, Integer) -> (Integer, Integer, Integer)
    fibTuple (x, y, 0) = (x, y, 0)
    fibTuple (x, y, index) = fibTuple (y, x + y, index - 1)
    
    fibResult :: (Integer, Integer, Integer) -> Integer
    fibResult (x, y, z) = x
    
    -- Simplify
    fib':: Integer -> Integer
    fib' x = fibResult (fibTuple (0, 1, x))
    
    -- Chaining
    second = head . tail -- => second x = head (tail x)
    
    fibNextPair :: (Integer, Integer) -> (Integer, Integer)
    fibNextPair (x, y) = (y, x + y)
    
    fibNthPair :: Integer -> (Integer, Integer)
    fibNthPair 1 = (1, 1)
    fibNthPair n = fibNextPair (fibNthPair (n - 1))
    
    fib'' :: Integer -> Integer
    fib'' = fst . fibNthPair
    
    -- Lists
    size[] = 0
    size (h:t) = 1 + size t
    
    prod [] = 1
    prod (h:t) = h * prod t
    
    -- Generating Lists - Recursions
    
    -- let h:t = [1, 2, 3]
    -- 1:[2, 3]
    -- Doesn't work: [1]:[2, 3]
    -- [1]:[]
    -- [1]:[[2, 3], [4, 5, 6]]

    allEven :: [Integer] -> [Integer]
    allEven [] = []
    allEven (h:t) = if even h then h:allEven t else allEven t
    
    -- Ranges and Composition
    -- [1..2]
    -- [1..4]
    -- [10, 8 .. 4]
    -- [10, 9.5 .. 4]
    -- take 5 [1 ..]
    -- take 5 [0, 2 ..]
    
    -- List Comprehensions
    -- [x * 2 | x <- [1, 2, 3]] // Collect x * 2 where x is taken from the list [1, 2, 3]
    -- [ (y, x) | (x, y) <-  [(1, 2), (2, 3), (3, 1)]]
    
    -- let crew = ["Kirk", "Spock", "McCoy"]
    -- [(a, b) | a <- crew, b <- crew]
    
    -- [(a, b) | a <- crew, b <- crew, a /= b] // remove duplicates
    
    -- [(a, b) | a <- crew, b <- crew, a < b] // sorted order
    
    -- Higher order functions
    squareAll list = map square list
        where square x = x * x
        
    -- Lazy evaluation
    myRange start step = start:(myRange (start + step) step)
    
    lazyFib x y = x:(lazyFib y (x + y))
    
    fib''' = lazyFib 1 1
    
    fibNth x = head (drop (x - 1) (take (x) fib'''))
    
    
    -- User-Defined Types
    
    -- Type Constructors
    data Boolean = True | False
    
    data Suit = Spades | Hearts deriving (Show)
    data Rank = Ten | Jack | Queen | King | Ace deriving (Show)
    
    -- Alias Types
    type Card = (Rank, Suit)
    type Hand = [Card]
    
    value :: Rank -> Integer
    value Ten = 1
    value Jack = 2
    value Queen = 3
    value King = 4
    value Ace = 5
    
    cardValue :: Card -> Integer
    cardValue (rank, suit) = value rank
    
    -- Polymorphic Data Type
    data Triplet a = Trio a a a deriving (Show) -- Trio is a data constructor
    
    -- Recursive Data Types
    data Tree a = Children [Tree a] | Leaf a deriving (Show)
    -- Type Constructor => Tree
    -- Data Constructor => Children, Leaf
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    