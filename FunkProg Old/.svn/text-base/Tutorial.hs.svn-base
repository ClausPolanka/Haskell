doubleMe x = x + x

doubleUs x y = doubleMe x + doubleMe y

doubleSmallNumber x = if x > 100 then x else x * 2
doubleSmallNumber' x = (if x > 100 then x else x * 2) + 1

claus = "Claus"

boomBang xs = [ if x < 10 then "Boom" else "Bang" | x <- xs, odd x]

length' xs = sum [ 1 | _ <- xs ]

length'' :: (Num b) => [a] -> b
length'' [] = 0
length'' (_:xs) = 1 + length'' xs

removeNoneUpperCase st = [ c | c <- st, c `elem` ['A'..'Z']]

factorial :: Integer -> Integer
factorial n = product [1..n]

lucky :: (Integral a) => a -> String
lucky 7 = "LUCKY NUMBER SEVEN!"
lucky x = "Sorry, you're out of luck, pal!"

factorial' :: (Integral a) => a -> a
factorial' 0 = 1
factorial' n = n * factorial' (n - 1)

addVectors :: (Num a) => (a, a) -> (a, a) -> (a, a)
addVectors a b = (fst a + fst b, snd a + snd b)

addVectors' :: (Num a) => (a, a) -> (a, a) -> (a, a)
addVectors' (x1, y1) (x2, y2) = (x1 + x2, y1 + y2)

first :: (a, b, c) -> a
first (x, _, _) = x

second :: (a, b, c) -> b
second (_, y, _) = y

third :: (a, b, c) -> c
third (_, _, z) = z

head' :: [a] -> a
head' [] = error "Can't call head on an empty list, dummy!"
head' (x:_) = x

tell :: (Show a) => [a] -> String
tell [] = "The list is empty"
tell (x:[]) = "The list has one element: " ++ show x
tell (x:y:[]) = "The list has two elements: " ++ show x ++ " and " ++ show y
tell (x:y:_) = "This list is long. The first two elements are: " ++ show x ++ " and " ++ show y

sum' :: (Num a) => [a] -> a
sum' [] = 0
sum' (x:xs) = x + sum' xs

capital :: String -> String
capital "" = "Empty string, whoops!"
capital all@(x:xs) = "The first letter of " ++ all ++ " is " ++ [x]

-- Guards and where bindings
myCompare :: (Ord a) => a -> a -> Ordering
a `myCompare` b
	| a > b = GT
	| a == b = EQ
	| otherwise = LT
	
bmiTell :: (RealFloat a) => a -> a -> String
bmiTell weight height
	| bmi <= 18.5 = "You're underweight, you emo, you!"
	| bmi <= 25.0 = "You're supposedly normal. Pffft, I bet you're ugly!"
	| bmi <= 30.0 = "You're fat! Lose some weight, fatty!"
	| otherwise = "You're a whale, congratulations!"
		where bmi = weight / height ^ 2
		
bmiTell' :: (RealFloat a) => a -> a -> String
bmiTell' weight height
	| bmi <= skinny = "You're underweight, you emo, you!"
	| bmi <= normal = "You're supposedly normal. Pffft, I bet you're ugly!"
	| bmi <= fat = "You're fat! Lose some weight, fatty!"
	| otherwise = "You're a whale, congratulations!"
    where bmi = weight / height ^ 2
          (skinny, normal, fat) = (18.5, 25.0, 30.0)
		  
initials :: String -> String -> String
initials firstname lastname = [f] ++ ". " ++ [l] ++ "."
    where (f:_) = firstname
          (l:_) = lastname
		  
calcBmis :: (RealFloat a) => [(a, a)] -> [a]
calcBmis xs = [bmi w h | (w, h) <- xs]
	where bmi weight height = weight / height ^ 2
	
-- let expressions
cylinder :: (RealFloat a) => a -> a -> a
cylinder r h =
    let sideArea = 2 * pi * r * h
        topArea = pi * r ^2
    in sideArea + 2 * topArea
    
calcBmis' :: (RealFloat a) => [(a, a)] -> [a]
calcBmis' xs = [ bmi | (w, h) <- xs, let bmi = w / h ^ 2 ]

-- Case expressions
head'' :: [a] -> a
head'' [] = error "No head for empty lists"
head'' (x:_) = x

head''' :: [a] -> a
head''' xs = case xs of [] -> error "No head for empts lists"
                        (x:_) -> x

describeList :: [a] -> String
describeList xs = "The list is " ++ case xs of [] -> "empty."
                                               [x] -> "a singleton list."
                                               xs -> "a longer list."
                                               
describeList' :: [a] -> String
describeList' xs = "The list is " ++ what xs
    where what [] = "empty."
          what [x] = "a singleton list."
          what xs = "a longer list."

-- Recursion
maximum' :: (Ord a) => [a] -> a
maximum' [] = error "maximum of empty list"
maximum' [x] = x
maximum' (x:xs)
    | x > maxTail = x
    | otherwise = maxTail
    where maxTail = maximum' xs

maximum'' :: (Ord a) => [a] -> a
maximum'' [] = error "maximum of empty list"
maximum'' [x] = x
maximum'' (x:xs) = max x (maximum'' xs)

replicate' :: (Num i, Ord i) => i -> a -> [a]
replicate' n x
    | n <= 0 = []
    | otherwise = x:replicate' (n - 1) x

take' :: (Num i, Ord i) => i -> [a] -> [a]
take' n _
    | n <= 0 = []
take' _ [] = []
take' n (x:xs) = x : take' (n - 1) xs
    
reverse' :: [a] -> [a]
reverse' [] = []
reverse' (x:xs) = reverse' xs ++ [x]

repeat' :: a -> [a]
repeat' x = x:repeat' x

zip' :: [a] -> [b] -> [(a, b)]
zip' [] _ = []
zip' _ [] = []
zip' (x:xs) (y:ys) = (x, y):zip' xs ys

elem' :: (Eq a) => a -> [a] -> Bool
elem' a [] = False
elem' a (x:xs) 
    | a == x = True 
    | otherwise = a `elem'` xs
    
quicksort :: (Ord a) => [a] -> [a]
quicksort [] = []
quicksort (x:xs) = 
    let smallerSorted = quicksort [ a | a <- xs, a <= x]
        biggerSorted = quicksort [ a | a <- xs, a > x ]
    in smallerSorted ++ [x] ++ biggerSorted
    
-- Aufgabe 2 (Funktionale Programmierung @ TU)
zeichenreihenBlock :: Integer -> [String]
zeichenreihenBlock x = reverse (partition (show x))

partition :: String -> [String]
partition [] = []
partition x
    | length x `mod` 3 == 2 = take 3 (addedOneZeroTo x) : (partition $ drop 3 $ addedOneZeroTo x)
    | length x `mod` 3 == 1 = take 3 ("00" ++ x) : partition (drop 3 ("00" ++ x))   
    | length x `mod` 3 == 0 = take 3 x : partition (drop 3 x)
    where addedOneZeroTo x =  "0" ++ x
    
-- Higher order functions
multThree :: (Num a) => a -> a -> a -> a
multThree x y z = x * y * z

-- ((multThree 3) 5) 6

multWithNine = multThree 9
-- multWithNine 2 3

multWithEighteen = multWithNine 2
-- multWithEighteen 10

compareWithHundret :: (Num a, Ord a) => a -> Ordering
compareWithHundret x = compare 100 x

-- compareWithHundret 99

compareWithHundret' :: (Num a, Ord a) => a -> Ordering
compareWithHundret' = compare 100

divideByTen :: (Floating a) => a -> a
divideByTen = (/10)

-- divideByTen 200

isUpperAlphanum :: Char -> Bool
isUpperAlphanum = (`elem` ['A'..'Z'])

-- isUpperAlphanum 'A'
    
-- Some higher-orderism is in order
applyTwice :: (a -> a) -> a -> a
applyTwice f x = f (f x)

-- applyTwice (+3) 10
-- applyTwice (++ " Haha") "Hey"
-- applyTwice (multThree 2 2) 9
-- applyTwice (3:) [1]

zipWith' :: (a -> b -> c) -> [a] -> [b] -> [c]
zipWith' _ [] _ = []
zipWith' _ _ [] = []
zipWith' f (x:xs) (y:ys) = f x y : zipWith' f xs ys

-- zipWith' (+) [4,2,5,6] [2,6,2,3]
-- zipWith' (*) (replicate 5 2) [1..]
-- zipWith' (zipWith' (*)) [[1,2,3],[3,5,6],[2,3,4]] [[3,2,2],[3,4,5],[5,4,3]]

flip' :: (a -> b -> c) -> (b -> a -> c)
flip' f = g
    where g x y = f y x

flip'' :: (a -> b -> c) -> b -> a -> c
flip'' f y x = f x y

-- flip'' zip [1,2,3,4,5] "hello"
-- zipWith (flip'' div) [2,2..] [10,8,6,4,2]

-- Maps and filters

-- map (+3) [1,5,3,1,6]
-- map (++ "!") ["BIFF", "BANG", "POW"]
-- map (replicate 3) [3..6]
-- map (map (^2)) [[1,2],[3,4,5,6],[7,8]]
-- map fst [(1,2),(3,5),(6,3),(2,6),(2,5)]

-- filter (>3) [1,5,3,2,1,6,4,3,2,1]
-- filter (==3) [1,2,3,4,5]
-- filter even [1..10]
-- let notNull x = not (null x) in filter notNull [[1,2,3],[],[3,4,5],[2,2],[],[],[]]
-- filter (`elem` ['a'..'z']) "u LaUgH aT mE BeCaUsE I aM diFfeRent"
-- filter (`elem` ['A'..'Z']) "i lauGh At You BecAuse u r aLL the Same"

quicksort' :: (Ord a) => [a] -> [a]
quicksort' [] = []
quicksort' (x:xs) =
    let smallerSorted = quicksort' . filter (<=x)  $ xs
        biggerSorted = quicksort' . filter (>x) $ xs 
    in smallerSorted ++ [x] ++ biggerSorted

largestDivisible :: (Integral a) => a
largestDivisible = head (filter p [100000,99999..])
    where p x = x `mod` 3829 == 0
    
-- takeWhile (/=' ') "elephants know how to party"

-- The sum of all odd squares that are smaller than 10,000.
-- sum (takeWhile (<10000) (filter odd (map (^2) [1..])))
-- sum $ takeWhile (<10000) $ filter odd $ map (^2) [1..]

-- Collatz sequences

-- For all starting numbers between 1 and 100, how many chains have a length greater than 15
numLongChains :: Int
numLongChains = length $ filter isLong $ map chain [1..100]
    where isLong xs = length xs > 15

chain :: (Integral a) => a -> [a]
chain 1 = [1]
chain n
    | even n = n:chain (n `div` 2)
    | odd n = n:chain (n*3 + 1)    
    
listOfFuns = map (*) [0..]
-- (listOfFuns !! 4) 5
-- = 20

-- Lambdas
numLongChains' :: Int
numLongChains' = length $ filter (\xs -> length xs > 15) $ map chain [1..100]

-- zipWith (\a b -> (a * 30 + 3) / b) [5,4,3,2,1] [1,2,3,4,5]
-- map (\(a,b) -> a + b) [(1,2),(3,5),(6,3),(2,6),(2,5)]

flip''' :: (a -> b -> c) -> b -> a -> c
flip''' f = \x y -> f y x

-- Folds
sum'' :: (Num a) => [a] -> a
sum'' xs = foldl (\acc x -> acc + x) 0 xs

sum''' :: (Num a) => [a] -> a
sum''' = foldl (+) 0

elem'' :: (Eq a) => a -> [a] -> Bool
elem'' y ys = foldl (\acc x -> if x == y then True else acc) False ys

map' :: (a -> b) -> [a] -> [b]
map' f xs = foldr (\x acc -> f x : acc) [] xs

maximum''' :: (Ord a) => [a] -> a
maximum''' = foldr1 (\x acc -> if x > acc then x else acc)

reverse'' :: [a] -> [a]
reverse'' = foldl (\acc x -> x : acc) []
-- reverse'' = foldl (flip (:)) []

product' :: (Num a) => [a] -> a
product' = foldr1 (*)

filter' :: (a -> Bool) -> [a]-> [a]
filter' p = foldr (\x acc -> if p x then x : acc else acc) []

head'''' :: [a] -> a
head'''' = foldr1 (\x _ -> x)

last' :: [a] -> a
last' = foldl1 (\_ x -> x)

-- scanl (+) 0 [3,5,2,1]
--[0,3,8,10,11]
-- scanr (+) 0 [3,5,2,1]
--[11,8,3,1,0]
-- scanl1 (\acc x -> if x > acc then x else acc) [3,4,5,3,7,9,2,1]
--[3,4,5,5,7,9,9,9]
-- scanl (flip (:)) [] [3,2,1]
--[[],[3],[2,3],[1,2,3]]    
    
-- How many elements does it take for the sum of the roots of all 
-- natural numbers to exceed 1000?

sqrtSums :: Int
sqrtSums = (length $ takeWhile (<1000) $ scanl1 (+) $ map sqrt [1..]) + 1

-- Function application with $

-- sum (map sqrt [1..130])
-- sum $ map sqrt [1..130]

-- sum (filter (> 10) (map (*2) [2..10]))
-- sum $ filter (> 10) $ map (*2) [2..10]
    
-- Function composition

-- A list of numbers and we want to turn them all into negative numbers
-- map (\x -> negate (abs x)) [5,-3,-6,7,-3,2,-19,24]
-- [-5,-3,-6,-7,-3,-2,-19,-24]

-- map (negate . abs) [5,-3,-6,7,-3,2,-19,24]
-- [-5,-3,-6,-7,-3,-2,-19,-24]

--map (\xs -> negate (sum (tail xs))) [[1..5],[3..6],[1..7]]
--[-14,-15,-27]

--into

--map (negate . sum . tail) [[1..5],[3..6],[1..7]]
--[-14,-15,-27]

-- sum (replicate 5 (max 6.7 8.9))
-- (sum . replicate 5 . max 6.7) 8.9
-- sum . replicate 5 . max 6.7 $ 8.9

-- replicate 100 (product (map (*3) (zipWith max [1,2,3,4,5] [4,5,6,7,8])))
-- replicate 100 . product . map (*3) . zipWith max [1,2,3,4,5] $ [4,5,6,7,8]
-- replicate 100 $ product $ map (*3) $ zipWith max [1,2,3,4,5] [4,5,6,7,8]
    

    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
