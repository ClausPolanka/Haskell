fib :: Integer -> Integer
fib 0 = 1
fib 1 = 1
fib n = fib(n - 2) + fib(n - 1)

capVowels :: Char -> Char
capVowels 'a' = 'A'
capVowels 'e' = 'E'
capVowels 'i' = 'I'
capVowels 'o' = 'O'
capVowels 'u' = 'U'
capVowels 'c' = 'c'