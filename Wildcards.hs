add :: Integer -> Integer -> Integer
add n 0 = n
add 0 n = n
add m n = m + n

mult :: Integer -> Integer -> Integer
mult 0 _ = 0
mult _ 0 = 0
mult m n = m * n