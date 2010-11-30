imax :: Integer -> Integer -> Integer
imax p q
	| p >= q = p
	| otherwise = q

tripleMax :: Integer -> Integer -> Integer -> Integer
tripleMax p q r
	| (imax p q == p) && (p 'imax' r == p) = p
	| (imax p q == q) && (q 'imax' r == q) = q
	| otherwise = r