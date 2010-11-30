fac :: Integer -> Integer
fac n = 
	if n == 0 then 
		1 
	else 
		(n * fac(n - 1))
		
-- Form der bedingten Gleichungen
fac1 :: Integer -> Integer
fac1 n
	| n == 0 = 1
	| otherwise = n * fac1(n - 1)
	
-- Lambda Kalkül
fac2 :: Integer -> Integer
fac2 = \n -> (if n == 0 then 1 else (n * fac2(n - 1)))

-- Gleichungsorientiert
fac3 :: Integer -> Integer
fac3 n = if n == 0 then 1 else (n * fac3(n - 1))