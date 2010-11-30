--------------------------------------------------------------------------------
-- 3. Aufgabenblatt zu Funktionale Programmierung vom 19.10.2010
-- 
-- Gruppe: 145
-- Mitglieder: Felix MAYERHUBER (0825283) und Dominik RAUCH (0825084)
-- 
-- Created: 15.10.2010
--------------------------------------------------------------------------------

> import Prelude

--------------------------------------------------------------------------------
-- 3.1 dreiNplusEins
-- 
--------------------------------------------------------------------------------

> dreiNplusEins :: Integer -> [Integer]
> dreiNplusEins n
>	| n == 1 = [1]
> 	| even n = n : dreiNplusEins x1
>	| otherwise = n : dreiNplusEins x2
>		where
>		x1 = n `div` 2
>		x2 = n * 3 + 1
	
--------------------------------------------------------------------------------
-- 3.2 maxZyklus
-- 
--------------------------------------------------------------------------------

> type UntereGrenze = Integer
> type ObereGrenze = Integer
> type MaxZykLaenge = Integer

> integerLength :: [a] -> Integer
> integerLength x = toInteger (length x)

> thd3 :: (a,b,Integer) -> Integer
> thd3 (_,_,third) = third

> maxZyklus :: UntereGrenze -> ObereGrenze -> (UntereGrenze,ObereGrenze,MaxZykLaenge)
> maxZyklus u o
>	| u > o = (u,o,0)
>	| otherwise = (u,o, maxZyklus' [u..o] 0)

> maxZyklus' :: [Integer] -> Integer -> Integer
> maxZyklus' (x:xs) max
>	| xs == [] = if max >= dreiNplusEinsX then max else dreiNplusEinsX
>	| max >= dreiNplusEinsX = maxZyklus' xs max
>	| otherwise = maxZyklus' xs dreiNplusEinsX
>	where
>	dreiNplusEinsX = integerLength (dreiNplusEins x)

--------------------------------------------------------------------------------
-- 3.3 anzNachbarn
-- 
--------------------------------------------------------------------------------

> type BoolMatrix = BoolRows -- [Zeile1, Zeile2, ..., ZeileM]
> type BoolRows = [BoolRow]
> type BoolRow = [BoolCell] -- [Spalte1, Spalte2, ..., SpalteN]
> type BoolCell = Bool
> type Coord = (RowCoord,ColCoord)
> type RowCoord = Integer
> type ColCoord = Integer

> rows :: BoolMatrix -> Integer
> rows matrix = integerLength matrix

> cols :: BoolMatrix -> Integer
> cols matrix = integerLength (head matrix)

> inMatrix :: BoolMatrix -> Coord -> Bool
> inMatrix matrix (row,col)
>	| row < 0 || row >= rows matrix = False
>	| col < 0 || col >= cols matrix = False
>	| otherwise = True

> trueInMatrix :: BoolMatrix -> Coord -> Bool
> trueInMatrix matrix (row,col)
> 	| not (inMatrix matrix (row,col)) = False
> 	| otherwise = (matrix!!fromInteger row)!!fromInteger col

> anzNachbarn :: BoolMatrix -> Coord -> Integer
> anzNachbarn matrix (row,col)
> 	| not (inMatrix matrix (row,col)) = -1
> 	| otherwise =	count (row-1,col-1) +
> 					count (row-1,col) +
> 					count (row-1,col+1) +
> 					count (row,col-1) +
> 					count (row,col+1) +
> 					count (row+1,col-1) +
> 					count (row+1,col) +
> 					count (row+1,col+1)
>	  where
>	  count (row,col) = if trueInMatrix matrix (row,col) then 1 else 0

--------------------------------------------------------------------------------
-- 3.4 transform
-- 
--------------------------------------------------------------------------------

> type IntegerMatrix = [[Integer]]
> type IntegerRow = [Integer]
> type BoolCells = [Bool]
> type IntegerCell = Integer

> transform :: BoolMatrix -> IntegerMatrix
> transform matrix = transform' matrix matrix 0

> transform' :: BoolMatrix -> BoolRows -> RowCoord -> IntegerMatrix
> transform' matrix [] x = []
> transform' matrix (row:rows) x = transformed_row : transform' matrix rows (x+1)
>	where
>	transformed_row = transform_row matrix row x

> transform_row :: BoolMatrix -> BoolRow -> RowCoord -> IntegerRow
> transform_row matrix row x = transform_row' matrix row x 0

> transform_row' :: BoolMatrix -> BoolCells -> RowCoord -> ColCoord -> IntegerRow
> transform_row' matrix [] x y = []
> transform_row' matrix (cell:cells) x y = transformed_cell : transform_row' matrix cells x (y+1)
>	where
>	transformed_cell = anzNachbarn matrix (x,y)

--------------------------------------------------------------------------------
