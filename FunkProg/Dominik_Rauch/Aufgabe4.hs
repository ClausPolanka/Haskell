--------------------------------------------------------------------------------
-- 4. Aufgabenblatt zu Funktionale Programmierung vom 26.10.2010
-- 
-- Gruppe: 145
-- Mitglieder: Felix MAYERHUBER (0825283) und Dominik RAUCH (0825084)
-- 
-- Created: 28.10.2010
--------------------------------------------------------------------------------

module Aufgabe4 where
import Prelude

--------------------------------------------------------------------------------
-- 4.1 a
-- 
--------------------------------------------------------------------------------

data Tree = Leaf Integer |
			Node Integer Tree Tree

type Layer = [Integer]
data Ord = BottomUp | TopDown

-- writeLayer :: Tree -> Ord -> [Layer]
-- writeLayer root TopDown = writeLayer' [[]] [root] 1
-- writeLayer root BottomUp = reverse (writeLayer root TopDown)

-- writeLayer' :: [Layer] -> [Tree] -> Integer -> [Layer]
-- writeLayer' result queue nodeNumber =
	-- case queue of
		-- ((Leaf value):[]) -> newresult value -- Leaf & last Tree element in queue: Add value, end.
		-- ((Leaf value):xs) -> writeLayer' (newresult value) xs (nodeNumber+1) -- Leaf: Add value, process rest of queue.
		-- ((Node value left right):xs) -> writeLayer' (newresult value) (xs ++ [left] ++ [right]) (nodeNumber+1) -- Node: Add value, add children to queue, process rest of queue.
	-- where
	-- layerNumber = floor (logBase 2 (fromInteger nodeNumber))
	-- (pre, layer:post) = splitAt layerNumber result
	-- newresult value = pre ++ [layer ++ [value]] ++ post
	
	-- newresult value = 
		-- case splitAt layerNumber result of
			-- (pre, layer::[]) -> pre ++ [layer ++ [value]]
			-- (pre, layer:post) -> pre ++ [layer ++ [value]] ++ post

writeLayer :: Tree -> Ord -> [Layer]
writeLayer (Leaf a) _ = [[a]]
writeLayer tree TopDown = [getLayerN tree z 1 | z <- [1..depthTree tree]]
writeLayer tree BottomUp = [getLayerN tree z 1 | z <- [depthTree tree - n | n <-[0..depthTree tree-1]]]

getLayerN :: Tree -> Integer -> Integer -> Layer
getLayerN (Leaf a) n actual =  if(n==actual) then[a] else []
getLayerN (Node node st1 st2) n actual = if(n==actual) then [node] else getLayerN st1 n (actual+1) ++ getLayerN st2 n (actual+1)

depthTree :: Tree -> Integer
depthTree (Leaf _) = 1
depthTree (Node _ bt1 bt2) = 1 + max (depthTree bt1) (depthTree bt2)
			
--------------------------------------------------------------------------------
-- 4.1 b
-- 
--------------------------------------------------------------------------------

data STree = Nil |
			 SNode Integer STree STree deriving Show

transform :: Tree -> STree
transform (Leaf value) = (SNode value Nil Nil)
transform node = listToSTree (treeToList node)

treeToList :: Tree -> [Integer]
treeToList (Leaf value) = [value]
treeToList (Node value left right) = [value] ++ treeToList left ++ treeToList right

listToSTree :: [Integer] -> STree
listToSTree [] = Nil
listToSTree (x:xs) = (SNode x (listToSTree [y | y <- xs, y<x]) (listToSTree [y | y <- xs, y>x]))

--------------------------------------------------------------------------------
-- 4.2 a
-- 
--------------------------------------------------------------------------------

type Cost = Integer
type Vertex = Integer
newtype Graph = Graph [(Vertex,[(Vertex,Cost)])]

data Result = Yes | No | Invalid deriving Show
data PathResult = InvalidPath |
				  Cost Integer
				  deriving Eq

path :: Graph -> Vertex -> Vertex -> Cost -> Result
path graph from to maxCost
	| notInGraph from graph = Invalid
	| notInGraph to graph = Invalid
	| otherwise = path' graph from to maxCost

notInGraph :: Vertex -> Graph -> Bool
notInGraph v (Graph []) = True
notInGraph v (Graph ((s,tos):gs)) = if v == s || contains v (map fst tos) then False else notInGraph v (Graph gs)
	
path' :: Graph -> Vertex -> Vertex -> Cost -> Result
path' graph from to maxCost =
	case minPathCostResult of
		InvalidPath -> No
		(Cost cost) -> if maxCost - cost < 0 then No else Yes
	where
	(_, minPathCostResult) = minPathCost graph from to True [] 0

minPathCost :: Graph -> Vertex -> Vertex -> Bool -> [Vertex] -> Cost -> ([Vertex], PathResult)
minPathCost graph from to negCosts visited costSum
	| contains from visited = ([], InvalidPath)
	| neighbours == [] = ([], InvalidPath)
	| otherwise = follow graph neighbours to negCosts (visited ++ [from]) costSum
		where
		neighbours = getNeighbours from graph negCosts

contains :: Vertex -> [Vertex] -> Bool
contains v [] = False
contains v (x:xs) = if v == x then True else contains v xs
		
getNeighbours :: Vertex -> Graph -> Bool -> [(Vertex,Cost)]
getNeighbours x (Graph ((v,n):xs)) negCosts
	| xs == [] = if x == v then filteredN else []
	| otherwise = if x == v then filteredN else getNeighbours x (Graph xs) negCosts
	where
	filteredN = if negCosts then n else filter (\(v,c) -> c > 0) n
	
follow :: Graph -> [(Vertex,Cost)] -> Vertex -> Bool-> [Vertex] -> Cost -> ([Vertex], PathResult)
follow graph froms to negCosts visited costSum =
	case froms of
		(neighbour,cost):[] -> followResult neighbour cost
		(neighbour,cost):xs -> compareResult (followResult neighbour cost) (follow graph xs to negCosts visited costSum)
	where
	followResult neighbour cost = if neighbour == to then (visited, (Cost (costSum+cost))) else minPathCost graph neighbour to negCosts visited (costSum+cost)

compareResult :: ([Vertex], PathResult) -> ([Vertex], PathResult) -> ([Vertex], PathResult)
compareResult a@(_,ares) b@(_,bres) =
	case (ares,bres) of
		(InvalidPath,_) -> b
		(_,InvalidPath) -> a
		((Cost costa), (Cost costb)) -> if costa < costb then a else b

--------------------------------------------------------------------------------
-- 4.2 b
-- 
--------------------------------------------------------------------------------

minpath :: Graph -> Vertex -> Vertex -> ([Vertex], Cost)
minpath graph from to
	| notInGraph from graph = ([], 0)
	| notInGraph to graph = ([], 0)
	| otherwise = minpath' graph from to

minpath' :: Graph -> Vertex -> Vertex -> ([Vertex], Cost)
minpath' graph from to = 
	case minPathCostResult of
		([], InvalidPath) -> ([], 0)
		(path, (Cost cost)) -> (path++[to], cost)
	where
	minPathCostResult = minPathCost graph from to False [] 0

--------------------------------------------------------------------------------
