--------------------------------------------------------------------------------
-- 5. Aufgabenblatt zu Funktionale Programmierung vom 09.11.2010
-- 
-- Gruppe: 145
-- Mitglieder: Felix MAYERHUBER (0825283) und Dominik RAUCH (0825084)
-- 
-- Created: 09.11.2010
--------------------------------------------------------------------------------

type Cost = Integer -- Eine Kosteninformation
type Vertex = Integer -- Ein Knoten
type MaxVertexNo = Integer -- Höchste Knotennummer (= Anzahl der Knoten)
type Edge = (Vertex, Cost, Vertex) -- Kante (von, Kosten, zu)
type Row = [Integer] -- Zeile einer Matrix

data ALgraph = ALg [(Vertex, [(Vertex, Cost)])] deriving (Eq, Show) -- Adjazenzliste
data ELgraph = ELg MaxVertexNo [Edge] deriving (Eq, Show) -- Kantenliste
data AMgraph = AMg [Row] deriving (Eq, Show) -- Adjazenzmatrix

type Inp = (MaxVertexNo, [(Vertex,Vertex,Cost)]) -- Form des Inputs

--------------------------------------------------------------------------------
-- 5.1
-- 
--------------------------------------------------------------------------------

isValid :: Inp -> Bool
isValid (maxVertexNo, edges) = if maxVertexNo < 0 then False else isValid' maxVertexNo edges []

isValid' :: MaxVertexNo -> [(Vertex,Vertex,Cost)] -> [(Vertex,Vertex)] -> Bool
isValid' _ [] _ = True
isValid' maxVertexNo ((v1, v2, cost):xs) edges
	| v1 < 0 || v1 > maxVertexNo = False -- Anfangspunkt mit Wert zwischen 0 und MaxVertexNo
	| v2 < 0 || v2 > maxVertexNo = False -- Endpunkt mit Wert zwischen 0 und MaxVertexNo
	| elem (v1,v2) edges = False -- Höchstens eine Kante zwischen zwei Knoten
	| cost <= 0 = False -- Kantenkosten echt größer als 0
	| otherwise = isValid' maxVertexNo xs ((v1,v2):edges)

--------------------------------------------------------------------------------
-- 5.2
-- 
--------------------------------------------------------------------------------

-- a) --  --  --  --  --  --  --  --  --  --  --  --  --  --  --  --  --  --  --

inp2el :: Inp -> ELgraph
inp2el (maxVertexNo, edges) = (ELg maxVertexNo (inp2elEdges edges))

inp2elEdges :: [(Vertex,Vertex,Cost)] -> [Edge]
inp2elEdges [] = []
inp2elEdges ((v1, v2, cost):xs) = (v1, cost, v2):(inp2elEdges xs)

-- b) --  --  --  --  --  --  --  --  --  --  --  --  --  --  --  --  --  --  --

al2am :: ALgraph -> AMgraph
al2am g = el2am (al2el g)

-- c) --  --  --  --  --  --  --  --  --  --  --  --  --  --  --  --  --  --  --

al2el :: ALgraph -> ELgraph
al2el (ALg xs) = (ELg maxVertexNo edges)
	where
	maxVertexNo = al2elMaxVertexNo xs
	edges = al2elEdges xs

al2elMaxVertexNo :: [(Vertex, [(Vertex, Cost)])] -> MaxVertexNo
al2elMaxVertexNo [] = 0
al2elMaxVertexNo ((v, adjs):xs) = max3 v adjMax (al2elMaxVertexNo xs)
	where
	adjMax = if length adjs > 0 then maximum (map fst adjs) else 0

al2elEdges :: [(Vertex, [(Vertex, Cost)])] -> [Edge]
al2elEdges [] = []
al2elEdges ((v, adjs):xs) = al2elEdges' v adjs ++ al2elEdges xs

al2elEdges' :: Vertex -> [(Vertex, Cost)] -> [Edge]
al2elEdges' _ [] = []
al2elEdges' v ((adj, cost):xs)
	| cost > 0 = (v, cost, adj) : nextEdges
	| otherwise = nextEdges
	where
	nextEdges = al2elEdges' v xs
	
max3 :: Integer -> Integer -> Integer -> Integer
max3 a b c = max a (max b c)

-- d) --  --  --  --  --  --  --  --  --  --  --  --  --  --  --  --  --  --  --

am2al :: AMgraph -> ALgraph
am2al graph = ALg(am2alHelper graph 0)

am2alHelper :: AMgraph -> Integer -> [(Vertex,[(Vertex,Cost)])]
am2alHelper (AMg (row:rows)) rowNr
	| rows == [] = [(rowNr,(convertRow row 0))]
	| otherwise = (rowNr,(convertRow row 0)):(am2alHelper(AMg rows) (rowNr+1))
	
convertRow :: Row -> Integer -> [(Vertex,Cost)]
convertRow (col:cols) column
	| cols == [] = if col > 0 then [(column,col)] else []
	| col > 0 = (column,col):convertRow cols (column+1)
	| otherwise = convertRow cols (column+1)

-- e) --  --  --  --  --  --  --  --  --  --  --  --  --  --  --  --  --  --  --

am2el :: AMgraph -> ELgraph
am2el g = al2el (am2al g)

-- f) --  --  --  --  --  --  --  --  --  --  --  --  --  --  --  --  --  --  --

el2al :: ELgraph -> ALgraph
el2al g = am2al (el2am g)

-- g) --  --  --  --  --  --  --  --  --  --  --  --  --  --  --  --  --  --  --

el2am :: ELgraph -> AMgraph
el2am (ELg maxVertexNo edges) = (AMg (el2am' maxVertexNo 0 edges))

el2am' :: MaxVertexNo -> Integer -> [Edge] -> [Row]
el2am' maxVertexNo v edges
	| maxVertexNo == v = [thisRow]
	| otherwise = (thisRow:nextRows)
	where
	thisRow = el2amRow maxVertexNo v 0 edgesInRow
	nextRows = el2am' maxVertexNo (v+1) edges
	edgesInRow = filter (\(v1,cost,v2) -> v1 == v) edges

el2amRow :: MaxVertexNo -> Integer -> Integer -> [Edge] -> Row
el2amRow maxVertexNo v1 v2 edges
	| maxVertexNo == v2 = [thisCell]
	| otherwise = (thisCell:nextCells)
	where
	thisCell = costForCell v1 v2 edges
	nextCells = el2amRow maxVertexNo v1 (v2+1) edges

costForCell :: Integer -> Integer -> [Edge] -> Integer
costForCell v1 v2 [] = 0
costForCell v1 v2 ((ev1,cost,ev2):xs)
	| v1==ev1 && v2==ev2 = cost
	| otherwise = costForCell v1 v2 xs 

	-- GESCHEITERTER VERSUCH DOMINIK, WÄRE "SCHÖNER", ABER GEHT WOHL NICHT...
	-- el2am :: ELgraph -> AMgraph
	-- el2am (ELg maxVertexNo edges) = el2am' matrix edges
		-- where
		-- matrix = replicate vertexCount (replicate vertexCount 0)
		-- vertexCount = maxVertexNo + 1

	-- el2am' :: [Row] -> [Edge] -> [Row]
	-- el2am' m [] = m
	-- el2am' m (edge:edges) = el2am' newMatrix edges
		-- where
		-- newMatrix = el2amEdge m edge
		
	-- el2amEdge :: [Row] -> Edge -> [Row]
	-- el2amEdge m (v1, cost, v2) = m@((m!!v1)!!v2) = cost

--------------------------------------------------------------------------------
-- 5.3
-- 
--------------------------------------------------------------------------------

-- a) --  --  --  --  --  --  --  --  --  --  --  --  --  --  --  --  --  --  --

isNeighbourOf :: ELgraph -> Vertex -> Vertex -> Bool
isNeighbourOf elg@(ELg max xs) node1 node2
	| xs == [] = False
	| max < 0 = False
	| otherwise = isNeighbourOf' elg node1 node2

isNeighbourOf' :: ELgraph -> Vertex -> Vertex -> Bool
isNeighbourOf' (ELg max ((a,c,b):xs)) node1 node2
	| xs == [] = (a==node1 && b==node2)
	| a==node1 && b==node2 = True 
	| otherwise = isNeighbourOf' (ELg max xs) node1 node2

-- b) --  --  --  --  --  --  --  --  --  --  --  --  --  --  --  --  --  --  --

allNeighboursOf :: ELgraph -> Vertex -> [Vertex]
allNeighboursOf graph node = allNeighboursHelper (el2am(graph)) node 0

allNeighboursHelper :: AMgraph -> Vertex -> Integer -> [Vertex]
allNeighboursHelper (AMg (row:rows)) node rowNr
	| rows == [] && node /= rowNr = []
	| node == rowNr = allNeighboursHelperHelper row 0
	| otherwise = allNeighboursHelper (AMg rows) node (rowNr+1)

allNeighboursHelperHelper :: Row -> Integer -> [Vertex]
allNeighboursHelperHelper (x:xs) column
	| xs == [] = if x > 0 then [column] else []
	| otherwise = if x > 0 then column:nextVertex else nextVertex
		where nextVertex = allNeighboursHelperHelper xs (column+1)

-- c) --  --  --  --  --  --  --  --  --  --  --  --  --  --  --  --  --  --  --

numberOfEdges :: AMgraph -> Integer
numberOfEdges graph = lengthELgraph (am2el graph)

lengthELgraph :: ELgraph -> Integer
lengthELgraph elg@(ELg max xs)
	| xs == [] = 0
	| max < 0 = 0
	| otherwise = lengthELgraph' elg

lengthELgraph' :: ELgraph -> Integer
lengthELgraph' (ELg max ((a,c,b):xs))
	| xs == [] = 1
	| otherwise = 1 + lengthELgraph' (ELg max xs)

-- d) --  --  --  --  --  --  --  --  --  --  --  --  --  --  --  --  --  --  --

isOnCycle :: ALgraph -> Vertex -> Cost -> Bool
isOnCycle graph node maxCost
	| getVertexNeighbors graph node == [] = False
	| otherwise = followPath graph node 0 maxCost (getVertexNeighbors graph node)

followPath :: ALgraph -> Vertex -> Cost -> Cost -> [(Vertex,Cost)] -> Bool
followPath graph goal nowCost maxCost ((a,b):xs)
	| nowCost > maxCost = False
	| a == goal && (nowCost+b) <= maxCost = True
	| xs == [] = if( nextNeighborsNotEmpty && followDeeper) then True else False
	| otherwise = if( nextNeighborsNotEmpty && followDeeper) then True else followPath graph goal nowCost maxCost xs
		where 
			nextNeighborsNotEmpty = getVertexNeighbors graph a /= []
			followDeeper = followPath graph goal (nowCost+b) maxCost (getVertexNeighbors graph a)
	
getVertexNeighbors :: ALgraph -> Vertex -> [(Vertex,Cost)]
getVertexNeighbors (ALg ((a,b):xs)) vert
	| xs == [] = if(a==vert) then b else []
	| otherwise = if(a==vert) then b ++ nextVertex else nextVertex
		where nextVertex = getVertexNeighbors (ALg xs) vert

--------------------------------------------------------------------------------
