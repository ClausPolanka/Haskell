module Aufgabe5 where

-- Gültige Graphen:
-- Gerichtete Graphen mit kostenbenannten Kannten.
-- Höchstens eine gerichtete Kante zw. je zwei Knoten je Richtung (laut Forum).
-- Alle Knoten fortlaufend nummeriert beginnend bei 0.
-- Alle Kantenkosten > 0.

type Cost = Integer
type Vertex = Integer
type MaxVertexNo = Integer

type Inp = (MaxVertexNo, [(Vertex, Vertex, Cost)])

-- Aufgabe 1 ------------------------------------------------------------------------------
isValid :: Inp -> Bool
isValid (x, _)
    | x < 0 = False
isValid (x, xs)
    | fromIntegral x < length xs = False
    | someCostsLowerThanZero xs = False
    | edgesNotUnique xs xs = False
    | otherwise = True

someCostsLowerThanZero :: [(Vertex, Vertex, Cost)] -> Bool
someCostsLowerThanZero xs
    | length [ z | (x, y, z) <- xs, (fromIntegral z) <= 0 ] > 0 = True
    | otherwise = False
    
edgesNotUnique :: [(Vertex, Vertex, Cost)] -> [(Vertex, Vertex, Cost)] -> Bool
edgesNotUnique xs ys
    | length [ 1 | (x, y, _) <- xs, (r, s, _) <- ys, x == r && y == s ] > length xs = True
    | otherwise = False

-- Aufgabe 2 ------------------------------------------------------------------------------

-- Adjazenzlisten
data ALgraph = ALg [(Vertex, [(Vertex, Cost)])] deriving (Eq, Show)
-- Adjazenzmatrizen
data AMgraph = AMg [Row] deriving (Eq, Show)
-- Kantenlisten
data ELgraph = ELg MaxVertexNo [Edge] deriving (Eq, Show)

type Row = [Integer]
type Edge = (Vertex, Cost, Vertex)

-- a) --------------------------------------------------------------------------------------
inp2el :: Inp -> ELgraph
inp2el (maxVertexNo, xs) = ELg maxVertexNo (toEdges xs)

toEdges :: [(Vertex, Vertex, Cost)] -> [(Vertex, Cost, Vertex)]
toEdges [] = []
toEdges ((v1, v2, c):xs) = (v1, c, v2):toEdges xs

-- b) --------------------------------------------------------------------------------------
al2am :: ALgraph -> AMgraph
al2am alGraph = convertToAMGraph
    where convertToAMGraph = AMg $ buildAM nrOfNodes allEdges
          nrOfNodes = [0..((lengthOf alGraph) - 1)]
          allEdges = (filterEdgesFor nrOfNodes alGraph)

lengthOf (ALg []) = 0
lengthOf (ALg (x:xs)) = 1 + lengthOf (ALg xs)

buildAM _ [] = []
buildAM [] _ = []
buildAM nrOfNodes (edgesOfFirstVerdict:xs) = setCostsForVerdict nrOfNodes edgesOfFirstVerdict : buildAM nrOfNodes xs
              
setCostsForVerdict [] _ = []
setCostsForVerdict (n:ns) edges
    | ifEdgeDoesNotExist = zeroCosts : (setCostsForVerdict ns edges)
    | otherwise = costOfEdge : (setCostsForVerdict ns edges)
        where ifEdgeDoesNotExist = getEdge n edges == (0, 0)
              zeroCosts = 0
              costOfEdge = snd (getEdge n edges)

getEdge _ [] = (0, 0)
getEdge n (x:xs) 
    | n == (fst x) = x
    | otherwise = getEdge n xs

filterEdgesFor [] _ = []
filterEdgesFor (x:xs) alGraph = nodeList : filterEdgesFor xs alGraph
    where nodeList = nodesOf x alGraph

nodesOf :: Vertex -> ALgraph -> [(Vertex, Cost)]
nodesOf v (ALg x)
    | length nodes > 0 = nodes !! 0
    | otherwise = []
        where nodes = [xs | (x, xs) <- x, x == v ]
    
-- c) --------------------------------------------------------------------------------------
al2el :: ALgraph -> ELgraph
al2el alGraph = createNewElGraph
    where createNewElGraph = ELg nrOfNodes edges
          nrOfNodes = (lengthOf alGraph) - 1 -- Because verdict-numbers start with zero.
          edges = flatten $ buildEdgesFor alGraph

buildEdgesFor :: ALgraph -> [[(Vertex, Cost, Vertex)]]
buildEdgesFor (ALg []) = []
buildEdgesFor (ALg (x:xs)) = buildTupelFor verdict edges : buildEdgesFor (ALg xs)
    where verdict = fst x
          edges = snd x

buildTupelFor :: t -> [(a, b)] -> [(t, b, a)]
buildTupelFor _ [] = []
buildTupelFor verdict1 (x:xs) =  (verdict1, cost, verdict2) : buildTupelFor verdict1 xs
    where cost = snd x
          verdict2 = fst x

flatten :: [[a]] -> [a]
flatten = foldl (++) []

-- d) --------------------------------------------------------------------------------------
am2al :: AMgraph -> ALgraph
am2al amGraph = ALg $ buildAlGraph [0..(lengthOfAMg amGraph)-1] amGraph

lengthOfAMg (AMg []) = 0
lengthOfAMg (AMg (x:xs)) = 1 + lengthOfAMg (AMg xs)

buildAlGraph :: [Integer] -> AMgraph -> [(Vertex, [(Vertex, Cost)])]
buildAlGraph [] _ = []
buildAlGraph (vertex:xs) amGraph = (vertex, getListOfNeighbours amGraph vertex [0..(lengthOfAMg amGraph)-1]) : buildAlGraph xs amGraph

getListOfNeighbours :: AMgraph -> Integer -> [Integer] -> [(Vertex, Cost)]
getListOfNeighbours (AMg x) vertex size = vertexWithCost (x !! (fromIntegral vertex)) size

vertexWithCost [] _ = []
vertexWithCost _ [] = []
vertexWithCost (cost:xs) (vertex:ys)
    | cost /= 0 = (vertex, cost) : vertexWithCost xs ys
    | otherwise = vertexWithCost xs ys


-- e) --------------------------------------------------------------------------------------
am2el :: AMgraph -> ELgraph
am2el amGraph = createNewEdgeList
    where createNewEdgeList = ELg maxVertexNo (buildEdges amGraph nrOfVertices)
          maxVertexNo = ((lengthOfAMg amGraph) - 1)
          nrOfVertices = [0..maxVertexNo]

buildEdges :: AMgraph -> [Integer] -> [(Vertex, Cost, Vertex)]
buildEdges (AMg []) _ = []
buildEdges _ [] = []
buildEdges (AMg (row:xs)) (vertex:ys) = (filterEdgeFor row vertex) ++ (buildEdges (AMg xs) ys)

filterEdgeFor :: Row -> Integer -> [(Vertex, Cost, Vertex)]
filterEdgeFor row vertex = getEdgesForSpecificRow vertex rowSize row
    where rowSize = [0..maxRow]
          maxRow = fromIntegral (length row) - 1

getEdgesForSpecificRow :: Integer -> [Integer] -> Row -> [(Vertex, Cost, Vertex)] 
getEdgesForSpecificRow _ [] _ = []
getEdgesForSpecificRow _ _ [] = []
getEdgesForSpecificRow vertex (vertex2:xs) (cost:ys)
    | cost /= 0 = (vertex, cost, vertex2) : getEdgesForSpecificRow vertex xs ys
    | otherwise = getEdgesForSpecificRow vertex xs ys

-- f) --------------------------------------------------------------------------------------
el2al :: ELgraph -> ALgraph
el2al (ELg max edges) = ALg (toAl [0..max] edges)

toAl _ [] = []
toAl [] _ = []
toAl (x:xs) edges = (x, (getNeighbours x edges)) : (toAl xs edges)

getNeighbours x edges
    | (length result) == 0 = []
    | otherwise = result
        where result = [ (v2, c) | (v1, c, v2) <- edges, v1 == x ]

-- g) --------------------------------------------------------------------------------------
el2am :: ELgraph -> AMgraph
el2am (ELg x xs) = AMg $ (buildAmGraph maxVertex (sort xs maxVertex) maxVertex)
    where maxVertex = [0..x]

buildAmGraph [] _ _ = []
buildAmGraph _ [] _ = []
buildAmGraph (vertex:xs) sortedEdges zs = (buildRowFor vertex sortedEdges zs) : (buildAmGraph xs sortedEdges zs)

sort [] _ = []
sort _ [] = []
sort xs nrs@(vertex:vs) = (sortByNeighbour unsortedNeighbours nrs) ++ sort xs vs
    where unsortedNeighbours = [(x, y, z) | (x, y, z) <- xs, x == vertex]

sortByNeighbour [] _ = []
sortByNeighbour _ [] = []
sortByNeighbour xs (vertex:vs) = [(x, y, z) | (x, y, z) <- xs, z == vertex] ++ sortByNeighbour xs vs

buildRowFor v [] (x:xs) = 0 : buildRowFor v [] xs
buildRowFor _ [] _ = []
buildRowFor _ _ [] = []
buildRowFor vertex edges@((v1, cost, v2):xs) (y:ys)
    | v1 == vertex && v2 /= y = 0 : buildRowFor vertex edges ys
    | v1 == vertex && v2 == y = cost : buildRowFor vertex xs ys
    | otherwise = 0 : buildRowFor vertex xs ys

-- Test cases must be deleted ------------------------------------------
testall = and [testinp]

-- 1.
tinp_all = [tinp_1, tinp_2, tinp_3, tinp_4]

tinp_1 = (4,[(1,2,500), (0,3,100), (0,2,200)]) -- okay
tinp_2 = (2,xs) -- zu viele Kanten
	where (_,xs) = tinp_1
tinp_3 = (maxno,(2,1,400):xs) -- zwei Kanten zwischen zwei Knoten
	where (maxno,xs) = tinp_1
tinp_4 = (maxno,(1,3,-400):xs) -- negative Kosten
	where (maxno,xs) = tinp_1

testinp = [True,False,True,False] == [isValid x | x <- tinp_all]
    
-- 2.: Beachten Sie, dass das Resultat i.a. nicht eindeutig festgelegt ist!
testkonv = and tkonv_all

tkonv_all = [tkonv_a, tkonv_b, tkonv_c, tkonv_f]-- tkonv_g, tkonv_e, tkonv_g]

tkonv_a = inp2el tinp_1 == (ELg 4 [(1,500,2),(0,100,3),(0,200,2)])
tkonv_b = al2am (el2al (inp2el tinp_1)) == (AMg [[0,0,200,100,0],[0,0,500,0,0],[0,0,0,0,0],[0,0,0,0,0],[0,0,0,0,0]])
tkonv_c = al2el (el2al (inp2el tinp_1)) == (ELg 4 [(0,100,3),(0,200,2),(1,500,2)] )
tkonv_d = am2al (el2am (inp2el tinp_1)) == (ALg [(0,[(2,200),(3,100)]),(1,[(2,500)]),(2,[]),(3,[]),(4,[])])
tkonv_e = am2el (el2am (inp2el tinp_1)) == (ELg 4 [(0,200,2),(0,100,3),(1,500,2)])
tkonv_f = el2al (inp2el tinp_1) == (ALg [(0,[(3,100),(2,200)]),(1,[(2,500)]),(2,[]),(3,[]),(4,[])])
tkonv_g = el2am (inp2el tinp_1) == (AMg [[0,0,200,100,0],[0,0,500,0,0],[0,0,0,0,0],[0,0,0,0,0],[0,0,0,0,0]])


-- Aufgabe 3 ------------------------------------------------------------------------------

isNeighbourOf :: ELgraph -> Vertex -> Vertex -> Bool
isNeighbourOf (ELg _ []) _ _ = False
isNeighbourOf (ELg maxVert ((v1, cost, v2):xs)) vertex1 vertex2 = 
    if v1 == vertex1 && v2 == vertex2 then True 
    else isNeighbourOf (ELg maxVert xs) vertex1 vertex2

allNeighboursOf :: ELgraph -> Vertex -> [Vertex]
allNeighboursOf (ELg _ []) _ = []
allNeighboursOf (ELg maxVert ((v1, cost, v2):xs)) vertex =
    if v1 == vertex then 
        mergesort (v2 : (allNeighboursOf (ELg maxVert xs) vertex))
    else (allNeighboursOf (ELg maxVert xs) vertex) ++ []

mergesort [] = []
mergesort [x] = [x]
mergesort xs = let (as, bs) = splitAt (length xs `quot` 2) xs
               in merge (mergesort as) (mergesort bs)

merge [] ys = ys
merge xs [] = xs
merge (x:xs) (y:ys) = if x <= y
                      then x : merge xs (y:ys)
                      else y : merge (x:xs) ys

numberOfEdges :: AMgraph -> Integer
numberOfEdges (AMg []) = 0
numberOfEdges (AMg (x:xs)) = (count x) + (numberOfEdges (AMg xs))

count [] = 0
count (x:xs)
    | x == 0 = count xs
    | otherwise = 1 + (count xs)

-- ALg [(0,[(3,100),(2,200)]),(1,[(2,500)]),(2,[]),(3,[]),(4,[])]
-- ELg 4 [(1,500,2),(0,100,3),(0,200,2)]
-- AMg [[0,0,200,100,0],[0,0,500,0,0],[0,0,0,0,0],[0,0,0,0,0],[0,0,0,0,0]]
isOnCycle :: ALgraph -> Vertex -> Cost -> Bool
isOnCycle amGraph@(ALg ((v1, (x:xs)):vs)) vertex cost = True

foo (x:xs) alGraph = foo' (fst x) 

-- Test cases must be deleted ------------------------------------------
-- 3.
tfkt_all = [tfkt_a,tfkt_b,tfkt_c ]--,tfkt_d,tfkt_e,tfkt_f]

tfkt_a = isNeighbourOf (inp2el tinp_1) 0 2 == True
tfkt_b = allNeighboursOf (inp2el tinp_1) 0 == [2,3]
tfkt_c = numberOfEdges (el2am (inp2el tinp_1)) == 3
gcycle = ALg [(0,[]), (1,[(2,20)]), (2,[(3,30),(5,50)]), (3,[(0,500),(4,40)]), (4,[(1,10)])]
-- tfkt_d = isOnCycle gcycle 1 200 == True
-- tfkt_e = isOnCycle gcycle 2 50 == False
-- tfkt_f = isOnCycle gcycle 5 1000 == False

testfkt = and tfkt_all












