--------------------------------------------------------------------------------
-- 6. Aufgabenblatt zu Funktionale Programmierung vom 16.11.2010
-- 
-- Gruppe: 145
-- Mitglieder: Felix MAYERHUBER (0825283) und Dominik RAUCH (0825084)
-- 
-- Created: 16.11.2010
--------------------------------------------------------------------------------

> module Aufgabe6 where

> import List
> import Maybe

--------------------------------------------------------------------------------
-- Teil 1
-- 
--------------------------------------------------------------------------------

> type Vertex = Integer
> type Origin = Vertex
> type Destination = Vertex
> type Key = Integer
> type Name = Integer

> data BTree a = BLeaf Key a |
>				 BNode Key a (BTree a) (BTree a) deriving Show

> data LTree a = LNode Key a [(LTree a)] deriving Show

> data ALgraph = ALg [(Origin,[Destination])] deriving (Eq,Show)

> class Structure s where
>	noOfSources :: s -> Integer
>	noOfSinks :: s -> Integer
>	notSourceConnected :: s -> [Name]
>	notSinkConnected :: s -> [Name]

--------------------------------------------------------------------------------
-- 6.1
-- 
--------------------------------------------------------------------------------

> instance Eq a => Eq (BTree a) where
>	(==) (BLeaf _ a) (BLeaf _ b) = a == b
>	(==) (BNode _ a w x) (BNode _ b y z) = a == b && w == y && x == z
> 	(==) _ _ = False

> instance Eq a => Eq (LTree a) where
>	(==) (LNode _ a xs) (LNode _ b ys) = a == b && length xs == length ys && and (map (\(x,y) -> x==y) (zip xs ys))

--------------------------------------------------------------------------------
-- 6.2
-- 
--------------------------------------------------------------------------------
 
> instance Structure (BTree s) where
>	noOfSources _ = 1
>	noOfSinks (BLeaf _ _) = 1
>	noOfSinks (BNode _ _ x y) = noOfSinks x + noOfSinks y
>	notSourceConnected _ = []
>	notSinkConnected _ = []
 
> instance Structure (LTree s) where
>	noOfSources _ = 1
>	noOfSinks (LNode _ _ []) = 1
>	noOfSinks (LNode _ _ xs) = sum (map noOfSinks xs)
>	notSourceConnected _ = []
>	notSinkConnected _ = []
 
> instance Structure ALgraph where
>	noOfSources = alNoOfSources
>	noOfSinks = alNoOfSinks
>	notSourceConnected = alNotSourceConnected
>	notSinkConnected = alNotSinkConnected

-- ALgraph noOfSources

> alNoOfSources :: ALgraph -> Integer
> alNoOfSources (ALg []) = 0
> alNoOfSources (ALg xs) = toInteger (length (filter id (map (\x -> notElem x dests) origins)))
>        where
>        (origins, dests) = getStructure (ALg xs)

> getStructure :: ALgraph -> ([Origin],[Destination])
> getStructure (ALg []) = ([],[])
> getStructure (ALg ((origin,dests):xs)) = ( (origin:origins), dests ++ dests' )
>         where
>        (origins,dests') = getStructure (ALg xs)

-- ALgraph noOfSinks

> alNoOfSinks :: ALgraph -> Integer
> alNoOfSinks (ALg []) = 0
> alNoOfSinks (ALg xs) = toInteger (length (filter id (map (\x -> notElem x realOrigins) (dests++emptyOrigins))))
>        where
>        (realOrigins, emptyOrigins, dests) = getStructure2 (ALg xs)

> getStructure2 :: ALgraph -> ([Origin],[Origin],[Destination]) -- Info: realOrigins = (origin,[Destinations]), emptyOrigins = (origin,[]), dests' = all destinations
> getStructure2 (ALg []) = ([],[],[])
> getStructure2 (ALg ((origin,[]):xs)) = (realOrigins, (origin:emptyOrigins), dests')
>        where
>        (realOrigins, emptyOrigins, dests') = getStructure2 (ALg xs)
> getStructure2 (ALg ((origin,dests):xs)) = ((origin:realOrigins), emptyOrigins, dests ++ dests')
>        where
>        (realOrigins, emptyOrigins, dests') = getStructure2 (ALg xs)

-- ALgraph notSourceConnected

> alSources :: ALgraph -> [Origin]
> alSources (ALg []) = []
> alSources (ALg xs) = filter (>=0)(map (\x -> if(notElem x dests) then x else -1) origins)
>		where
>		(origins, dests) = getStructure (ALg xs)

> alNotSourceConnected :: ALgraph -> [Name]
> alNotSourceConnected graph
> 	| graph == (ALg []) = []
>	| alSources graph == [] = origins
>	| otherwise = filter (>=0) (map (\x -> if(notElem x (sort (nub (concat [o:testIfSourceConnected graph o [o] (getVertexNeighbors graph o) | o <- alSources graph])))) then x else -1) origins)
>		where
>		(origins) = getAllAlgraphOrigins graph

> testIfSourceConnected :: ALgraph -> Origin -> [Origin] -> [Destination] -> [Origin]
> testIfSourceConnected graph start past [] = []
> testIfSourceConnected graph start past (x:[]) = if x == start then [] else 
>						if(elem x past) then [] else
>						if(getVertexNeighbors graph x == []) then [x] 
>						else [x] ++ testIfSourceConnected graph start (past++[x]) (getVertexNeighbors graph x)
> testIfSourceConnected graph start past (x:xs) = if x == start then [] else 
>						if(elem x past) then [] else
>						if(getVertexNeighbors graph x == []) then [x] ++ testIfSourceConnected graph start past xs 
>						else [x] ++ testIfSourceConnected graph start (past++[x]) (getVertexNeighbors graph x) ++ testIfSourceConnected graph start past xs

> getVertexNeighbors :: ALgraph -> Vertex -> [Destination]
> getVertexNeighbors (ALg ((a,b):xs)) vert
>	| xs == [] = if(a==vert) then b else []
>	| otherwise = if(a==vert) then b ++ nextVertex else nextVertex
>		where nextVertex = getVertexNeighbors (ALg xs) vert

> getAllAlgraphOrigins :: ALgraph -> [Origin]
> getAllAlgraphOrigins graph = nub [o | (o,l) <- getCompleteALgraph graph]

> getCompleteALgraph :: ALgraph -> [(Origin, [Destination])]
> getCompleteALgraph (ALg []) = []
> getCompleteALgraph (ALg ((o, dests):xs)) =
>	(o, dests) : newEntries ++ (getCompleteALgraph (ALg xs))
>	where
>		newEntries = [(x, []) | x <- dests, (not (any (x==) origins))]
>		origins = allOrigins (ALg xs)

> allOrigins :: ALgraph -> [Origin]
> allOrigins (ALg list) = [o | (o, l) <- list]

-- ALgraph notSinkConnected

> alNotSinkConnected :: ALgraph -> [Name]
> alNotSinkConnected (ALg []) = []
> alNotSinkConnected graph = sort $ filter (\x -> noPath graph x alsinks []) alnodes -- Nur Knoten sind notSinkConnected von denen es keinen Pfad vom Knoten zu einem Sink gibt
>	where
>	alsinks = alSinks graph
>	alnodes = (alNodes graph) \\ alsinks -- Sinks sind niemals notSinkConnected, daher gleich abschneiden

> alSinks :: ALgraph -> [Vertex] -- Liefert alle Sinks im Graphen
> alSinks (ALg []) = []
> alSinks (ALg xs) = emptyOrigins ++ filter (`notElem` realOrigins) dests
>        where
>        (realOrigins, emptyOrigins, dests) = getStructure2 (ALg xs)

> alNodes :: ALgraph -> [Vertex] -- Liefert überhaupt alle Knoten im Graphen
> alNodes (ALg []) = []
> alNodes (ALg ((origin,dests):xs)) = nub ((origin:dests) ++ (alNodes (ALg xs)))

> noPath :: ALgraph -> Origin -> [Destination] -> [Origin] -> Bool
> noPath (ALg []) _ _ _ = True
> noPath graph o dests visitedBefore
>	| elem o dests = False -- Sind wir an einem Sink angekommen so gibt es einen Pfad vom Knoten zum Sink -> noPath=False
>	| otherwise = and $ map (\x -> noPath graph x dests visited) outgoingNeighbours -- Rekursiv für alle Nachfolgeknoten noPath aufrufen, ein einziges noPath=False reicht damit noPath gesamt False ist
>		where
>		visited = (o:visitedBefore)
>		outgoingNeighbours = (outgoingNeighbours' graph o) \\ visited

> outgoingNeighbours' :: ALgraph -> Origin -> [Origin] -- Liefert alle (ausgehenden) Nachbarn eines Knotens, jeder Knoten ist garantiert nur einmal im Ergebnis (wichtig für \\ falls sie gemein sind und ausgehende Knoten mehrmals eintragen!)
> outgoingNeighbours' (ALg xs) o = nub $ snd $ fromMaybe (-1,[]) $ find (\x -> o==fst(x)) xs -- Knoten Suchen, Maybe abschneiden (für Nothing: [] einsetzen), Destinations (snd) zurückgeben, evtl. doppelt eingetragene Knoten entfernen

--------------------------------------------------------------------------------
-- Teil 2
-- 
--------------------------------------------------------------------------------

> type State = Integer
> type StartState = State
> type AcceptingStates = [State]
> type Word a = [a]
> type Row a = [[a]]

> data AMgraph a = AMg [(Row a)] deriving (Eq,Show)

> type Automaton a = AMgraph a

--------------------------------------------------------------------------------
-- 6.3
-- 
--------------------------------------------------------------------------------

> length' :: [a] -> Integer
> length' xs = toInteger (length xs)

> accept :: Eq a => (Automaton a) -> StartState -> AcceptingStates -> (Word a) -> Bool
> accept (AMg a) s es ws
>	| s < 0 || s >= length' a = False -- Ist Startknoten kein Knoten des Graphen => False
>	| and (map (\x -> x < 0 || x >= length' a) es) = False -- Enthalten die Endzustände keinen einzigen Knoten der auch im Graphen ist => False
>	| otherwise = accept' (AMg a) s es ws -- Otherwise => Automaten ablaufen lassen

> accept' :: Eq a => (AMgraph a) -> StartState -> AcceptingStates -> (Word a) -> Bool
> accept' _ s es [] = elem s es -- Wort abgearbeitet => Derzeitiger Zustand in den Endzuständen?
> accept' (AMg rows) s es (w:ws) = 
>	let
>	successors = map (\cell -> elem w cell) (rows!!(fromInteger s)) -- Gibt eine Liste [Bool] zurück, True falls möglicher Nachfolger, False anderenfalls
>	successorIndices = findIndices id successors -- Indices dieser möglichen Nachfolgerknoten (= alle möglichen Startknoten für den nächsten Schritt); können mehrere sein weil Automat nicht-deterministisch ist!
>	nextStepResults = map (\x -> accept' (AMg rows) (toInteger x) es ws) successorIndices -- Rekursiver Aufruf für alle möglichen Nachfolger
>	in
>	any id nextStepResults -- Eine einzige Möglichkeit die zum Endzustand geführt hat reicht

--------------------------------------------------------------------------------
