--------------------------------------------------------------------------------
-- 7. Aufgabenblatt zu Funktionale Programmierung vom 23.11.2010
-- 
-- Gruppe: 145
-- Mitglieder: Felix MAYERHUBER (0825283) und Dominik RAUCH (0825084)
-- 
-- Created: 24.11.2010
--------------------------------------------------------------------------------

module Aufgabe7 where

import Prelude;
import List;

--------------------------------------------------------------------------------
-- 7.1
-- 
--------------------------------------------------------------------------------

type State = Integer
type StartState = State
type AcceptingStates = [State]
type Word a = [a]
type Row a = [[a]]

data AMgraph a = AMg [(Row a)] deriving (Eq,Show)

type Automaton a = AMgraph a

type Postfix a = Word a

isPostfix :: Eq a => (Automaton a) -> StartState -> AcceptingStates -> (Postfix a) -> Bool
isPostfix a s es ps = fst $ prefixPostfix a s es ps

--------------------------------------------------------------------------------
-- 7.2
-- 
--------------------------------------------------------------------------------

type Prefix a = Word a

givePrefix :: Eq a => (Automaton a) -> StartState -> AcceptingStates -> (Postfix a) -> (Maybe (Prefix a))
givePrefix a s es ps = if ispostfix then (Just prefix) else Nothing
	where
	(ispostfix,prefix) = prefixPostfix a s es ps

-- Liefert (True, Prefix) falls das Postfix im Sinne der Angabe möglich ist, anderenfalls (False,[])
prefixPostfix :: Eq a => (Automaton a) -> StartState -> AcceptingStates -> (Postfix a) -> (Bool, (Prefix a))
prefixPostfix a s es ps = if length positiveResults > 0 then head positiveResults else (False,[])
	where
	allValidStarts = ((s,[]):(allValidStartsForPostfix a s [] []))
	acceptFromHere x = accept a x es ps
	results = map (\(x,p) -> (acceptFromHere x, p)) allValidStarts
	positiveResults = filter (\(b,p) -> b) results

-- Liefert length als Integer statt Int
length' :: [a] -> Integer
length' xs = toInteger (length xs)

-- Liefert alle Knoten an denen das Postfix gültigerweise beginnen dürfte (d.h. Knoten die vom Startknoten aus erreichbar sind) + das Prefix um vom gegebenen Startknoten zu diesem Knoten zu kommen
allValidStartsForPostfix :: Eq a => (Automaton a) -> StartState -> [State] -> (Prefix a) -> [(State, (Prefix a))]
allValidStartsForPostfix a@(AMg rows) s vs oldprefix = successorsWithPrefixes ++ concatMap (\(x,xprefix) -> allValidStartsForPostfix a x newvs xprefix) successorsWithPrefixes
	where
	newvs = (s:vs) -- Alle bereits besuchten Knoten
	row = rows!!(fromInteger s) -- Die Row des derzeitigen s = alle Knoten und wie sie (möglicherweise) von s aus direkt erreicht werden können
	successors = (map toInteger $ findIndices id $ map (\cell -> length' cell > 0) row) \\ newvs -- Alle Knoten die unbesuchte, über ein Wort direkt erreichbare Nachfolger von s sind
	successorsWithPrefixes = concatMap (\x -> allFor a s x oldprefix) successors

allFor :: Eq a => (Automaton a) -> StartState -> State -> (Prefix a) -> [(State, (Prefix a))]
allFor (AMg rows) from to oldprefix = map (\x -> (to,oldprefix ++ [x])) transitions
	where
	transitions = (rows!!(fromInteger from))!!(fromInteger to)

-- Accept aus Aufgabe6
accept :: Eq a => (Automaton a) -> StartState -> AcceptingStates -> (Word a) -> Bool
accept (AMg a) s es ws
	| s < 0 || s >= length' a = False -- Ist Startknoten kein Knoten des Graphen => False
	| and (map (\x -> x < 0 || x >= length' a) es) = False -- Enthalten die Endzustände keinen einzigen Knoten der auch im Graphen ist => False
	| otherwise = accept' (AMg a) s es ws -- Otherwise => Automaten ablaufen lassen

accept' :: Eq a => (AMgraph a) -> StartState -> AcceptingStates -> (Word a) -> Bool
accept' _ s es [] = elem s es -- Wort abgearbeitet => Derzeitiger Zustand in den Endzuständen?
accept' (AMg rows) s es (w:ws) = 
	let
	successors = map (\cell -> elem w cell) (rows!!(fromInteger s)) -- Gibt eine Liste [Bool] zurück, True falls möglicher Nachfolger, False anderenfalls
	successorIndices = findIndices id successors -- Indices dieser möglichen Nachfolgerknoten (= alle möglichen Startknoten für den nächsten Schritt); können mehrere sein weil Automat nicht-deterministisch ist!
	nextStepResults = map (\x -> accept' (AMg rows) (toInteger x) es ws) successorIndices -- Rekursiver Aufruf für alle möglichen Nachfolger
	in
	any id nextStepResults -- Eine einzige Möglichkeit die zum Endzustand geführt hat reicht
	
--------------------------------------------------------------------------------
-- 7.3
-- 
--------------------------------------------------------------------------------

type Vertex = Integer
type Origin = Vertex
type Destination = Vertex

data ALbgraph a = ALbg [(Origin,a,[Destination])] deriving (Eq,Show)

-- (a -> a) ist eine Funktion wie zum Beispiel (+1),(*2), ...
-- (a -> Bool) ist ein Prädikat . zum Beispiel isOdd, (>1), ...

traverse :: Eq a => (a -> a) -> (a -> Bool) -> (ALbgraph a) -> (ALbgraph a)
traverse func praed g@(ALbg graph) = (ALbg [if(praed value) then (node,(func value),dests) else (node,value,dests) | (node,value,dests) <- graph])

-- TRYOUT FELIX:
--traverse func praed (ALbg ((node,value,dests):[])) = if(praed value) then (ALbg [(node,(func value),dests)]) else (ALbg [(node,value,dests)])
--traverse func praed (ALbg ((node,value,dests):xs)) = if(praed value) then (ALbg [(node,(func value),dests)]) ++ traverse func praed (ALbg xs) else (ALbg [(node,value,dests)]) ++ traverse func praed xs

--traverse func praed graph (ALbg ((node,value,dests):xs))
--	| xs == [] = if(praed value) then (ALbg [(node,(func value), dests)]) else (ALbg[(node,value,dests)])
--	| praed value = [(node,(func value),dests)] ++ traverse func praed (ALbg xs)
--	| otherwise = [(node,value,dests)] ++ traverse func praed (ALbg xs)

-- TRYOUT DOMINIK:
-- noch kürzer aber schlechter lesbar:
-- traverse f p (ALbg g) = (ALbg (map (\x@(o,v,ds) -> if p v then (o,f v,ds) else x) g))

--------------------------------------------------------------------------------
-- 7.4
-- 
--------------------------------------------------------------------------------

data Color = Red | Blue | Green | Yellow deriving (Eq,Show)

data Ugraph = Ug [(Origin,Color,[Destination])] deriving (Eq,Show)

isWellColored :: Ugraph -> Bool
isWellColored l@(Ug graph) = and $ [ isWellColoredHelper l v c | (v,c,d) <- graph ]

isWellColoredHelper :: Ugraph -> Vertex -> Color -> Bool
isWellColoredHelper graph node color = and $ map (\x -> x /= color) (getVertexNeighborcolors graph (getVertexNeighbors graph node))

getVertexNeighbors :: Ugraph -> Vertex -> [Vertex]
getVertexNeighbors (Ug ((a,c,b):xs)) vert
	| xs == [] = if(a==vert) then b else []
	| otherwise = if(a==vert) then b ++ nextVertex else nextVertex
		where nextVertex = getVertexNeighbors (Ug xs) vert

getVertexNeighborcolors :: Ugraph -> [Vertex] -> [Color]
getVertexNeighborcolors (Ug ((a,c,b):xs)) vert
	| xs == [] = if(elem a vert) then [c] else []
	| otherwise = if(elem a vert) then [c] ++ nextVertex else nextVertex
		where nextVertex = getVertexNeighborcolors (Ug xs) vert

--------------------------------------------------------------------------------
