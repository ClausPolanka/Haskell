+-------------------------------------------------------+
|		      Aufgabenblatt 6			|
|			23.11.2010			|
|							|
|		Kamil Sierzant - 0526973		|
|		Claus Polanka  - 0225648		|
+-------------------------------------------------------+

> import Data.List

> type Vertex = Integer
> type Origin = Vertex
> type Destination = Vertex
> type Key = Integer
> type Name = Integer
> data BTree a = BLeaf Key a | BNode Key a (BTree a) (BTree a) deriving Show
> data LTree a = LNode Key a [(LTree a)] deriving Show
> data ALgraph = ALg [(Origin,[Destination])] deriving (Eq,Show)

und die Typklasse Structure

> getNoOfSinks :: [LTree a] -> Integer
> getNoOfSinks [] = 0
> getNoOfSinks [(LNode k a [])] = 1
> getNoOfSinks [(LNode k a t)] = getNoOfSinks [head t] + getNoOfSinks (tail t)
> getNoOfSinks a = getNoOfSinks [head a] + getNoOfSinks (tail a)

> class Structure s where
>	noOfSources :: s -> Integer
>	noOfSinks :: s -> Integer
>	notSourceConnected :: s -> [Name]
>	notSinkConnected :: s -> [Name]

1. Machen Sie die Typen BTree a und LTree a mithilfe expliziter Instanzdeklarationen instance Eq
   a => Eq (BTree a)... und instance Eq a => Eq (LTree a)... zu Instanzen der Typklasse Eq.
   B-B¨aume bzw. L-B¨aume sind gleich, wenn sie in Struktur und a-Wert ¨ubereinstimmen. Der Key-
   Wert soll f¨ur den Gleichheitstest keine Rolle spielen.

> instance Structure (BTree a) where 
> 	noOfSources a = 1
> 	noOfSinks (BLeaf _ a) = 1
> 	noOfSinks (BNode _ a x y) = noOfSinks(x) + noOfSinks(y)
> 	notSourceConnected a = []
> 	notSinkConnected a = []

> instance Structure (LTree a) where
> 	noOfSources a = 1
> 	noOfSinks (LNode _ a []) = 1
> 	noOfSinks a = getNoOfSinks [a]
> 	notSourceConnected a = []
> 	notSinkConnected a = []

> instance (Eq a) => Eq (BTree a) where
> 	(==)	(BLeaf key1 x) (BLeaf key2 y) = (x == y)
> 	(==)	(BNode key1 x t1 t2) (BNode key2 y u1 u2) = (x == y) && (t1 == u1) && (t2 == u2)
>		&& noOfSources(BNode key1 x t1 t2) == noOfSources(BNode key2 y u1 u2)
>		&& noOfSinks(BNode key1 x t1 t2) == noOfSinks(BNode key2 y u1 u2)
>		&& notSourceConnected(BNode key1 x t1 t2) == notSourceConnected(BNode key2 y u1 u2)
>		&& notSinkConnected(BNode key1 x t1 t2) == notSinkConnected(BNode key2 y u1 u2)	
> 	(==)	_ _ = False

> instance (Eq a) => Eq (LTree a) where
> 	(==)	(LNode key1 x t1) (LNode key2 y t2) = (x == y) && (t1 == t2)
>		&& noOfSources(LNode key1 x t1) == noOfSources(LNode key2 y t2)
>		&& noOfSinks(LNode key1 x t1) == noOfSinks(LNode key2 y t2)
>		&& notSourceConnected(LNode key1 x t1) == notSourceConnected(LNode key2 y t2)
>		&& notSinkConnected(LNode key1 x t1) == notSinkConnected(LNode key2 y t2)
> 	(==)	_ _ = False

> instance Structure (ALgraph) where
> 	noOfSources g = toInteger(length l)
>		where
>			l = [x | x <- (allOrigins g), (isDestination g x == False)]

> 	noOfSinks g = noOfSinksAlg (ALg (runGetCompleteALgraph g))

>	notSourceConnected g = sort ( nub ([x | x <- allNodes, elem x rn == False]))
>		where
>			allNodes = getAllNodes g
>			cg = (ALg (runGetCompleteALgraph g))
>			sources = getAllSources g
>			rn = getReachableNodesForSources cg sources

>	notSinkConnected g = sort (nub (notSinkConnectedAlg cg cg (sinks)))
>		where
>			cg = (ALg (runGetCompleteALgraph g))
>			sinks = getAllSinks cg



> getReachableNodesForSources :: ALgraph -> [Vertex] -> [Vertex] 
> getReachableNodesForSources (ALg []) s = s
> getReachableNodesForSources _ [] = []
> getReachableNodesForSources g (x:xs) = 
>	nub ((getReachableNodes g x) ++ (getReachableNodesForSources g xs))

> getAllNodes :: ALgraph -> [Vertex]
> getAllNodes (ALg []) = []
> getAllNodes (ALg ((o, dests):xs)) = nub ((o : dests) ++ (getAllNodes (ALg xs)))

> getAllSources :: ALgraph -> [Vertex]
> getAllSources g = [x | x <- (allOrigins g), (isDestination g x == False)]

> notSinkConnectedAlg :: ALgraph -> ALgraph -> [Vertex] -> [Name] 
> notSinkConnectedAlg _ (ALg []) _ = []
> notSinkConnectedAlg g (ALg ((o, dests):xs)) sinks
>	| (intersect rn sinks) == [] = o : (notSinkConnectedAlg g (ALg xs) sinks)
>	| otherwise = (notSinkConnectedAlg g (ALg xs) sinks)
>		where
>			rn = getReachableNodes g o
> notSinkConnectedAlg _ _ _ = []

> getReachableNodes :: ALgraph -> Vertex -> [Vertex] 
> getReachableNodes (ALg []) _ = []
> getReachableNodes g s = 
>	nub (getReachableNodesBFS g (getNeighbours g s) [s])
>		where
>			getReachableNodesBFS g [] v = v
>			getReachableNodesBFS g (x:xs) v
>				| elem x v == True = getReachableNodesBFS g xs v
>				| otherwise = getReachableNodesBFS g (nub (xs ++ (getNeighbours g x))) (x:v)

> getNeighbours :: ALgraph -> Vertex -> [Vertex]
> getNeighbours (ALg []) _ = []
> getNeighbours (ALg ((o, dests):xs)) s
>	| o == s = dests
>	| otherwise = getNeighbours (ALg xs) s

> getAllSinks :: ALgraph -> [Vertex] 
> getAllSinks (ALg []) = []
> getAllSinks (ALg [(o, [])]) = [o]
> getAllSinks (ALg ((o, d):xs))
>	| d == [] = o : getAllSinks (ALg xs)
>	| otherwise = getAllSinks (ALg xs)

> noOfSinksAlg :: ALgraph -> Integer 
> noOfSinksAlg (ALg []) = 0
> noOfSinksAlg (ALg [(_, [])]) = 1
> noOfSinksAlg (ALg ((o, d):xs))
>	| length d == 0 = 1 + noOfSinksAlg (ALg xs)
>	| otherwise = noOfSinksAlg (ALg xs)

> isDestination :: ALgraph -> Origin -> Bool
> isDestination (ALg []) _ = False
> isDestination (ALg ((o, dests):xs)) origin
> 	| elem origin dests == True = True
>	| otherwise = isDestination (ALg xs) origin

> runGetCompleteALgraph :: ALgraph -> [(Origin, [Destination])]
> runGetCompleteALgraph g = getCompleteALgraph g origins
>	where
>		origins = allOrigins g

> getCompleteALgraph :: ALgraph -> [Origin] -> [(Origin, [Destination])]
> getCompleteALgraph (ALg []) _ = []
> getCompleteALgraph (ALg ((o, dests):xs)) origins =
>	(o, dests) : newEntries ++ (getCompleteALgraph (ALg xs) newOrigins)
>	where
>		newEntries = [(x, []) | x <- dests, (not (any (x==) origins))]
>		newOrigins = nub ((allOrigins (ALg xs)) ++ origins)

> allOrigins :: ALgraph -> [Origin]
> allOrigins (ALg list) = [o | (o, l) <- list]


> type State = Integer
> type StartState = State
> type AcceptingStates = [State]
> type Word a = [a]
> type Row a = [[a]]
> data AMgraph a = AMg [(Row a)] deriving (Eq,Show)
> type Automaton a = AMgraph a

> accept :: Eq a => (Automaton a) -> StartState -> AcceptingStates -> (Word a) -> Bool
> accept (AMg []) _ _ _ = False
> accept g@(AMg rows) s a w
>	| (fromInteger s) >= (length rows) || s < 0 = False
>	| (inGraphAS g a) == False = False
>	| (intersect a t) == [] = False
>	| otherwise = True
>		where
>			t = tryAccept g s w 


> tryAccept :: Eq a => (AMgraph a) -> State -> (Word a) -> [State]
> tryAccept (AMg []) _ _ = []
> tryAccept _ s [] = [s]
> tryAccept g s (x:xs) =
>	 [y | z <- n, y <- (tryAccept g z xs)]
>		where
>			n = (getNeighboursAM g s x)
> tryAccept _ _ _ = []

> inGraphAS :: (AMgraph a) -> AcceptingStates -> Bool
> inGraphAS (AMg []) _ = False
> inGraphAS _ [] = False
> inGraphAS g@(AMg rows) (x:xs)
>	| (fromInteger x) >= (length rows) || x < 0 = inGraphAS g xs
>	| otherwise = True

> getNeighboursAM :: Eq a => (AMgraph a) -> State -> a -> [State]
> getNeighboursAM (AMg []) _ _ = []
> getNeighboursAM (AMg rows) s a = 
> 	getNeighboursAMRow ((!!) rows (fromInteger s)) 0 a
>		where
>			getNeighboursAMRow [] _ _ = []
>			getNeighboursAMRow (x:xs) j a
>				| elem a x == True = j : (getNeighboursAMRow xs (j+1) a)
>				| otherwise = getNeighboursAMRow xs (j+1) a

