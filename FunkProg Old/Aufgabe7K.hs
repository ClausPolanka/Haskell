module Aufgabe7K where
{-
+-------------------------------------------------------+
|		      Aufgabenblatt 7			|
|			30.11.2010			|
|							|
|		Kamil Sierzant - 0526973		|
|		Claus Polanka  - 0225648		|
+-------------------------------------------------------+
-}
import Data.List

type State = Integer
type StartState = State
type AcceptingStates = [State]
type Word a = [a]
type Row a = [[a]]
data AMgraph a = AMg [(Row a)] deriving (Eq,Show)
type Automaton a = AMgraph a

type Postfix a = Word a

{-1. Schreiben Sie eine Haskell-Rechenvorschrift isPostfix mit der Signatur isPostfix :: Eq a =>
(Automaton a) -> StartState -> AcceptingStates -> (Postfix a) -> Bool. Angewendet auf
einen Automaten A, einen Anfangszustand s, eine Menge von Endzuständen E und ein Wort p,
ist das Resultat von isPostfix True, falls p Postfix eines von A bezüglich s und E akzeptierten
Wortes ist, sonst False.
-}

isPostfix :: Eq a => (Automaton a) -> StartState -> AcceptingStates -> (Postfix a) -> Bool
isPostfix automat start end word
	| (length rf) > 0 = True
	| otherwise = False
		where
			rf = [x | x <- rww, (reachableFrom automat start x) == True]
			rww = isReachableWithWord automat end word


reachableFrom :: Eq a => (Automaton a) -> State -> State -> Bool
reachableFrom automat start state = isReachable automat [start] [] state
			
-- Diese Methode wandert den Pfad vom Startzustand (3. Argument) entlang, bis
-- sie den gesuchten Zustand (4. Argument) erreicht hat. In diesem Fall wird
-- True zurückgegeben, ansonsten False. Meines erachtens wird das Durchwandern
-- hier mittels Breitensuche implementiert.
isReachable :: Eq a => (Automaton a) -> [State] -> [State] -> State -> Bool 
isReachable (AMg []) _ _ _ = False
isReachable _ [] visited state = elem state visited
isReachable automat todo@(x:xs) visited state -- Does todo mean something? 
	| (elem state n) == True = True -- One True is enough in that case.
	| otherwise = isReachable automat ((xs++n)\\(x:visited)) (x:visited) state
		where
			n = (getNeighboursAM automat x)

-- Diese Methode ermittelt die Nachbarzustände des übergebenen Ausgangszustands indem die entsprechende
-- Reihe des Ausgangszustands im AM-Graphen gewählt und die Kantenwerte überprüft werden. Ist die Wortlänge
-- größer Null gibt es einen Übergang zu einem Zustand der als Nachbarzustand zurückgegeben wird. 
getNeighboursAM :: Eq a => (Automaton a) -> State -> [State]
getNeighboursAM (AMg []) _ = []
getNeighboursAM (AMg rows) state = [x | let row = (!!) rows (fromInteger state), 
                                        i <- [0..((length row)-1)], 
                                        let edge = (!!) row i, 
                                        (length edge) > 0, 
                                        let x = toInteger i]

-- Diese Methode ermittelt die Liste aller Zustände, die aufgrund des übergebenen Wortes (z.B. "xb")
-- in einen Endszustand (z.B. [0, 1, 2]) gelangen können.
isReachableWithWord :: Eq a => (Automaton a) -> [State] -> (Postfix a) -> [State]  
isReachableWithWord _ states [] = states
isReachableWithWord automat states word = nub (isReachableWithWord automat 
                                                -- (isReachableWith automat states (last word)) instead of list comprehension?
                                                [x | let letter = last word, x <- isReachableWith automat states letter] 
                                                -- More readable? (init word))
                                                (take ((length word)-1) word))

-- Diese Methode ermittelt die Liste der Zustände die einen Übergang mit dem übergebenen Kantenwert (z.B. 'a')
-- zu einen der übergebenen Endzustände besitzen (z.B. [0, 1, 2]).
isReachableWith :: Eq a => (Automaton a) -> [State] -> a -> [State]
isReachableWith (AMg []) _ _ = []
isReachableWith _ [] _ = []
isReachableWith (AMg rows) states a = nub [ x | s <- states,
                                                i <- [0..((length rows)-1)], -- Indizes der Elemente in einer Row.
                                                let row = (!!) rows i, 
                                                let edge = (!!) row (fromInteger s), -- Kantenwert an Position des Endzustands.
                                                elem a edge == True, -- Erfolgskriterium: Wenn Kantenwert den 
                                                                     -- übergebenen Kantenwert enthält.
                                                let x = toInteger i ]

{-2. Schreiben Sie eine Haskell-Rechenvorschrift givePrefix mit der Signatur givePrefix :: Eq a =>
(Automaton a) -> StartState -> AcceptingStates -> (Postfix a) -> (Maybe (Prefix a)).
Angewendet auf einen Automaten A, einen Anfangszustand s, eine Menge von Endzuständen E und
ein Wort p, ist das Resultat von givePrefix Nothing, falls p kein Postfix eines von A bzgl. s und
E akzeptierten Wortes ist, ansonsten Just q, so dass die Konkatenation qp ein von A bzgl. s und
E akzeptierten Wortes ist. Beachten Sie, dass q i.a. nicht eindeutig bestimmt ist. Es reicht, wenn
Ihre Funktion ein gültiges Prefix q zu einem Postfix p bestimmt.
-}

type Prefix a = Word a
givePrefix :: Eq a => (Automaton a) -> StartState -> AcceptingStates -> (Postfix a) -> (Maybe (Prefix a))
givePrefix (AMg []) _ _ _ = Nothing
givePrefix automat start accept word
	| not $ isPostfix automat start accept word = Nothing
	| otherwise = Just edges
		where
			edges = getEdges automat path
			path = getPath automat start postfixstates
			postfixstates = isReachableWithWord automat accept word

-- Diese Methode such einen Pfad ausgehend vom übergebenen Startzustand (z.B. 0) hin zu einen der gültigen
-- Postfix-Zustände. Sobald der letzte Zustand des Pfads mit einem der gültigen Endzustände übereinstimmt,
-- wird der Pfad zurückgegeben.
-- Call: getPath (AMg [ (["","a","b","c"]), (["","","d","e"]), (["","","",""]), (["","","x",""]) ]) 0 [1] 
-- Ergebnis: [0, 0, 1]
getPath :: Eq a => (Automaton a) -> State -> [State] -> [State] 
getPath (AMg []) _ _ = []
getPath _ _ [] = []
getPath automat start accept = dfs [start] []
	where
		dfs :: [State] -> [State] -> [State]
		dfs [] visited = [start] ++ visited
		dfs (x:xs) visited
			| elem x accept = [start] ++ visited ++ [x]
			-- | elem x visited = dfs xs visited <== Better?
			| elem x visited = dfs xs visited -- Necessary if xs is passed to n in the next line instead of visited? (dfs (n ++ xs) ...
			-- | otherwise = dfs (n ++ xs) (visited ++ [x]) <== Better?
			| otherwise = dfs (n ++ visited) (visited ++ [x]) -- Why is visited in the first argument?
				where
					n = (getNeighboursAM automat x)
					
getEdges :: Eq a => (Automaton a) -> [State] -> [a]
getEdges (AMg []) _  = []
getEdges _ []  = []
getEdges automat@(AMg rows) (x:xs)
	| length xs == 0 = []
	| otherwise = [y | let row = (!!) rows (fromInteger x), 
                       let edge = (!!) row (fromInteger h), 
                       (length edge) > 0, 
                       let y = head edge] ++ (getEdges automat xs)
		where
			h = head xs

{-3. Schreiben Sie eine Haskell-Rechenvorschrift traverse mit Signatur traverse :: Eq a => (a ->
a) -> (a -> Bool) -> (ALbgraph a) -> (ALbgraph a). Angewendet auf eine Funktion f, ein
Prädikat p und einen ALb-Graphen G, besucht die Funktion traverse jeden Knoten k in G und
transformiert den a-Wert von k mit f, falls der a-Wert das Prädikat p erfüllt, ansonsten lässt sie
den a-Wert unverändert.
Angewendet auf die Nachfolgerfunktion (+1) und das Prädikat isOdd liefert die Funktion traverse
also einen ALb-Graphen zurück, in dem alle Schlüsselwerte gerade sind.
Sie können davon ausgehen, dass die Funktion traverse nur mit ALb-Graphen aufgerufen wird,
die der eingangs genannten Wohlgeformtheitsbedingung genügen.
-}

type Vertex = Integer
type Origin = Vertex
type Destination = Vertex
data ALbgraph a = ALbg [(Origin,a,[Destination])] deriving (Eq,Show)

traverse :: Eq a => (a -> a) -> (a -> Bool) -> (ALbgraph a) -> (ALbgraph a)
traverse _ _ (ALbg []) = ALbg []
traverse f p g = ALbg (traverse' f p g)

traverse' :: Eq a => (a -> a) -> (a -> Bool) -> (ALbgraph a) -> [(Origin,a,[Destination])]
traverse' _ _ (ALbg []) = []
traverse' f p original@(ALbg ((origin,v,destinations):nodes))
	| p v = [(origin, (f v), destinations)] ++ (traverse' f p (ALbg nodes))
	| otherwise = [(origin, v, destinations)] ++ (traverse' f p (ALbg nodes))

{-4. Schreiben Sie eine Haskell-Rechenvorschrift isWellColored mit Signatur isWellColored :: Ugraph
-> Bool. Angewendet auf einen ungerichteten Graphen G, ist das Resultat von isWellColored
True, falls G wohlgefärbt ist, sonst False.
Sie können davon ausgehen, dass die Funktion isWellColored nur mit U-Graphen aufgerufen wird,
die der oben genannten Wohlgeformtheitsbedingung genügen.
-}

data Color = Red | Blue | Green | Yellow deriving (Eq,Show)

data Ugraph = Ug [(Origin,Color,[Destination])] deriving (Eq,Show)

isWellColored :: Ugraph -> Bool
isWellColored (Ug []) = True
isWellColored g@(Ug nodes)
	| length n > 0 = False
	| otherwise = True
		where 
            n = [x | originTupel <- nodes, let (origin, color, destinations) = originTupel, let equalColors = getEqualColors g color destinations, length equalColors > 0, let x = originTupel]
        
getEqualColors :: Ugraph -> Color -> [Destination] -> [Origin]
getEqualColors (Ug []) _ _ = []
getEqualColors _ _ [] = []
getEqualColors (Ug nodes) color destinations =
	[origin | (origin,oColor,oDest) <- nodes,
              elem origin destinations, 
              color == oColor]

