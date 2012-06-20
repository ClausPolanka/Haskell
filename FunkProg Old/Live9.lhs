\documentclass[a4paper,12pt]{scrartcl}

%die naechste zeile ist fuer lhs2TeX noetig
%include polycode.fmt
\usepackage[T1]{fontenc}
\usepackage[utf8x]{inputenc}
\usepackage[ngerman]{babel}

\usepackage{hyperref}
\usepackage{url}
\usepackage{marvosym}
\usepackage{verbatim}


\newcommand{\authormod}[2]{#1\\{\small#2}}

\newcommand{\hugs}{\texttt{hugs}}

\begin{document}

\author{
	\authormod{Bong Min Kim}{e0327177\MVAt student.tuwien.ac.at} \and
	\authormod{Christoph Sp\"ork}{christoph.spoerk\MVAt inode.at} \and
	\authormod{Florian Hassanen}{florian.hassanen\MVAt googlemail.com } \and
	\authormod{Bernhard Urban}{lewurm\MVAt gmail.com}
}

\subject{Haskell Live}

\title{[09] IO in Haskell und Aufgabenblatt 6}
\date{3. Dezember 2010}

\maketitle

\begin{comment}

\section*{Hinweise}

Diese Datei kann als sogenanntes ``Literate Haskell Skript'' von \texttt{hugs}
geladen werden, als auch per
\texttt{lhs2TeX}\footnote{\url{http://people.cs.uu.nl/andres/lhs2tex}} und
\LaTeX\ in ein Dokument umgewandelt werden.

\end{comment}
\begin{code}
import Data.List
import Data.Char
import System
\end{code}
\input{09hl_invalid}
\subsection*{IO Monaden}
\begin{code}
-- ab hier ausf\"uhrbare Beispiele!
io1 :: IO ()
io1 = putStr "hallo" >> putStrLn " welt :) (again)"

io2 :: IO ()
io2 = putStr "Name? "
	>> getLine
	>>= \a -> putStr "Alter? "
	>> getLine
	>>= \b -> putStr (a ++ " " ++ b ++ "\n")
-- Warum geht das? \texttt{>>} und \texttt{>>=} sind linksassoziativ,
-- Lambda aber rechtsassoziativ. Das ganze wird recht schnell un\"ubersichtlich,
-- daher verwendet man die sogenannte \texttt{do}-Notation:

io3 :: IO ()
io3 = do -- gleiche Funktionalit\"at wie in \texttt{io2}
	putStr "Name? "
	a <- getLine
	putStr "Alter? "
	b <- getLine
	putStr (a ++ " " ++ b ++ "\n")

-- Man kann auch eigene Funktionen im IO-Kontext definieren und verwenden:
myGetLine :: String -> IO String
myGetLine prefix = do
	x <- getLine
	return (prefix ++ x)

io4 :: IO ()
io4 = do
	putStr "-> "
	a <- myGetLine "foo"
	putStr a

-- Manchmal will man eine Liste von IO-Objekten verarbeiten:
main :: IO ()
main = do
	args <- getArgs
	let arg1 = if length args > 0 then args!!0 else "/proc/cpuinfo"
	content <- readFile arg1
	-- Funktionen (hier: \texttt{lines}) ohne \texttt{IO} auch verwendbar
	-- m\"uessen aber in einem \texttt{let}-Ausdruck stehen
	let alllines = lines content
	sequence_ [putStrLn i | i <- (take 5 alllines)]
\end{code}
Tipp: Mit \texttt{ghc -o 09hl 09hl.lhs} kann man aus diesem Haskellskript ein
ausf\"uhrbares Programm generieren lassen. Ausgef\"uhrt werden beim Aufruf vom
Programm \texttt{09hl} die Anweisungen in \texttt{main}.

\section*{Aufgabenblatt 6}

\begin{code}
type Vertex      = Integer
type Origin      = Vertex
type Destination = Vertex
type Key         = Integer
type Name        = Integer

\end{code}
\begin{code}

data BTree a = BLeaf Key a |
               BNode Key a (BTree a) (BTree a) deriving Show

data LTree a = LNode Key a [(LTree a)] deriving Show

data ALgraph = ALg [(Origin,[Destination])] deriving (Eq, Show)

\end{code}
\begin{code}

class Structure s where
    noOfSources :: s -> Integer
    noOfSinks   :: s -> Integer
    notSourceConnected :: s -> [Name]
    notSinkConnected :: s -> [Name]
\end{code}

\subsection*{instance eq btree}

\begin{code}

instance Eq a => Eq (BTree a) where
    (BLeaf _ a)       == (BLeaf _ a')         = (a == a')
    (BNode _ a t1 t2) == (BNode _ a' t1' t2') = (a == a' && t1 == t1' && t2 == t2')
    _                 == _                    = False

\end{code}

\subsection*{instance eq ltree}

\begin{code}

instance Eq a => Eq (LTree a) where
    (LNode _ a trees) == (LNode _ a' trees') = a == a' && (trees == trees')

\end{code}

\subsection*{instance of structure}

\subsubsection*{btree}

\begin{code}

b2l (BLeaf k v)       = (LNode k v [])
b2l (BNode k v t1 t2) = (LNode k v [b2l t1, b2l t2])

instance Structure (BTree a) where
    noOfSources t        = noOfSources        (b2l t)
    noOfSinks   t        = noOfSinks          (b2l t)
    notSourceConnected t = notSourceConnected (b2l t)
    notSinkConnected t   = notSinkConnected   (b2l t)

\end{code}

\subsubsection*{ltree}

\begin{code}

l2al tree = ALg (l2al' tree)

l2al' (LNode k v list) = [(k, children)] ++ (concat [ l2al' tree | tree <- list ])
                         where children = [ k | (LNode k _ _) <- list ]

instance Structure (LTree a) where
    noOfSources t        = noOfSources        (l2al t)
    noOfSinks   t        = noOfSinks          (l2al t)
    notSourceConnected t = notSourceConnected (l2al t)
    notSinkConnected t   = notSinkConnected   (l2al t)

\end{code}

\subsubsection*{algraph}

\begin{code}

instance Structure (ALgraph) where
    noOfSources g        = fromIntegral $ length( getSources g )
    noOfSinks   g        = fromIntegral $ length( getSinks   g )
    notSourceConnected g = (getNodes g) \\ (sourceConnected g)
    notSinkConnected g   = (getNodes g) \\ (sinkConnected g)

\end{code}
\begin{code}

getNodes (ALg al) = (sort . nub . concat) [ from : tos | (from, tos) <- al ] 

\end{code}
\begin{code}

getSources g@(ALg al) = 
  [ n | n <- nodes, not( n `elem` all_tos ) ]
  where nodes   = getNodes g
        all_tos = concat [ tos | (_, tos) <- al]

\end{code}
\begin{code}

getSinks g@(ALg al) = 
  [ n | n <- nodes, not( n `elem` all_froms ) ]
  where nodes     = getNodes g
        all_froms = [ from | (from, tos) <- al, tos /= [] ]

\end{code}
\begin{code}

type VertexMapping = ( Vertex -> [Vertex] )

forwardMapping :: ALgraph -> VertexMapping

forwardMapping (ALg []) _ = []
forwardMapping (ALg ((from, tos):rs)) x
 | x == from    = tos
 | otherwise    = forwardMapping (ALg rs) x

\end{code}
\begin{code}

reverseMapping :: ALgraph -> VertexMapping

reverseMapping (ALg []) _ = []
reverseMapping (ALg ((from, tos):rs)) x
 | x `elem` tos = from : (reverseMapping (ALg rs) x)
 | otherwise    = reverseMapping (ALg rs) x


\end{code}
\begin{code}

mappingMinus :: VertexMapping -> Vertex -> VertexMapping

mappingMinus mapping remove = (filter (remove /=)) . mapping


\end{code}
\begin{code}

(\\\) :: VertexMapping -> Vertex -> VertexMapping

(\\\) = mappingMinus

\end{code}
\begin{code}

(\\\\) :: VertexMapping -> [Vertex] -> VertexMapping

(\\\\) = foldl' (\\\) 

\end{code}
\begin{code}

sourceConnected g@(ALg al) = reachable (mapping \\\\ sources) sources
                             where sources = getSources g
                                   mapping = forwardMapping g

\end{code}
\begin{code}

sinkConnected g@(ALg al) = reachable (mapping \\\\ sinks) sinks
                           where sinks   = getSinks g
                                 mapping = reverseMapping g

\end{code}
\begin{code}

reachable :: VertexMapping -> [Vertex] -> [Vertex]

reachable mapping set 
  | neighbours /= [] = reachable (mapping \\\\ neighbours) (set ++ neighbours)
  | otherwise        = set
  where neighbours = nub [ n | x <- set, n <- mapping x]

\end{code}

\subsection*{accept}

\begin{code}

type State           = Integer
type StartState      = State
type AcceptingStates = [State]
type Word a          = [a]
type Row a           = [[a]]

\end{code}
\begin{code}

data AMgraph a = AMg [(Row a)] deriving (Eq, Show)

type Automaton a = AMgraph a

\end{code}
\begin{code}

type AutomataMapping a = State -> a -> [State]

amapping :: Eq a => (AMgraph a) -> (AutomataMapping a)

amapping (AMg matrix) from weight = neighbours
                                    where row        = matrix!!(fromIntegral from)
                                          size       = (length row) - 1
                                          neighbours = [ fromIntegral n | n <- [0 .. size], weight `elem` (row!!n) ]
\end{code}
\begin{code}

accept :: Eq a => (Automaton a) -> StartState -> AcceptingStates -> (Word a) -> Bool

accept auto start ends word =
    accept' (amapping auto) start ends word

\end{code}
\begin{code}

accept' :: Eq a => (AutomataMapping a) -> StartState -> AcceptingStates -> (Word a) -> Bool

accept' _       current ends []           = current `elem` ends
accept' mapping current ends (char:chars) =
    any (\next -> accept' mapping next ends chars) (mapping current char)


\end{code}

\subsection*{alternatives accept}
\begin{code}
accept2 :: Eq a => (Automaton a) -> StartState -> AcceptingStates -> (Word a) -> Bool
accept2 ma src sinks [] = src `elem` sinks
accept2 ma@(AMg matrix) src sinks (kante:xs) =
	or [ accept2 ma (fromIntegral y) sinks xs
	   | y <- [0..((length row)-1)]
	   , kante `elem` (row!!y)
	   ]
	where row = matrix!!(fromIntegral src)
\end{code}
\end{document}
