\documentclass[a4paper,12pt,landscape]{scrartcl}

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

\title{[10] Software Transactional Memory in Haskell,\\Tortenwurf und Aufgabenblatt 7}
\date{10. Dezember 2010}

\maketitle

\begin{comment}

\section*{Hinweise}

Diese Datei kann als sogenanntes ``Literate Haskell Skript'' von \texttt{hugs}
geladen werden, als auch per
\texttt{lhs2TeX}\footnote{\url{http://people.cs.uu.nl/andres/lhs2tex}} und
\LaTeX\ in ein Dokument umgewandelt werden.

\end{comment}
\section*{Software Transactional Memory}
Siehe \texttt{10stm.zip}. Entpacken und mit \texttt{make} builden. Man
ben\"otigt daf\"ur die Pakete \texttt{stm}, \texttt{network}
\texttt{regex-base} und \texttt{regex-compat} die man auf \url{www.haskell.org}
finden kann (falls sie nicht schon installiert sind). Danach kann man den server
mit \texttt{./server} in der Shell starten und sich mit \texttt{telnet
$<$hostaddr$>$ 8888} verbinden. Viel spass \texttt{:-)}

Ein \"au\ss{}erst lesenswertes Paper \"uber STM von Simon Peyton-Jones (Erfinder
von Haskell und GHC Entwickler):\\
\url{http://research.microsoft.com/en-us/um/people/simonpj/papers/stm/#beautiful}

\section*{Tortenwurf}
\begin{code}
import Data.List

type Person = Int; type Reihe = [Person]

permutation :: Reihe -> [Reihe]
permutation [] = [[]]
permutation xs = [x:ys | x <- xs, ys <- permutation (delete x xs)]

-- torten werfer
-- 1.Variante
cakeThrowerVisible1 :: Reihe -> [Person]
cakeThrowerVisible1 list = [p | i <- [0..l], let p = list!!i, (i == 0 || p > maximum (take i list))]
    where l = (length list) - 1

-- 2.Variante
cakeThrowerVisible2 :: Reihe -> [Person]
cakeThrowerVisible2 list =  cakeThrowerVisible2_ 0 list

cakeThrowerVisible2_ _ [] = []
cakeThrowerVisible2_ maxSoFar (p:ps) 
    | p > maxSoFar = p:(cakeThrowerVisible2_ p ps)
    | otherwise = cakeThrowerVisible2_ maxSoFar ps
                    
-- torten opfer
cakeVictimVisible1 :: Reihe -> [Person]
cakeVictimVisible1 list = cakeThrowerVisible1 (reverse list)

cakeVictimVisible2 :: Reihe -> [Person]
cakeVictimVisible2 list = cakeThrowerVisible2 (reverse list)

-- brute force solution
solveCakeCrime :: Int -> Int -> Int -> [Reihe]
solveCakeCrime n p r = [per | per <- (permutation [1..n]), 
                              length (cakeThrowerVisible2 per) == p,
                              length (cakeVictimVisible2 per) == r]
\end{code}
\section*{Aufgabenblatt 7}
\subsection*{\texttt{isPostfix}}
\begin{code}
type State = Integer; type StartState = State
type AcceptingStates = [State]
type Word a = [a]; type Row a = [[a]]

data AMgraph a = AMg [(Row a)] deriving (Eq,Show)
type Automaton a = AMgraph a

type Postfix a = Word a

isPostfix :: Eq a => (Automaton a) -> StartState -> AcceptingStates -> (Postfix a) -> Bool
isPostfix ma s end post =
	case givePrefix ma s end post of
		Nothing -> False
		Just _ -> True
\end{code}
\subsection*{\texttt{givePrefix}}
\begin{code}
type Prefix a = Word a

getTransitions :: Eq a => (Automaton a) -> State -> (Row a)
getTransitions (AMg rows) s = rows!!(fromIntegral s)

getStates :: Eq a => (Automaton a) -> [Integer]
getStates (AMg rows) = [0..(fromIntegral $ length rows - 1)]

givePrefix :: Eq a => (Automaton a) -> StartState -> AcceptingStates -> (Postfix a) -> (Maybe (Prefix a))
givePrefix ma s end post
	| ret == [] = Nothing
	| otherwise = Just $ head ret
	where ret = givePrefix' ma s end ((getStates ma)\\[s]) s [] post

givePrefix' :: Eq a => (Automaton a) -> StartState -> AcceptingStates -> [State] -> State -> (Word a) -> (Postfix a) -> [(Prefix a)]
givePrefix' ma s end tovisit akt word post
	| accept ma s end (word ++ post) = [word]
	| otherwise = concat
		[ concat
			[ givePrefix' ma s end newtv ii (word ++ [e]) post
			| e <- (transitions!!i) -- jede m\"ogliche Transition durchprobieren
			]
		| i <- [0..(length transitions)-1]
		, let ii = fromIntegral i
		, let newtv = tovisit\\[ii] -- den zu bearbeiteten Knoten im n\"achsten rekursiven Aufruf ausschlie\ss{}en
		, not $ null (transitions!!i) -- nur wenn es mind.\ einen \"Ubergang gibt
		, ii `elem` tovisit -- nur wenn dieser Knoten in der ``ToDo-Liste'' steht
		]
	where transitions = getTransitions ma akt

accept :: Eq a => (Automaton a) -> StartState -> AcceptingStates -> (Word a) -> Bool
accept ma src sinks [] = src `elem` sinks
accept ma@(AMg matrix) src sinks (kante:xs) =
	or [ accept ma (fromIntegral y) sinks xs
	   | y <- [0..((length row)-1)]
	   , kante `elem` (row!!y)
	   ]
	where row = matrix!!(fromIntegral src)
\end{code}
\subsection*{\texttt{traverse}}
\begin{code}
type Vertex = Integer; type Origin = Vertex; type Destination = Vertex

data ALbgraph a = ALbg [(Origin,a,[Destination])] deriving (Eq,Show)

traverse :: Eq a => (a -> a) -> (a -> Bool) -> (ALbgraph a) -> (ALbgraph a)
traverse _ _ (ALbg []) = ALbg []
traverse f p (ALbg ((k,a,dest):xs))
	| p a = ALbg ((k,(f a),dest):rek)
	| otherwise = ALbg ((k,a,dest):rek)
	where (ALbg rek) = traverse f p (ALbg xs)
\end{code}
\subsection*{\texttt{isWellColored}}
\begin{code}
data Color = Red | Blue | Green | Yellow deriving (Eq,Show)

data Ugraph = Ug [(Origin,Color,[Destination])] deriving (Eq,Show)

getOrigin :: Ugraph -> Origin -> (Origin,Color,[Destination])
getOrigin (Ug []) _ = error "Leerer Graph"
getOrigin (Ug ((o,c,d):xs)) oo
	| o == oo = (o,c,d)
	| otherwise = getOrigin (Ug xs) oo

getColor :: Ugraph -> Origin -> Color
getColor ug o = s2
	where (_,s2,_) = getOrigin ug o

isWellColored :: Ugraph -> Bool
isWellColored ug@(Ug list) = isWellColored' ug allorg
	where allorg = [o | (o,_,_) <- list]

isWellColored' :: Ugraph -> [Origin] -> Bool
isWellColored' _ [] = True
isWellColored' ug (o:orgs)
	| or [c == (getColor ug n) | n <- dests] = False
	| otherwise = isWellColored' ug orgs
	where (_,c,dests) = getOrigin ug o
\end{code}
\end{document}
