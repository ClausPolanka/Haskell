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

\begin{document}

\author{
	\authormod{Bong Min Kim}{e0327177\MVAt student.tuwien.ac.at} \and
	\authormod{Christoph Sp\"ork}{christoph.spoerk\MVAt inode.at} \and
	\authormod{Florian Hassanen}{florian.hassanen\MVAt googlemail.com } \and
	\authormod{Bernhard Urban}{lewurm\MVAt gmail.com}
}

\subject{Haskell Live}

\title{[06] Aufgabenblatt 3 \& CityMaut}
\date{12. November 2010}

\maketitle

\begin{comment}

\section*{Hinweise}

Diese Datei kann als sogenanntes ``Literate Haskell Skript'' von \texttt{hugs}
geladen werden, als auch per
\texttt{lhs2TeX}\footnote{\url{http://people.cs.uu.nl/andres/lhs2tex}} und
\LaTeX\ in ein Dokument umgewandelt werden.

\end{comment}

\section*{Aufgabenblatt 3}

M\"ogliche L\"osungswege f\"ur die Aufgaben 3. \"Ubungsblatts:

\begin{code}

import Data.Char
import Data.List

\end{code}
\subsection*{dreiNplusEins}
\begin{code}

dreiNplusEins :: Integer -> [Integer]

dreiNplusEins 1 = [1]
dreiNplusEins x = x:(dreiNplusEins next)
                  where
                    next = if   x `mod` 2 == 0
                           then x `div` 2
                           else ( x * 3 ) + 1

\end{code}
\subsection*{maxZyklus}
\begin{code}

-- wrapper for correct type
mylen :: [a] -> Integer
mylen = fromIntegral . length

\end{code}
\begin{code}

type UntereGrenze = Integer
type ObereGrenze = Integer
type MaxZykLaenge = Integer

\end{code}
\begin{code}

maxZyklus :: UntereGrenze -> ObereGrenze -> (UntereGrenze,ObereGrenze,MaxZykLaenge)
maxZyklus m n
    | m > n     = (m,n,0)
    | otherwise = (m,n,maxLength)
    where 
        maxLength = maximum [ length | i <- [m..n] , let length = mylen( dreiNplusEins i ) ]

\end{code}
\begin{code}

-- another solution (which can exceed the 4000 limit by balancing the search tree)
maxZyklusBalanced :: UntereGrenze -> ObereGrenze -> (UntereGrenze,ObereGrenze, MaxZykLaenge)

maxZyklusBalanced u o =
 (u,
  o,
  max 0 (zyklenStep u o)
 )

\end{code}
\begin{code}

zyklenStep :: UntereGrenze -> ObereGrenze -> MaxZykLaenge

zyklenStep u o
 | u == o    = mylen( dreiNplusEins u )
 | u >  o    = -1
 | otherwise = max (zyklenStep u mid) (zyklenStep (mid + 1) o) -- ``balancing'' happens here
               where mid = u + ( (o - u) `div` 2 )

\end{code}
\subsection*{anzNachbarn}
\begin{code}

anzNachbarn :: [[Bool]] -> (Integer,Integer) -> Integer

anzNachbarn matrix (m,n)
    | not( (m,n) `inMatrix` matrix ) = -1 
    | otherwise                      = anzahl
    where (hoehe, breite) = sizeOfMatrix matrix
          offsets         = [ (z,s) | z <- [-1,0,1], -- all offsets
                                      s <- [-1,0,1], 
                                      not( z == 0 && s == 0 ) ]
          koords          = [ (z,s) | (zoff,soff) <- offsets, -- add offsets to coords
                                      let z = m + zoff,
                                      let s = n + soff,
                                      (z,s) `inMatrix` matrix ]
          trues           = [ (z,s) | (z,s) <- koords, -- filter for ``true'' neighbours
                                      matrix!!(fromIntegral z)!!(fromIntegral s) ]
          anzahl          = fromIntegral( length( trues ) ) -- and count them

\end{code}
\begin{code}

inMatrix :: (Integer,Integer) -> [[Bool]] -> Bool

inMatrix (a,b) matrix =
    a >= 0 && a < hoehe && b >= 0 && b < breite
    where (hoehe,breite) = sizeOfMatrix matrix


\end{code}
\begin{code}

sizeOfMatrix :: [[Bool]] -> (Integer,Integer)

sizeOfMatrix matrix@(ersteZeile:_) = ( mylen(matrix), mylen(ersteZeile) )

\end{code}
\subsection*{transform}
\begin{code}

transform :: [[Bool]] -> [[Integer]]

transform matrix = 
    [ [anzNachbarn matrix (z,s) 
      | s <- [0..(breite - 1)]
      ] 
    | z <- [0..(hoehe - 1)]
    ]
	where (hoehe,breite) = sizeOfMatrix matrix

\end{code}

\newpage
\section*{CityMaut}

\begin{code}

type Bezirk = Char
type AnzBezirke = Integer
type Route = (Bezirk, Bezirk)
type Weg   = [Bezirk]
type CityMap = (AnzBezirke, [Route])

\end{code}
\begin{code}

angabeCity = (6, [
                 ('B','C'), 
                 ('A','B'), 
                 ('C','A'), 
                 ('D','C'), 
                 ('D','E'), 
                 ('E','F'), 
                 ('F','C')])

\end{code}

a function of the type ``BezirkMapping'' shall return all neighbouring bezirks to a given bezirk

\begin{code}

type BezirkMapping = (Bezirk -> [Bezirk])

\end{code}

bezirkMapping generates a ``BezirkMapping'' based on the route information of a CityMap

\begin{code}

bezirkMapping :: [Route] -> BezirkMapping

bezirkMapping []               _ = [] 
bezirkMapping ((from,to):rest) x 
    | x == from = to:recursion
    | x == to   = from:recursion
    | otherwise = recursion
    where recursion = bezirkMapping rest x

\end{code}
\begin{code}

paths1 :: BezirkMapping -> Bezirk -> Bezirk -> [Weg]
paths1 mapping v1 v2 =  paths1_ mapping v1 v2 []

\end{code}
\begin{code}

type TabuList = [Bezirk]

paths1_ :: BezirkMapping -> Bezirk -> Bezirk -> TabuList -> [Weg]

-- explicit tabulist
paths1_ mapping start dest tabulist
        | (start == dest) = [[dest]]
        | not tabu        = [(start:tailpath) | next <- neighbours, 
                                                tailpath <- (paths1_ mapping next dest (start:tabulist))]
        | otherwise       = []
                           -- ``\textbackslash\textbackslash'' means ``without'' (see Data.List for definition)
        where neighbours = (mapping start) \\ [start]
              tabu       = (elem start tabulist)

\end{code}
\begin{code}

bezirkMappingMinus :: BezirkMapping -> Bezirk -> BezirkMapping

-- hint: $f(g(x)) = (f . g)(x)$
bezirkMappingMinus mapping bezirkToDelete = (filter ( bezirkToDelete /= )) . mapping

-- this is equivalent:
bezirkMappingMinus' mapping bezirkToDelete askedBezirk = filter ( bezirkToDelete /= ) (mapping askedBezirk)

\end{code}
\begin{code}

-- define operator \textbackslash\textbackslash\textbackslash
(\\\) = bezirkMappingMinus

\end{code}
\begin{code}

paths2 :: BezirkMapping -> Bezirk -> Bezirk -> [Weg]

-- ``implicit tabulist'' via ``\textbackslash\textbackslash\textbackslash'' operator
paths2 mapping start dest
    | (start == dest)     = [[dest]]
    | next_bezirks == []  = []
    | otherwise           = [ start:tailpath | next <- next_bezirks, 
                                               tailpath <- paths2 (mapping \\\ start) next dest ]
    where next_bezirks = (mapping \\\ start) start

\end{code}
\begin{code}

paths3 :: BezirkMapping -> Bezirk -> Bezirk -> [Weg]

paths3 mapping v1 v2 =  paths3_ mapping [v1] v2 []

\end{code}
\begin{code}

paths3_ :: BezirkMapping -> [Bezirk] -> Bezirk -> [Bezirk] -> [Weg]

-- without list comprehension
paths3_ _ [] _ _ = []
paths3_ mapping (current:neighbours) dest partial
    | (dest == current)      = [partial ++ [current]] ++ recursion_excl_current
    | not tabu               = recursion_incl_current ++ recursion_excl_current
    | otherwise              = recursion_excl_current
    where recursion_excl_current = paths3_ mapping neighbours          dest partial 
          recursion_incl_current = paths3_ mapping currents_neighbours dest (partial ++ [current])
          currents_neighbours    = (mapping current) \\ [current]
          tabu                   = elem current partial

\end{code}
\begin{code}

angabeMapping = bezirkMapping $ snd angabeCity

\end{code}
\begin{code}

allRoutes :: BezirkMapping -> Bezirk -> Bezirk -> [Weg]

allRoutes = paths1
-- allRoutes = paths2

\end{code}
\begin{code}

-- simple testsuite
equivTest = [ (xy,res1 == res2,res2 == res3,res3 == res1) 
            | x <- ['A'..'F'], 
              y <- ['A'..'F'],
              let p1 = paths1 angabeMapping, 
              let p2 = paths2 angabeMapping,
              let p3 = paths3 angabeMapping,
              let res1 = p1 x y,
              let res2 = p2 x y,
              let res3 = p3 x y,
              let xy = (x,y)
            ]

equivTestSuccess = all (\(_,t1,t2,t3) -> (t1 && t2 && t3)) equivTest

\end{code}
\begin{code}

nadeloehrs :: BezirkMapping -> Bezirk -> Bezirk -> Maybe [Bezirk]

-- returns Nothing on invalid parameters
-- returns Just <list> where list is the [potentially empty] list of nadeloehrs
nadeloehrs mapping start end
    | no_route  = Nothing
    | otherwise = Just (intersectAll routes)
    where routes   = allRoutes mapping start end
          no_route = routes == [] || start == end

\end{code}
\begin{code}

intersectAll = intersectAll1

\end{code}
\begin{code}

-- using foldl' (assumption: all bezirk names are upper case letters)
-- $ (init . tail) $ strips the first and last element of a list
intersectAll1 :: [Weg] -> [Bezirk]

intersectAll1 routes = foldl' (intersect) ['A'..'Z'] (map (init . tail) routes)

\end{code}
\begin{code}

-- using list comprehension (same assumption as before)
intersectAll2 :: [Weg] -> [Bezirk]

intersectAll2 routes = [n | n <- ['A'..'Z'], and (map ((elem n) . init . tail) routes)]

\end{code}
\begin{code}

-- using simple recurions (do note: no assumptions on names of bezirks needed)
intersectAll3 :: [Weg] -> [Bezirk]

intersectAll3 [route]        = route -- try to figure out, why this case is needed
intersectAll3 []             = []    -- and this one as well
intersectAll3 (route:routes) = intersect route (intersectAll3 routes)

\end{code}
\end{document}
