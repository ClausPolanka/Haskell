\documentclass[a4paper,12pt]{scrartcl}
%die naechste zeile ist fuer lhs2TeX noetig
%include polycode.fmt

\usepackage{hyperref}
\usepackage{url}
\usepackage[utf8x]{inputenc}
\usepackage{marvosym}

\newcommand{\authormod}[2]{#1\\{\small#2}}

\begin{document}

\author{
	\authormod{Bong Min Kim}{e0327177\MVAt student.tuwien.ac.at} \and
	\authormod{Christoph Sp\"ork}{christoph.spoerk\MVAt inode.at} \and
	\authormod{Florian Hassanen}{florian.hassanen\MVAt googlemail.com } \and
	\authormod{Bernhard Urban}{lewurm\MVAt gmail.com}
}

\subject{Haskell Live}

\title{[04] Aufgabenblatt 1 \& 2}
\date{29. Oktober 2010}

\maketitle

\section*{Hinweise}
Diese Datei kann als sogenanntes ``Literate Haskell Skript'' von \texttt{hugs}
geladen werden, als auch per
\texttt{lhs2TeX}\footnote{\url{http://people.cs.uu.nl/andres/lhs2tex}} und
\LaTeX\ in ein Dokument umgewandelt werden.

\section*{Algebraische Datentypen}
Algebraische Datentypen Definitionen werden mit dem Schl\"usselwort \emph{data} eingeleitet.
Die simplester Form eines algebraischen Datentyps ist die Enumeration der Elemente

\begin{code}
import Data.Char

data Temp = Cold | Hot deriving Show
data Season = Spring | Summer | Autumn | Winter
\end{code}
\emph{Cold} und \emph{Hot} werden als Konstruktoren des Typs \emph{Temp} bezeichnet

\begin{code}
weather :: Season -> Temp
weather Summer = Hot
weather _ = Cold
\end{code}
Wir k\"onnen auch einen Datentyp definieren, der aus mehreren Komponenten besteht, und er wird
auch als Produkttyp bezeichnet

\begin{code}
data People = Person Name Age
type Name = String
type Age = Int

showPerson :: People -> String
showPerson (Person st n) = st ++ " -- " ++ (show n)
\end{code}
\emph{newtype} definiert auch einen neuen Datentypen, aber dabei ist nur ein Konstruktor erlaubt und dieser Konstruktor muss genau ein Argument besitzen. 
\begin{code}
newtype Student = Student Name

showStudent :: Student -> String
showStudent (Student name) = "Student " ++ name
\end{code}
Wir k\"onnen den Argumenten eines Konstruktors die Labels mitgeben
\begin{code}
data Point = Pt Float Float
pointx                  :: Point -> Float
pointx (Pt x _)         =  x
pointy                  :: Point -> Float
pointy (Pt _ y)         =  y

-- alternativ
data Point2 = Pt2 {point2x, point2y :: Float}
\end{code}
Mit \emph{rekursiven} algebraischen Datentypen sind auch unendliche Datenstrukturen m\"oglich wie Listen und Baeume etc.
\begin{code}
data MyList = Nil | 
              Elem Integer MyList

testlist :: MyList
testlist = (Elem 1 (Elem 2 (Elem 3 Nil)))


myListSize :: MyList -> Int
myListSize Nil = 0
myListSize (Elem x rest) = 1 + (myListSize rest)


myListToList :: MyList -> [Integer]
myListToList Nil = []
myListToList (Elem x rest) = (x:(myListToList rest))
\end{code}

\newpage
\section*{Aufgabenblatt 1}
Ein m\"oglicher L\"osungsweg f\"ur das 1. Aufgabenblatt:
\begin{code}
-- 1.1
convert :: Integer -> [Integer]
convert x = reverse (convert' x)
	where
	convert' :: Integer -> [Integer]
	convert' x
		| x < 10 = [x]
		| otherwise = (x `mod` 10):(convert' (x `div` 10))

-- 1.2
quersumme :: Integer -> Integer
quersumme x = mysum (convert x)

mysum :: [Integer] -> Integer
mysum [] = 0
mysum (x:xs) = x + (mysum xs)


-- using foldl power
quersumme2 :: Integer -> Integer
quersumme2 x = foldl (+) 0 (convert x)


-- 1.3
dreiTeilbar :: Integer -> Bool
dreiTeilbar x = (quersumme x `mod` 3) == 0

-- 1.4
sechsTeilbar :: Integer -> Bool
sechsTeilbar x = dreiTeilbar x && ((x `mod` 2) == 0)

-- 1.5
elfTeilbar :: Integer -> Bool
elfTeilbar x = ((a (convert x)) `mod` 11) == 0
	where
	a :: [Integer] -> Integer
	a [] = 0
	a (x:[]) = x
	a (x:y:xs) = x - y + a xs
\end{code}

\section*{Aufgabenblatt 2}
Ein m\"oglicher L\"osungsweg f\"ur das 2. Aufgabenblatt:
\begin{code}
-- 2.1
diffFolge :: (Integer,Integer) -> [Integer]
diffFolge (m,n)
	| m <= 0 = [m]
	| otherwise = m:(diffFolge (m-n,n))

-- 2.2
teilt :: (Integer,Integer) -> Bool
teilt x = head (reverse (diffFolge x)) == 0

-- 2.3
zahlenBlock :: Integer -> [Integer]
zahlenBlock x
	| x < 1000 = [x]
	| otherwise = block:(zahlenBlock next)
		where
		block = x `mod` 1000
		next = x `div` 1000

-- 2.4
zeichenreihenBlock :: Integer -> [String]
zeichenreihenBlock zahl = [myshow y | y <- (zahlenBlock zahl)]

myshow :: Integer -> String
myshow x = reverse (take 3 fillWithNull)
	where
	str :: String
	str = myshow' (fromInteger x)
	fillWithNull :: String
	fillWithNull = str ++ "000"

	myshow' :: Int -> String
	myshow' x
		| x > 10 = (chr akt):(myshow' next)
		| otherwise = [chr akt]
		where
		akt :: Int
		akt = 48 + (x `mod` 10)
		next :: Int
		next = x `div` 10

-- 2.5
siebenTeilbar :: Integer -> Bool
siebenTeilbar x = teilt (abs (as (zahlenBlock x)),7)

as :: [Integer] -> Integer
as [] = 0
as (x:[]) = x
as (x:y:xs) = x - y + (as xs)
\end{code}
\end{document}
