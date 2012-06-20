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

\title{[05] Aufgabenblatt 3 [doch nicht :-]}
\date{5. November 2010}

\maketitle

\begin{comment}

\section*{Hinweise}
Diese Datei kann als sogenanntes ``Literate Haskell Skript'' von \texttt{hugs}
geladen werden, als auch per
\texttt{lhs2TeX}\footnote{\url{http://people.cs.uu.nl/andres/lhs2tex}} und
\LaTeX\ in ein Dokument umgewandelt werden.

\end{comment}

\section*{Algebraische Datentypen}
Algebraische Datentypen Definitionen werden mit dem Schl\"usselwort \emph{data} eingeleitet.
Die simplester Form eines algebraischen Datentyps ist die Enumeration der Elemente

\begin{code}
import Data.Char

data Baum = Knoten2 Baum Baum |
            Knoten1 Baum |
            Blatt
            deriving (Eq, Show)

data Groesse = Gross | Klein | Winzig 
               deriving Show

type BaumKlassifizierer = Baum -> String

\end{code}
\begin{code}

winzigBaum = Blatt
kleinBaum  = Knoten1 
                Blatt
kleinBaum2 = Knoten2 
                Blatt
                Blatt
grossBaum  = Knoten2
                (Knoten1 
                    Blatt)
                Blatt
grossBaum2 = Knoten1
                (Knoten2 Blatt 
                    (Knoten1 Blatt))
grossBaum3 = Knoten2 
                (Knoten2 
                    Blatt 
                    Blatt) 
                (Knoten2 Blatt Blatt)
\end{code}
\begin{code}

showP :: Baum -> String
showP baum = "(" ++ show baum ++ ")"
\end{code}
\begin{code}

var1 :: BaumKlassifizierer

var1 Blatt         = showP Blatt ++ " ist " ++ show Winzig

var1 (Knoten1 x)   = showP (Knoten1 x) ++ " ist " ++ if (x == Blatt) 
                                                     then show Klein 
                                                     else show Gross

var1 (Knoten2 x y) = showP (Knoten2 x y) ++ " ist " ++ if(x == Blatt && y == Blatt)
                                                       then show Klein
                                                       else show Gross
\end{code}
\begin{code}

var2 :: BaumKlassifizierer

var2 Blatt                 = showP Blatt ++ " ist " ++ show Winzig

var2 (Knoten1 Blatt)       = showP (Knoten1 Blatt) ++ " ist " ++ show Klein

var2 (Knoten2 Blatt Blatt) = showP (Knoten2 Blatt Blatt) ++ " ist " ++ show Klein

-- var2 (Knoten2 (Knoten2 a b) (Knoten2 c d)) = showP (Knoten2 (Knoten2 a b) (Knoten2 c d)) ++ " ist exorbitant " ++ show Gross

var2 baum                  = showP baum ++ " ist " ++ show Gross
\end{code}
\begin{code}


reminder ganzes@(erstes:rest) = "ganzes = " ++ show ganzes ++ " erstes = " ++ show erstes ++ " rest = " ++ show rest

\end{code}
\begin{code}

var3 :: BaumKlassifizierer

var3 baum@Blatt                 = showP baum ++ " ist " ++ show Winzig

var3 baum@(Knoten1 Blatt)       = showP baum ++ " ist " ++ show Klein

var3 baum@(Knoten2 Blatt Blatt) = showP baum ++ " ist " ++ show Klein

-- var3 baum\MVAt(Knoten2 (Knoten2 \_ \_) (Knoten2 \_ \_)) = showP baum ++ " ist exorbitant " ++ show Gross

var3 baum                       = showP baum ++ " ist " ++ show Gross
\end{code}
\begin{code}

var4 :: BaumKlassifizierer

var4 baum =
    case baum of
        Blatt                 -> showP baum ++ " ist " ++ show Winzig
        (Knoten1 Blatt)       -> showP baum ++ " ist " ++ show Klein
        (Knoten2 Blatt Blatt) -> showP baum ++ " ist " ++ show Klein
--        (Knoten2 (Knoten2 \_ \_) (Knoten2 \_ \_) -> showP baum ++ " ist exorbitant " ++ show Gross
        _                     -> showP baum ++ " ist " ++ show Gross
\end{code}
\begin{code}

mix :: String -> String -> String

mix links rechts =
    case (links,rechts) of
        ( (l:ls), (r:rs) ) -> l:r:(mix ls rs)
        ("","")            -> "" -- not needed actually
        ("",_)             -> rechts
        (_,"")             -> links
        
test_mix = mix "aaaaaaaaaaaaa" "bbbbbbb"
\end{code}
\begin{code}

-- use responsiibly

wasZum__O_o x y = 
    case x of
        (x:y:xs)   -> y
        [y]        -> y
        []         -> y

\end{code}
\begin{code}

baumCreator :: String -> Baum

baumCreator string = fst( baumCreatorHelper string )
\end{code}
\begin{code}

baumCreatorHelper :: String -> (Baum,String)

baumCreatorHelper ('b':rest) = (Blatt,rest)
baumCreatorHelper ('1':rest) = (Knoten1 baum, restrest)
                               where (baum, restrest) = baumCreatorHelper rest
baumCreatorHelper ('2':rest) = (Knoten2 links rechts, restrestrest)
                               where (links, restrest)      = baumCreatorHelper rest
                                     (rechts, restrestrest) = baumCreatorHelper restrest
baumCreatorHelper (_:rest)   = (baum, restrest)
                               where (baum,restrest) = baumCreatorHelper rest
\end{code}
\begin{code}

grossBaum3'     = baumCreator "22bb2bb"
grossBaum3''    = baumCreator "2 2bb 2bb"
nichtGrossBaum3 = baumCreator "2 2bb 2b111b"

\end{code}
\begin{code}


testsuite :: BaumKlassifizierer -> [String]

testsuite baumKlassifizierer = [ baumKlassifizierer testBaum | testBaum <- [winzigBaum, kleinBaum, kleinBaum2, grossBaum, grossBaum2, grossBaum3] ]


testVar1 = testsuite var1
testVar2 = testsuite var2
testVar3 = testsuite var3
testVar4 = testsuite var4

\end{code}

\subsection*{Beispiel}
\begin{enumerate}
\item $result \leftarrow baumCreator("21bb")$
\item $result \leftarrow fst(helperResult)$ \\
      $helperResult \leftarrow baumCreatorHelper("21bb")$ \\
      $helperResult \leftarrow baumCreatorHelper('2':"1bb")$
\item $helperResult \leftarrow ((Knoten2\quad links_1\quad rechts_1),restrestrest_1)$ \\
      $(links_1,restrest_1) \leftarrow baumCreatorHelper("1bb")$ \\
      $(links_1,restrest_1) \leftarrow baumCreatorHelper('1':"bb")$ \\
      $(rechts_1,restrestrest_1) \leftarrow baumCreatorHelper(restrest_1)$ \\

\item $(links_1,restrest_1) \leftarrow ((Knoten1\quad baum_2),restrest_2)$ \\
      $(baum_2,restrest_2) \leftarrow baumCreatorHelper("bb")$ \\
      $(baum_2,restrest_2) \leftarrow baumCreatorHelper('b':"b")$
\item $(baum_2,restrest_2) \leftarrow (Blatt,"b")$
\item $\Rightarrow$ \\
      $(links_1,restrest_1) \leftarrow ((Knoten1\quad Blatt),"b")$
\item $\Rightarrow$ \\
      $(rechts_1,restrestrest_1) \leftarrow baumCreatorHelper("b")$ \\
      $(rechts_1,restrestrest_1) \leftarrow baumCreatorHelper('b':"\,\!")$ \\
\item $(rechts_1,restrestrest_1) \leftarrow (Blatt,"\,\!")$
\item $\Rightarrow$ \\
      $helperResult \leftarrow ((Knoten2\quad (Knoten1\quad Blatt)\quad Blatt),"\,\!")$ \\
\item $\Rightarrow$ \\
      $result \leftarrow fst(((Knoten2\quad (Knoten1\quad Blatt)\quad Blatt),"\,\!"))$ \\
      $result \leftarrow (Knoten2\quad (Knoten1\quad Blatt)\quad Blatt)$
\end{enumerate}
\end{document}
