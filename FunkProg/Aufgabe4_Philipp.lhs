> module Aufgabe4 ( msk, mm, ms, mp ) where

> import Prelude


> type Matrix = [[Integer]]

> type Skalar = Integer
> type Zeilen = Integer
> type Spalten = Integer
> type SpaZei = Integer
> type Fuellwert = Integer
> type ProtoMatrix = ([[Integer]],Zeilen,Spalten,Fuellwert)

> type Typung_mnpw = (Zeilen,SpaZei,Spalten,Fuellwert)
> type Typung_mnw = (Zeilen,Spalten,Fuellwert)
> type Typung_mw = (SpaZei,Fuellwert)
> type Potenz = Integer
> type ProtoprotoMatrix = [[Integer]]


-- und zusätzlich verwende ich noch

> type Counter = Integer


BEISPIEL 1

> msk :: ProtoMatrix -> Skalar -> Matrix
> msk (l,z,s,w) sk = [ map (* sk) x | x <- matrix ]
>         where 
>                matrix = anp2 l z s w


BEISPIEL 2


Das Skalarprodukt zweier Zeilen- oder Spaltenvektoren der Länge n ist die Summe der Produkte
ihrer sich entsprechenden Komponenten.
α(N) β (N) = α(n) β(N) = α(N) β (n) = α(n) β(n) = Σj=1..n aj bj

spaltenvektor [[1],[2],[3]]
head spaltenvektor = [1]
head head spaltenvektor = 1
tail spaltenvektor = [ [2] , [3] ]

dh erstes Element ist head head spaltenvektor, rest ist tail spaltenvektor

zeilenvektor [[1,2,3]]
head zeilenvektor = [1,2,3]
head head zeilenvektor = 1
head zeilenvektor = [1,2,3]
tail head zeilenvektor = [2,3]
dh erstes Element ist head head zeilenvektor, rest ist [tail head zeilenvektor]


Es gibt schon eine ähnliche Funktion sp, die auch das Skalarprodukt berechnet, aber die nimmt jeweils [[Integer]], Laenge und Fuellwert als Argumente
Die hab ich so hier nicht wirklich zur Verfügung, aber dafür kann ich mich durch die Typung_mnpw kombiniert mit anp2 darauf verlassen, dass die
Matrizen bereits in einer gültigen, passenden und richtigen Form vorliegen.
Es werden nur Matrizen übergeben, bei denen bereits durch Anwendung von anp2 feststeht, dass es passende Matrizen sind, daher verzichte ich auf Tests undoder Fehlerkontrolle

> skalarprodukt :: Matrix -> Matrix -> Skalar
> skalarprodukt [] _ = 0
> skalarprodukt _ [] = 0
> skalarprodukt a b
>       | ((length (head a)) == 1) && ((length (head b)) == 1)        = ((head (head a)) * (head (head b))) + (skalarprodukt (tail a) (tail b) )                -- spaltenvektor * spaltenvektor
>       | ((length (head a)) == 1) && (not ((length (head b)) == 1))  = ((head (head a)) * (head (head b))) + (skalarprodukt (tail a) ([tail (head b)]) )       -- spaltenvektor * zeilenvektor
>       | (not ((length (head a)) == 1)) && ((length (head b)) == 1)  = ((head (head a)) * (head (head b))) + (skalarprodukt ([tail (head a)]) (tail b) )       -- zeilenvektor * spaltenvektor
>       | otherwise                                                   = ((head (head a)) * (head (head b))) + (skalarprodukt [tail (head a)] [tail (head b)] )  -- zeilenvektor * zeilenvektor

> zeilenvektor :: Zeilen -> Matrix -> Matrix
> zeilenvektor _ [] = []
> zeilenvektor 0  _ = []
> zeilenvektor zeile matrix = [(!!) matrix ((fromIntegral zeile)-1)]

> spaltenvektor :: Spalten  -> Matrix -> Matrix
> spaltenvektor _ [] = []
> spaltenvektor 0 _  = []
> spaltenvektor spalte matrix = [ [(!!) x ((fromIntegral spalte)-1)] | x<-matrix]



Vom Zettel:
Das Produkt einer (m,n)-Matrix A und einer (n,p)-Matrix B ist die (m,p)-Matrix P,
in der das Element p_ik sich als das
skalare Produkt des i-ten Zeilenvektors αi von A
mit dem k-ten Spaltenvektor βk von B ergibt

das kann ich mir so überhaupt nicht vorstellen, muss ich mir bildlich anschaun

(anzahl d reihen , anzahl d spalten)

(2,7)    *     (7,3)   = (2,3)


(a1 a2 a3 a4 a5 a6 a7)    (t1 t2 t3)    ( p11=αr1*βs1 p12=αr1*βs2 p13=αr1*βs3 )
(b1 b2 b3 b4 b5 b6 b7) *  (u1 u2 u3)    ( p21=αr2*βs1 p22=αr2*βs2 p23=αr2*βs3 )
                          (v1 v2 v3) =  
                          (w1 w2 w3)
                          (x1 x2 x3)
                          (y1 y2 y3)
                          (z1 z2 z3)





> mm :: ProtoprotoMatrix -> ProtoprotoMatrix -> Typung_mnpw -> Matrix
> mm p1 p2 (m,n,p,w) = mm_helper matrix_1 matrix_2 m p 1
>       where
>                matrix_1 = anp2 p1 m n w
>                matrix_2 = anp2 p2 n p w

> mm_helper :: Matrix -> Matrix -> Zeilen -> Spalten -> Counter -> Matrix
> mm_helper [] _ _ _ _ = []
> mm_helper _ [] _ _ _ = []
> mm_helper _ _ 0 _ _  = []
> mm_helper _ _ _ 0 _  = []
> mm_helper _ _ _ _ 0  = []
> mm_helper m1 m2 anzahl_zeilen anzahl_spalten counter_zeilen
>        | counter_zeilen <= anzahl_zeilen = [ this_row ] ++ next_row 
>        | otherwise                       = []
>        where
>               this_row = [skalarprodukt (zeilenvektor (fromIntegral counter_zeilen) m1) (spaltenvektor x m2) | x<-[1..(fromIntegral anzahl_spalten)] ]
>               next_row = mm_helper m1 m2 anzahl_zeilen anzahl_spalten (counter_zeilen + 1)

BEISPIEL 3

ich hab meine Version schon mal reinkopiert damit sie schon mal da ist falls morgen irgendwas schiefgeht
nachdem BSP2 auf diese Art funktioniert, hab ich dritte genauso gemacht und nur noch ein bisschen angepasst

> ms :: ProtoprotoMatrix -> ProtoprotoMatrix -> Typung_mnw -> Matrix
> ms p1 p2 (m,n,w) = ms_helper matrix_1 matrix_2 m n 1
>       where
>               matrix_1 = anp2 p1 m n w
>               matrix_2 = anp2 p2 m n w

> ms_helper :: Matrix -> Matrix -> Zeilen -> Spalten -> Counter -> Matrix
> ms_helper [] _ _ _ _ = []
> ms_helper _ [] _ _ _ = []
> ms_helper _ _ 0 _ _  = []
> ms_helper _ _ _ 0 _  = []
> ms_helper _ _ _ _ 0  = []
> ms_helper m1 m2 anzahl_zeilen anzahl_spalten counter_zeilen
>        | counter_zeilen <= anzahl_zeilen = [ this_row ] ++ next_row 
>        | otherwise                       = []
>        where
>               this_row = [ (!!) (head m1) x + (!!) (head m2) x | x<-[0..((fromIntegral anzahl_spalten) - 1)] ]
>               next_row = ms_helper (tail m1) (tail m2) anzahl_zeilen anzahl_spalten (counter_zeilen + 1)
 
Beispiel 4

> mp :: ProtoprotoMatrix -> Typung_mw -> Potenz -> Matrix
> mp p (sz, w) n
>    | n == 0 = einheitsMatrix (fromIntegral sz)
>    | otherwise = powerOf m m (sz, w) (n)
>        where 
>            m = anp2 p sz sz w

> einheitsMatrix :: Int -> Matrix
> einheitsMatrix sz = [insertOneInto (zeroMatrix !! i) (fromIntegral i) | i <- [0..(sz - 1)]]
>    where 
>        zeroMatrix = replicate sz (replicate sz 0)
>        insertOneInto (x:xs) index
>            | index == 0 = 1:xs
>            | otherwise = x:insertOneInto xs (index-1)

> powerOf m m_orig (sz, w) n
>    | n == 1 = m
>    | n > 0 = powerOf (mm m m_orig (sz, sz, sz, w)) m_orig (sz, w) (n-1)
>    | otherwise = m


DIE VON LETZTER WOCHE KOPIERTEN BEISPIELE

> type Laenge = Integer

Beispiel 1

> anp1 :: [[Integer]] -> Matrix
> anp1 [] = [[1]]
> anp1 compLists = [x ++ replicate (maxLength - length x) 0 | x <- compLists]
>     where maxLength = maximum (map length compLists)

Beispiel 2

> anp2 :: [[Integer]] -> Zeilen -> Spalten -> Fuellwert -> Matrix
> anp2 _ 0 _ _ = []
> anp2 _ _ 0 _ = []
> anp2 l z s w 
>     | z < 0 || s < 0 = error "unzulaessig"
>     | l == [] = replicate (fromIntegral z) componentListTemplate
>     | length l < (fromInteger z) = extended_L
>     | otherwise = cropped_L
>         where
>             componentListTemplate = (replicate (fromIntegral s) w)
>             extended_L = customizedComponentLists ++ requiredListsFilledWithValue
>             cropped_L = take (fromInteger z) customizedComponentLists
>             customizedComponentLists = [cropOrExtendCols x | x <- l]
>                 where 
>                     cropOrExtendCols x
>                         | length x < (fromIntegral s) = x ++ replicate ((fromIntegral s) - length x) w
>                         | otherwise = take (fromIntegral s) x
>             requiredListsFilledWithValue = (replicate ((fromIntegral z) - (length l)) componentListTemplate)

Beispiel 3

> transp :: [[Integer]] -> Zeilen -> Spalten -> Fuellwert -> Matrix
> transp l z s w
>     | z < 0 || s < 0 = error "unzulaessig"
>     | l == [] = anp2 l s z w
>     | otherwise = anp2 (buildTransponierteMatrixWithIndex 0) s z w
>         where buildTransponierteMatrixWithIndex i
>                 | i < (fromIntegral z) = [x !! i | x <- (anp2 l z s w)] : buildTransponierteMatrixWithIndex (i + 1)
>                 | otherwise = []

Beispiel 4

> sp :: [[Integer]] -> [[Integer]] -> Laenge -> Fuellwert -> Integer
> sp l1 l2 vl w
>     | vl < 0 = error "unzulaessig"
>     | otherwise = sum [(rowVector !! element) * (columnVector !! element) | element <- [0..(fromIntegral vl)-1]]
>         where 
>             rowVector = [x | x <- (anp2 l1 1 vl w) !! 0]
>             columnVector = [ y !! 0 | y <- (anp2 l2 vl 1 w)]
 
