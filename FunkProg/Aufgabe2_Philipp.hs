import Prelude
import Data.List

-- Angabe:
-- P = {1 + 4n | n <- IN} = { 1,5,9,13,17,21,...}
-- P ist bezüglich der Multiplikation abgeschlossen (a * b = c, if a,b <- P then c <- P)
-- manche Elemente aus P kann man in Faktorpaare zerlegen, z.B. 1+ 4*38 = 153 = 9*17
--              (aber nicht eindeutig, mehrere Möglichkeiten)
--   andere nicht
-- Elemente aus P, die sich NICHT in Faktorpaare zerlegen lassen, heißen Primalzahlen


       
       
-- Aufgabe 1: ist eine bestimme Zahl eine Primalzahl?
-- eine Zahl ist dann eine Primalzahl, wenn sie sich NICHT in Faktoren aufspalten lässt, die wieder Primalzahlen sind


-- Frage der Definition: ist 1 eine Primalzahl? Ich glaube, nein, so wie es auch keine Primzahl ist (?), also x>=1
-- erstellePMmenge erzeugt uns eine Liste aller Zahlen aus P, potienzielle Primalzahlen, bis inklusive "zahl"
-- (ACHTUNG: BUG: manchmal eine mehr, aber das sollte egal sein, könnte man ev noch verbessern)
--       Beispiele: erstellePMmenge 8  == [5,9]
--                  erstellePMmenge 12 == [5,9,13]
--                  erstellePMmenge 43 = [5,9,13,17,21,25,29,33,37,41]
--                  erstellePMmenge 44 = [5,9,13,17,21,25,29,33,37,41,45]
--                  erstellePMmenge 45 = [5,9,13,17,21,25,29,33,37,41,45]
--                  erstellePMmenge 46 = [5,9,13,17,21,25,29,33,37,41,45]

erstellePMmenge :: Integer -> [Integer]
erstellePMmenge zahl 
        | (zahl < 0)    = []
        | otherwise     = [1 + 4*x | x<-[1.. (div zahl 4) ] ]


-- Eine Zahl ist genau dann eine Primalzahl, wenn sie sich NÌCHT in Faktoren aufspalten lässt
-- dh, wenn die Liste ihrer Faktoren leer ist

-- anstatt die Faktoren-Liste nochmals neu zu bestimmen, nehme ich einfach die 
-- faktorisiere-Funktion aus dem zweiten Beispiel zu Hilfe

istPrimal :: Integer -> Bool
istPrimal n 
        | n <= 1                = False
        | notElem n pmenge      = False
        | otherwise             = faktorisiere n == []
        where
                pmenge = erstellePMmenge n

-- Aufgabe 2: wenn eine Zahl in P enthalten ist, aber keine Primalzahl ist, gib die Liste ihrer Faktoren aus
-- dh
-- (1) nur für Zahlen > 1 definiert (wieder: Vorsicht, was ist mit 1? keine Primalzahl?)
-- (2) schau nach ob n in P enthalten ist, wenn nein, "Unzulaessig"
-- (3) erstelle Liste aus Zahlenpaaren, wobei (3a) jede Zahl aus P stammt, und (3b) deren Produkt "n" ist

faktorisiere :: Integer -> [(Integer,Integer)]
faktorisiere n
        | (n <= 1)                         = error "Unzulaessig"
        | notElem n pmenge                 = error "Unzulaessig"
        | otherwise                        = [ (a,b) | a <- pmenge, b <- pmenge, (a * b) == n ]
        where
                -- pmenge :: [Integer]
                pmenge = erstellePMmenge n

-- Mir fällt auf, dass ich die pmenge bei _beiden_ Funktionen als Variable brauche, kann man das vereinfachen?
























type Editor = String
type Suchzeichenreihe = String
type Index = Integer
type Vorkommen = Integer
type Alt = String
type Neu = String


-- Gefällt mir überhaupt nicht, aber scheint zu funktionieren
suche :: Editor -> Suchzeichenreihe -> Index
suche _ [] = (-1)
suche [] _ = (-1)
suche e s
        | (fromIntegral gefundenerIndex) == (length e - 1) = (-1)
        | otherwise                                       = gefundenerIndex
        where
                gefundenerIndex = findePosition e s

findePosition :: Editor -> Suchzeichenreihe -> Index
findePosition [] _ = (-1)
findePosition (e:es) s 
        | isPrefixOf s (e:es)   = 0
        | otherwise             = 1 + findePosition (es) s


-- Gefällt mir sogar noch weniger, aber jetzt sitz ich schon Stunden dran, mittlerweise ist es mir schon ziemlich egal, es funktioniert:
sucheAlle :: Editor -> Suchzeichenreihe -> [Index]
sucheAlle [] _ = []
sucheAlle _ [] = []
sucheAlle e s = findePosition2 e s counter
        where counter = 0

findePosition2 :: Editor -> Suchzeichenreihe -> Int -> [Integer]
findePosition2 [] _ _ = []
findePosition2 (e:es) s i
        | take ((length s)) (e:es) == s   = [fromIntegral i] ++ findePosition2 (drop (length s) (e:es)) s (i + (length s))
        | otherwise                       = findePosition2 es s (i+1)


-- Ditto: wurscht. es geht.
ersetze :: Editor -> Vorkommen -> Alt -> Neu -> Editor
ersetze editor i alt neu
        | i < 1                                               = editor
        | (fromIntegral (length listeAllerVorkommnisse)) < i  = editor
        | otherwise                                           = alterEditorVorher ++ ersatzString ++ alterEditorNachher
        where
                ersatzString = neu
                listeAllerVorkommnisse = sucheAlle editor alt -- gibt mir [Index]
                ersetzePosition = fromIntegral ((!!) listeAllerVorkommnisse ((fromIntegral i) - 1)) -- ab welcher Stelle im String soll ersetzt werden?
                alterEditorVorher = take ersetzePosition editor
                alterEditorNachher = drop ersetzePosition (drop (length alt) editor)






