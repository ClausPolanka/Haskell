-- Vorlesung 1

-- Vorlesung 2

-- type Editor = [Char]

-- ersetze :: Editor -> Int -> String -> String -> Editor

f :: Int -> (Int -> Int -> Int)
f 1 = (+)
f 2 = (-)
f 3 = (*)
f _ = div

g :: (Int -> Int -> Int) -> Int
g (+) = 1
-- g (-) = 2
-- g (*) = 3
-- g div = 42
-- g _   = 99

add :: Int -> Int -> Int
add x y = x + y

k :: (Int -> Int -> Int) -> Int -> Int -> Int
k x y z = x y z

-- Weniger Erfahrung
doubleInc :: Int -> Int
doubleInc n = 2 * n

-- Mehr Erfahrung
doubleInc' :: Int -> Int
doubleInc' = (+) 2

-- Operator Section
doubleInc'' :: Int -> Int
doubleInc'' = (+2)

-- Vorlesung 3
-- Repetitive Rekursion (am günstigsten)
ggt :: Integer -> Integer -> Integer
ggt m n 
    | n == 0 = m
    | m >= n = ggt (m - n) n
    | m < n = ggt (n - m) m
    
-- Lineare Rekursion
powerThree :: Integer -> Integer
powerThree n
    | n == 0 = 1
    | n > 0 = 3 * powerThree (n - 1) -- * ist die äusserste Operation
    
-- Geschachtelte Rekursionen
fun91 :: Integer -> Integer
fun91 n
    -- | n > 100 = n - 10
    -- | otherwise = fun91 . fun91 $ (n + 11)
    | n > 100 = n - 10
    | n <= 100 = fun91(fun91(n+11))
    
-- Baumartige (kaskadenartige) Rekursion
binom :: (Integer, Integer) -> Integer
binom (n, k)
    | k == 0 || n == k = 1
    | otherwise = binom (n - 1, k - 1) + binom (n - 1, k)
    
-- Direkte Rekursion (entspricht Rekursion)
-- Indirekte (verschränkte, wechselweise) Rekursion
isEven :: Integer -> Bool
isEven n
    | n == 0 = True
    | n > 0 = isOdd (n - 1)
    
isOdd :: Integer -> Bool
isOdd n
    | n == 0 = False
    | n > 0 = isEven (n - 1)

-- Fakultätsfunktion lineare Rekursion
fac :: Integer -> Integer
fac n = if n == 0 then 1 else (n * fac (n - 1))

-- Effiziente Formulierung mittels Repetitivem Rekursionsmuster
fac' :: Integer -> Integer
fac' n = facRep (n, 1)

facRep :: (Integer, Integer) -> Integer
facRep (p, r) = if p == 0 then r else facRep (p - 1, p * r)

-- Fibonacci Zahlen mittels baumartiger Rekursion
-- Sehr langsam!
fib :: Integer -> Integer
fib n
    | n == 0 = 0
    | n == 1 = 1
    | otherwise = fib (n - 1) + fib (n - 2)
    
-- Aufzählungstypen
data Jahreszeiten = Fruehling | Sommer | Herst | Winter
data Werktage = Montag | Dienstag | Mittwoch | Donnerstag | Freitag
-- data Bool = True | False

-- Produkttyp
data Person = Pers Name Geschlecht Alter
type Name = String
data Geschlecht = Maennlich | Weiblich
type Alter = Int
    
-- Summentyp
data Verkehrsmittel = Fahrrad Bool |
                      Auto String Float |
                      Schiff String Float String |
                      Flugzeug Float Int

-- Typsynonyme                      
type Tandem = Bool
type Hersteller = String
type Hubraum = Float
type Tiefgang = Float
type Heimathafen = String
type Reichweite = Float
type Sitzplaetze = Int

type Euro = Float
type Yen = Float

myPi :: Float
myPi = 3.14

daumen :: Float
daumen = 5.55

currencyConverter :: Euro -> Yen
currencyConverter x = x + myPi * daumen

type Geschwindigkeit = Float
type Meilen = Float
type Km = Float
type Zeit = Float
type Wegstrecke = Meilen
type Distanz = Km
    
geschwindigkeit :: Wegstrecke -> Zeit -> Geschwindigkeit
geschwindigkeit w z = (/) w z

verbleibendeFlugzeit :: Distanz -> Geschwindigkeit -> Zeit
verbleibendeFlugzeit d g = (/) d g

-- Produkttyp
-- data Person = Pers Name Geschlecht Alter

-- Tupeltyp
type Person' = (Name, Geschlecht, Alter)

-- Vorteil von Produkttyp
data Euro' = EUR Float
data Yen' = YEN Float
data Temperatur = Temp Float

maxTemp = Temp 43.2

-- Nicht mehr möglich
-- currencyConverter' :: Euro' -> Yen'
-- currencyConverter' x = x + myPi * daumen

-- Insbesondere:
-- currencyConverter maxTemp
    
-- Typklasse Size stellt Typspez. für Funktion size zur Verfügung
class Size a where
    size :: a -> Int

instance Size [a] where
    size = length

-- instance Size [(a, b)] where
    -- size = (*2) . length
    
-- Instanzbildung der Typklasse Eq
data Point = Point (Int, Int)

instance Eq Point where
    -- (==) (Point x) (Point y) = (fst (x) == fst (y)) && (snd (x) == snd (y))
    (==) (Point (x, y)) (Point (u, v)) = (x == u) && (y == v)
    
-- Listen und Listenkomprehension
takeFirst :: Integer -> [a] -> [a]
takeFirst m ys = case (m, ys) of
                    (0, _) -> []
                    (_, []) -> []
                    (n, x:xs) -> x : takeFirst (n - 1) xs
                    
ifThenElse :: Bool -> a -> a -> a
ifThenElse c t e = case c of True -> t
                             False -> e

listTransform :: [a] -> [a]
listTransform l@(x:xs) = (x : l) ++ xs

-- Funktionen als Resultate
giveFourthElement ::  [a] -> a
giveFourthElement = head . tripleTail

tripleTail :: [a] -> [a]
tripleTail = tail . tail . tail
    
iterate' :: Int -> (a -> a) -> (a -> a)
iterate' n f
    | n > 0 = f . iterate' (n - 1) f
    | otherwise = id'
    
id' :: a -> a
id' a = a

constFun :: a -> (b -> a)
constFun c = \x -> c

flip' :: (a -> b -> c) -> (b -> a -> c)
flip' f x y = f y x

fancySelect :: [Int] -> [Int]
fancySelect xs = filter (42<) . map (2*) $ xs
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    