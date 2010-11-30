{-######################################################
FirstScript.hs ...‘‘ordinary scripts’’ erhalten
die Dateiendung .hs
######################################################-}

-- Die konstante Funktion sum17and4
sum17and4 :: Int
sum17and4 = 17+4

-- Die Funktion square zur Quadrierung einer ganzen Zahl
square :: Int -> Int
square n = n*n

-- Die Funktion double zur Verdopplung einer ganzen Zahl
double :: Int -> Int
double n = 2*n

-- Die Funktion doubleSquare, eine Anwendung der vorherigen
doubleSquare :: Int
doubleSquare = double (square (4711 - sum17and4))