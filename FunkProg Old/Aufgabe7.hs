module Aufgabe7 where

import Data.List

type State = Integer
type StartState = State
type AcceptingStates = [State]
type Word a = [a]
type Row a = [[a]]
data AMgraph a = AMg[(Row a)] deriving (Eq, Show)
type Automaton a = AMgraph a
type Postfix a = Word a
type Prefix a = Word a

-- Aufgabe 7.1 ---------------------------------------------------------------------------------------------------------------------

isPostfix :: Eq a => (Automaton a) -> StartState -> AcceptingStates -> (Postfix a) -> Bool
isPostfix _ _ _ postfix
    | postfix == [] = True
isPostfix automat startState acceptingStates postfix
    | ifPostfixIsValid = True
    | otherwise = False
        where ifPostfixIsValid = True `elem` testAllStatesForAcceptanceAboutGivenPostfix states automat acceptingStates postfix
              states = getStatesWichContainFirstLetterOf postfix automat startState
              startState = 0

testAllStatesForAcceptanceAboutGivenPostfix :: Eq a => [State] -> (Automaton a) -> AcceptingStates -> (Postfix a) -> [Bool]
testAllStatesForAcceptanceAboutGivenPostfix [] _ _ _ = []
testAllStatesForAcceptanceAboutGivenPostfix (startState:remainingStates) automat acceptingStates postfix =
    (accept automat startState acceptingStates postfix) : 
        (testAllStatesForAcceptanceAboutGivenPostfix remainingStates automat acceptingStates postfix)

-- Diese Methode ermittelt für den ersten Buchstaben des Postfix alle Zustände, die diesen 
-- Wert enthalten. Z.B. Bei einem Postfix "ab" ist der erste Buchstabe ein "a" und für den
-- Automaten: (AMg [ (["de","a","c","b"]), (["de","a","c","b"]), (["de","a","c",""]), (["de","a","","b"]) ])
-- wäre das Ergebnis die folgende Zustandsliste: [0, 1, 2, 3] da alle Rows in dem Automaten
-- den Wert "a" als Zustandsübergang enthalten.
getStatesWichContainFirstLetterOf :: Eq a => (Postfix a) -> (AMgraph a) -> State -> [State]
getStatesWichContainFirstLetterOf _ (AMg []) _ = []
getStatesWichContainFirstLetterOf postfix (AMg (row:rows)) startState
    | firstLetter `elem` row = startState : continueWitRemainingRows
    | otherwise = continueWitRemainingRows
        where firstLetter = (head postfix : [])
              continueWitRemainingRows = getStatesWichContainFirstLetterOf postfix (AMg rows) (startState + 1)
              
-- Aufgabe 7.2 ---------------------------------------------------------------------------------------------------------------------

-- Nothing, falls p kein Postfix eines von A bzgl. s und E akzeptierten Wortes ist.
-- Just q, so dass die Konkatenation qp ein von A bzgl. s und E akzeptierten Wortes ist 
-- E akzeptierten Wortes ist.
givePrefix :: Eq a => (Automaton a) -> StartState -> AcceptingStates -> (Postfix a) -> (Maybe (Prefix a))
givePrefix _ _ _ postfix
    | postfix == [] = (Just postfix)
givePrefix automat startState acceptingStates postfix
    | ifPostfixIsNotValid = Nothing
    | ifStartStateContainsFirstLetterOfPostfix = (Just firstLetterOfPostfix)
    | otherwise = Nothing--(Just postfix)
        where ifPostfixIsNotValid = not (isPostfix automat startState acceptingStates postfix)
              ifStartStateContainsFirstLetterOfPostfix = startState `elem` (getStatesWichContainFirstLetterOf postfix automat 0)
              firstLetterOfPostfix = (head postfix : [])
        
-- getValueOfTransitionFromStartStateToFirstStateWhichContainFirstLetterOfPostfix :: Eq a => (Automaton a) -> State -> (Postfix a) -> [a]
getTransitionValueFromStartStateToFirstStateWhichContainFirstLetterOfPostfix automat startState postfix
    | transitionValue /= "" = transitionValue
    | otherwise = ""
        where transitionValue = allTransitionValuesFromStartState !! firstResultStateWhichContainsFirstLetterOfPostfix
              allTransitionValuesFromStartState = (startStateTransitions automat startState)
              firstResultStateWhichContainsFirstLetterOfPostfix = (fromInteger (head (getStatesWichContainFirstLetterOf postfix automat 0)))

startStateTransitions :: Eq a => (AMgraph a) -> State -> (Row a)
startStateTransitions (AMg automat) startState = automat !! (fromInteger startState)

--------------

foo postfix automat acceptingStates = exec postfix automat acceptingStates ((firstState postfix automat)-1)

exec _ _ _ pos
    | pos < 0 = ""
exec postfix automat acceptingStates pos
    | (pos `elem` acceptingStates) = transitionValue ++ postfix
    | otherwise = exec (transitionValue ++ postfix) automat acceptingStates (pos - 1)
        where transitionValue = preState !! pos
              preState = (getPreStateOf postfix automat)

getPreStateOf postfix automat = (getRowFor preState automat)
    where preState = (firstState postfix automat) - 1

firstState postfix automat = (fromInteger (head (getStatesWichContainFirstLetterOf postfix automat 0)))

getRowFor state _
    | state < 0 = []
getRowFor state (AMg automat) = automat !! state

-- Aufgabe 7.3 ---------------------------------------------------------------------------------------------------------------------

-- TODO

-- Aufgabe 7.4 ---------------------------------------------------------------------------------------------------------------------

-- TODO

-- Hilfsmethoden von Aufgabe 6 ------------------------------------------------------------------------------------------------------
              
-- Diese Methode überprüft ob das übergebene Wort vom Zustandsautomaten akzeptiert wird.
accept :: Eq a => (Automaton a) -> StartState -> AcceptingStates -> (Word a) -> Bool
accept (AMg []) _ _ _ = False
accept stateMachine@(AMg rows) startState acceptingStates word
	| ifStartStateNotValid || ifAcceptingStatesAreNotValid = False
	| (intersect acceptingStates endStates) == [] = False
	| otherwise = True
        where ifStartStateNotValid = stateNumber >= nrOfStates || stateNrIsNegative
              stateNumber = (fromInteger startState)
              nrOfStates = (length rows)
              stateNrIsNegative = startState < 0
              ifAcceptingStatesAreNotValid = not (areValid acceptingStates stateMachine)
              endStates = getEndStates stateMachine startState word

-- Diese Methode ermittelt die Endzustände, die sich nach dem Durchwandern
-- der Zustandsmaschine aufgrund des übergebenen Wortes ergeben. Zum Beispiel
-- ergeben sich die Endzustände [1,1] bei einer Zustandsmachine
-- AMg [["a", "b", "b"], ["d", "a", "f"], ["x", "a", "r"]] und dem Wort "ba".
getEndStates :: Eq a => (AMgraph a) -> State -> (Word a) -> [State]
getEndStates (AMg []) _ _ = []
getEndStates _ state [] = [state]
getEndStates stateMachine startVerdict (letter:remainingLetters) = 
    [endState | verdict <- verdictsOfStateTransitions, 
                endState <- (getEndStates stateMachine verdict remainingLetters)]
		where 
            verdictsOfStateTransitions = 
                (getStateTransitionVerdictsForStateVerdict stateMachine startVerdict letter)
getEndStates _ _ _ = [] -- Necessary?

-- Diese Methode stellt für die übergebenen Endzustände fest, ob diese im Graphen
-- vorhanden sind. Die Fehlerbehandlung der Methode ist jedoch nicht richtig da
-- auf jeden Fall True zurückgegeben wird, sobald einer der Knoten im Graphen vorhanden
-- ist.
areValid :: AcceptingStates -> (AMgraph a) -> Bool
areValid _ (AMg []) = False
areValid [] _ = False
areValid (state:remainingStates) graph@(AMg rows) 
	| stateNr >= nrOfStates || stateNrIsNegative = areValid remainingStates graph
	| otherwise = True
        where stateNr = (fromInteger state)
              nrOfStates = (length rows)
              stateNrIsNegative = state < 0

-- Diese Methode gibt für den übergebenen Zustandsknoten (z.B. 0) die entsprechenden
-- Nachbarknoten zurück, die den Übergang a enthalten. Wenn es z.B. eine Kante vom
-- Knoten 0 nach Knoten 1 gibt, und für a der Wert 'c' übergeben worden ist, 
-- dann wäre das Ergebnis [1] wenn der Graph wie folgt aussieht: AMg [["", "c"]]
getStateTransitionVerdictsForStateVerdict :: Eq a => (AMgraph a) -> State -> a -> [State]
getStateTransitionVerdictsForStateVerdict (AMg []) _ _ = []
getStateTransitionVerdictsForStateVerdict (AMg rows) stateVerdict edgeValue = 
    getTransitionsIn row startState edgeValue
        where row = (rows !! (fromInteger stateVerdict))
              startState = 0

getTransitionsIn :: Eq a => (Row a) -> State -> a -> [State]
getTransitionsIn [] _ _ = []
getTransitionsIn (row:remainingRows) startState edgeValue
    | edgeValue `elem` row = startState : continueWithRemainingRows
	| otherwise = continueWithRemainingRows
        where continueWithRemainingRows = 
                (getTransitionsIn remainingRows (startState + 1) edgeValue)
