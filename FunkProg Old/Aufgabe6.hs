import Data.List

type State = Integer
type StartState = State
type AcceptingStates = [State]
type Word a = [a]
type Row a = [[a]]
data AMgraph a = AMg [(Row a)] deriving (Eq,Show)
type Automaton a = AMgraph a

-- Diese Methode �berpr�ft ob das �bergebene Wort vom Zustandsautomaten akzeptiert wird.
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

-- Diese Methode ermittelt die Endzust�nde, die sich nach dem Durchwandern
-- der Zustandsmaschine aufgrund des �bergebenen Wortes ergeben. Zum Beispiel
-- ergeben sich die Endzust�nde [1,1] bei einer Zustandsmachine
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
getEndStates _ _ _ = []

-- Diese Methode stellt f�r die �bergebenen Endzust�nde fest, ob diese im Graphen
-- vorhanden sind. Die Fehlerbehandlung der Methode ist jedoch nicht richtig da
-- auf jeden Fall True zur�ckgegeben wird, sobald einer der Knoten im Graphen vorhanden
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

-- Diese Methode gibt f�r den �bergebenen Zustandsknoten (z.B. 0) die entsprechenden
-- Nachbarknoten zur�ck, die den �bergang a enthalten. Wenn es z.B. eine Kante vom
-- Knoten 0 nach Knoten 1 gibt, und f�r a der Wert 'c' �bergeben worden ist, 
-- dann w�re das Ergebnis [1] wenn der Graph wie folgt aussieht: AMg [["", "c"]]
getStateTransitionVerdictsForStateVerdict :: Eq a => (AMgraph a) -> State -> a -> [State]
getStateTransitionVerdictsForStateVerdict (AMg []) _ _ = []
getStateTransitionVerdictsForStateVerdict (AMg rows) stateVerdict edgeValue = 
    getTransiationsIn row startState edgeValue
        where row = (rows !! (fromInteger stateVerdict))
              startState = 0

getTransiationsIn :: Eq a => (Row a) -> State -> a -> [State]
getTransiationsIn [] _ _ = []
getTransiationsIn (row:remainingRows) startState edgeValue
    | edgeValue `elem` row = startState : continueWithRemainingRows
	| otherwise = continueWithRemainingRows
        where continueWithRemainingRows = 
                (getTransiationsIn remainingRows (startState + 1) edgeValue)