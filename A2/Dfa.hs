{- Assignment 2 - Finite Automata (due November 11, noon)
***Write the names and CDF accounts for each of your group members below.***
Theresa Ma, g2potato
Shervin Khazraie Shaneivar, <CDF>
-}
module Dfa (State, Symbol, Transition, Automaton(..),
            allStrings, tableToDelta, extend, possibleOutcomes,
            accept, language, 
            removeUseless, isFiniteLanguage, language', epsilonClosure) where

import Data.List

-- Basic data types
type State = Integer
type Symbol = Char
type Transition = (State, Symbol, State)

-- Automaton Data Type
-- Automaton states alphabet transitions initial final
data Automaton = Automaton [State] [Symbol] [Transition] State [State]
-- Some helper functions for you to access the different automaton components
states :: Automaton -> [State]
states (Automaton s _ _ _ _) = s
alphabet :: Automaton -> [Symbol]
alphabet (Automaton _ a _ _ _) = a
transitions :: Automaton -> [Transition]
transitions (Automaton _ _ ts _ _) = ts
initial :: Automaton -> State
initial (Automaton _ _ _ i _) = i
final :: Automaton -> [State]
final (Automaton _ _ _ _ f) = f

-- Questions 1-4: transitions
tableToDelta :: [Transition] -> State -> Symbol -> [State]
tableToDelta trans = (\given_state given_symbol -> sort(nub(concatMap (\(s1, symbol, s2) -> 
			if s1 == given_state && symbol == given_symbol 
				then [s2]
				else []) trans)))  

--ACENDING ORDER AND NO DUPLICATES*****

extend :: (State -> Symbol -> [State]) -> (State -> String -> [State])
extend f = (\givenState givenString -> 
        if (givenString == "") then [givenState] else
		(sort ( nub ( helper f (f givenState (head givenString)) (tail givenString)))))

helper :: (State -> Symbol -> [State]) -> [State] -> String -> [State]
helper _ givenStates "" = givenStates
helper _ [] _ = []
helper f givenStates givenString = helper f (concatMap (\state ->  f state (head givenString)) givenStates) (tail givenString)
					

--Take as input a set of symbols of size k ≥ 1 (no duplicates)
--Outputs an infinite list, where the n-th item in the list 
--is a list of all kn strings of length n that can be made 
--from the symbols in the input set, in alphabetical order
allStrings :: [Symbol] -> [[String]]
allStrings str = [""] : eachString str : concatMap (\x -> helperMap x str) (eachString str) 
    : allStringsHelper (concatMap (\x -> helperMap x str) (eachString str)) str

allStringsHelper prev str = (concatMap (\x -> helperMap x str) prev)
    : allStringsHelper (concatMap (\x -> helperMap x str) prev) str

eachString "" = []
eachString str = sort ([[head str]] ++ eachString (tail str))

helperMap element eachStr = map (\x -> element ++ x ) (eachString eachStr)

possibleOutcomes :: Automaton -> State -> [[(String, [State])]]
possibleOutcomes auto q = map (\lst -> possibleHelper auto lst q) (allStrings (alphabet auto))
possibleHelper auto [""] q = [((""), [])]
possibleHelper auto lst q = map (\str -> (str, (extend (tableToDelta (transitions auto)) q str))) lst

listToString lst = concatMap (\x -> x) lst

-- Questions 5-6: acceptance
accept :: Automaton -> String -> Bool
accept auto "" = if ((initial auto) `elem` (final auto)) then True else False
accept auto str = and (map (\state -> state `elem` (extend (tableToDelta (transitions auto)) (initial auto) str)) (final auto))

language :: Automaton -> [String]
language auto = 
    if (((length (states auto)) == 1) && accept auto "")
        then [""]
    else
        filter (\str -> accept auto str) (concat (allStrings (alphabet auto)))

-- Questions 7-9: finiteness
--useful if there exists a string of symbols of length at most n 
--that can be read to transition from q to a final state
removeUseless :: Automaton -> Automaton
removeUseless auto = Automaton (newStates auto) (alphabet auto) (newTransitions auto) (initial auto) (final auto)

newStates :: Automaton -> [State]
newStates auto = (filter (\x -> removeHelper auto x) (states auto)) 

newTransitions :: Automaton -> [Transition]
newTransitions auto = filter (\(s1, _, s2) -> not (s1 `elem` newStates auto || s2 `elem` newStates auto)) (transitions auto)

removeHelper :: Automaton -> State -> Bool
removeHelper auto state = (length (filter (\x -> isFinal auto x || state == (initial auto))(take (length (states auto)) (possibleOutcomes auto state)))) > 0

isFiniteLanguage :: Automaton -> Bool
isFiniteLanguage auto = 
    let notUseless = removeUseless auto
    in not (or (map (\str -> accept notUseless str) (allStrings (alphabet notUseless) !! ((length (states notUseless)) + 1))))


language' :: Automaton -> [String]
language' auto = 
    let isFinite = isFiniteLanguage auto
    in if (isFinite) then
        takeWhile (\s -> (length s) < ((length (states auto)) + 1)) (language auto)
        else
            language auto

-- Question 10: epsilon transitions
epsilonClosure :: Automaton -> [State] -> [State]
epsilonClosure = undefined











