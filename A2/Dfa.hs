{- Assignment 2 - Finite Automata (due November 11, noon)

Notes:
- You may import Data.List; you may not import any other modules

***Write the names and CDF accounts for each of your group members below.***
<Name>, <CDF>
<Name>, <CDF>
-}
module Dfa (State, Symbol, Transition, Automaton(..),
            allStrings, tableToDelta, extend, possibleOutcomes,
            accept, language, 
            removeUseless, isFiniteLanguage, language', epsilonClosure, helper) where

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
extend f = (\given_state given_string -> 
		sort ( nub ( helper f (f given_state (head given_string)) (tail given_string))))

helper :: (State -> Symbol -> [State]) -> [State] -> String -> [State]
helper _ given_states "" = given_states
helper _ [] _ = []
helper f given_states given_string = helper f (concatMap (\state ->  f state (head given_string)) given_states) (tail given_string)
					

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
eachString str = [[head str]] ++ eachString (tail str)

helperMap element eachStr = map (\x -> element ++ x ) (eachString eachStr)

possibleOutcomes :: Automaton -> State -> [[(String, [State])]]
possibleOutcomes auto q = undefined


-- Questions 5-6: acceptance
accept :: Automaton -> String -> Bool
accept = undefined

language :: Automaton -> [String]
language = undefined


-- Questions 7-9: finiteness
removeUseless :: Automaton -> Automaton
removeUseless = undefined

isFiniteLanguage :: Automaton -> Bool
isFiniteLanguage = undefined

language' :: Automaton -> [String]
language' = undefined


-- Question 10: epsilon transitions
epsilonClosure :: Automaton -> [State] -> [State]
epsilonClosure = undefined
