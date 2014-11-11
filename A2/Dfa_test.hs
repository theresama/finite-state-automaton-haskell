{- Sample tests for Assignment 2 -}
import Test.HUnit
import Dfa (State, Symbol, Transition, Automaton(..),
            allStrings, tableToDelta, extend, possibleOutcomes,
            accept, language,
            removeUseless, isFiniteLanguage, language', epsilonClosure)


tableToDeltaTests = TestList [
    [2] ~=? tableToDelta [(1, 'f', 2)] 1 'f',
    -- Note: a symbol could be passed in that doesn't appear in any transition
    [] ~=? tableToDelta [(1, 'f', 2)] 1 'b',
    [] ~=? tableToDelta [(1, 'f', 2)] 1 ' '
    ]

extendTests = TestList [
    [2] ~=? extend (tableToDelta [(1, 'f', 2), (2, 'f', 2)]) 1 "ff",
    [1] ~=? extend (tableToDelta [(1, 'f', 2), (2, 'f', 2)]) 1 "",
    [2] ~=? extend (tableToDelta [(1, 'f', 2), (2, 'f', 1)]) 1 "fff",
    [2,3,4] ~=? extend (tableToDelta [(1, 'f', 2), (2, 'f', 1), (1, 'f', 3), (2, 'f', 4), (3, 'f', 3), (4, 'f', 4)]) 1 "fff"
    ]

allStringsTests = TestList [
    [""] ~=? allStrings "" !! 0,
    [""] ~=? allStrings "ahdsfljkas" !! 0,
    ["a", "b", "c"] ~=? allStrings "cba" !! 1,
    ["a", "b", "c"] ~=? allStrings "cba" !! 1,
    ["aa","ab","ac","ax","ay","az","ba",
    "bb","bc","bx","by","bz","ca","cb",
    "cc","cx","cy","cz","xa","xb","xc",
    "xx","xy","xz","ya","yb","yc","yx",
    "yy","yz","za","zb","zc","zx","zy","zz"] 
    ~=? allStrings "cbazyx" !! 2,
    ["aa", "ab", "ba", "bb"] ~=? allStrings "ab" !! 2,
    ["aaa","aab","aba","abb","baa","bab","bba","bbb"] ~=? allStrings "ab" !! 3
    ]

empty = Automaton [0] ['a'] [] 0 [0]

ex = Automaton [0,1,2]['a','b'][(0,'a',1),(1,'a',2),(0,'b',0),(1,'b',1),(2,'b',2),(1,'a',0)] 0 [2]

possibleOutcomesTests = TestList [
    [("aa",[1]), ("ab",[0,2]), ("ba",[0,2]), ("bb",[1])] ~=?
        (possibleOutcomes (Automaton [0,1,2]
                                     ['a','b']
                                     [(0,'a',1),
                                      (1,'a',2),
                                      (0,'b',0),
                                      (1,'b',1),
                                      (2,'b',2),
                                      (1,'a',0)] 0 [2]) 1) !! 2,
    [("",[])] ~=? possibleOutcomes ex 1 !! 0,
    [("",[])] ~=? possibleOutcomes ex 2 !! 0,
    [("a",[]),("b",[])] ~=? possibleOutcomes ex 5 !! 1,
    [("",[])] ~=? possibleOutcomes empty 0 !! 0,
    [("a",[])] ~=? possibleOutcomes empty 0 !! 1,
    [("a",[])] ~=? possibleOutcomes empty 1 !! 1
    ]

a1 = Automaton [0,1] ['a'] [(0,'a',1),(1,'a',0)] 0 [0]

ex2 = Automaton [0,1,2]
                ['a','b','c']
                [(0,'a',1),
                (0,'b',1),
                (0,'c',1),
                (1,'a',1),
                (1,'b',2),
                (1,'c',1),
                (2,'c',1),
                (2,'b',2),
                (1,'a',0)] 0 [2]

finite = Automaton [0,1] ['a'] [(0,'a',1)] 0 [1]

acceptTests = TestList [
    True ~=? accept a1 "aa",
    False ~=? accept a1 "a",
    False ~=? accept a1 "aaa",
    True ~=? accept a1 "",
    False ~=? accept ex "",
    True ~=? accept ex "aa",
    True ~=? accept ex "aab",
    True ~=? accept ex "aabbbbbbbbaab",
    False ~=? accept ex "babb",
    True ~=? accept ex "babbab",
    True ~=? accept ex2 "aaab",
    True ~=? accept finite "a",
    False ~=? accept finite "aa",
    False ~=? accept finite "aaa"
    ]

languageTests = TestList [
    ["","aa"] ~=? take 2 (language a1),
    ["","aa","aaaa","aaaaaa","aaaaaaaa","aaaaaaaaaa","aaaaaaaaaaaa"] ~=? take 7 (language a1),
    ["aa","aab"] ~=? take 2 (language ex),
    ["aa","aab","aba","baa","aaaa"] ~=? take 5 (language ex)
    ]

a2 = Automaton [0,1] ['a'] [(0,'a',1)] 0 [0]



eq :: Automaton -> Automaton -> Bool
eq (Automaton s1 a1 ts1 i1 f1) (Automaton s2 a2 ts2 i2 f2) =
    s1 == s2 &&
    a1 == a2 &&
    ts1 == ts2 &&
    i1 == i2 &&
    f1 == f2

removeUselessTests = let a3 = removeUseless a2
    in
    TestList [
        True ~=? eq a3 (Automaton [0] ['a'] [] 0 [0])
        ]


infinite = Automaton [0,1] ['a'] [(0,'a',0)] 0 [0]

isFiniteLanguageTests = TestList [
    True ~=? isFiniteLanguage a2,
    False ~=? isFiniteLanguage infinite
    ]


language'Tests = TestList [
    [""] ~=? language' a2
    ]

a3 = Automaton [0,1,2] ['a','b'] [(0,' ',2),(0,'a',1),(2,'b',0)] 0 [1]


epsilonClosureTests = TestList [
    [0,2] ~=? epsilonClosure a3 [0]
    ]

main :: IO ()
main = do
    -- Put each call to "runTestTT" on a separate line
    runTestTT tableToDeltaTests
    runTestTT extendTests
    runTestTT allStringsTests
    runTestTT possibleOutcomesTests
    runTestTT acceptTests
    runTestTT languageTests
    runTestTT removeUselessTests
    runTestTT isFiniteLanguageTests
    runTestTT language'Tests
    runTestTT epsilonClosureTests
    return ()
