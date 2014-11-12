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
    [] ~=? tableToDelta [(1, 'f', 2)] 1 ' ',
    [2] ~=?  tableToDelta [(0,'a',1),(1,'a',2),(0,'b',0),(1,'b',1),(2,'b',2),(1,'a',0)] 2 'b',
    [0, 2] ~=?  tableToDelta [(0,'a',1),(1,'a',2),(0,'b',0),(1,'b',1),(2,'b',2),(1,'a',0)] 1 'a',
    [] ~=?  tableToDelta [(0,'a',1),(1,'a',2),(0,'b',0),(1,'b',1),(2,'b',2),(1,'a',0)] 2 'a',
    [] ~=? tableToDelta [(0,'a',1), (1,'b',2), (0,'b',0)] 3 'a',
    [] ~=? tableToDelta [(0,'a',1), (1,'b',2), (0,'b',0)] 3 'c',
    [2, 4, 5] ~=? tableToDelta [(3,'c',5), (3,'c',4), (3,'c',2)] 3 'c',
    [3] ~=? tableToDelta [(3,'c',3), (3,'c',3), (3,'c',3)] 3 'c'
    ]


extendTests = TestList [
    [0] ~=? extend (tableToDelta [(0,'a',1),(1,'a',0)]) 1 "a",
    [1] ~=? extend (tableToDelta [(0,'a',1),(1,'a',0)]) 1 "aa",
    [0] ~=? extend (tableToDelta [(0,'a',1),(1,'a',0)]) 1 "aaa",
    [1] ~=? extend (tableToDelta [(0,'a',1),(1,'a',0)]) 0 "a",
    [0] ~=? extend (tableToDelta [(0,'a',1),(1,'a',0)]) 0 "aa",
    [1] ~=? extend (tableToDelta [(0,'a',1),(1,'a',0)]) 0 "aaa",
    [] ~=? extend (tableToDelta [(3,'c',3), (3,'c',3), (3,'c',3)]) 1 "fff",
    [5] ~=? extend (tableToDelta [(3,'c',3), (3,'c',3), (3,'c',3)]) 5 "",
    [] ~=? extend (tableToDelta [(3,'c',3), (3,'c',3), (3,'c',3)]) 0 "zxc",
    [3] ~=? extend (tableToDelta [(3,'c',3), (3,'c',3), (3,'c',3)]) 3 "ccc",
    [1, 4, 5] ~=? extend (tableToDelta [(3,'c',5), (3,'c',4), (3,'c',1), (4,'c',2), (5,'c',3)]) 3 "ccccc",
    [2] ~=? extend (tableToDelta [(1, 'f', 2), (2, 'f', 2)]) 1 "ff",
    [1] ~=? extend (tableToDelta [(1, 'f', 2), (2, 'f', 2)]) 1 "",
    [2] ~=? extend (tableToDelta [(1, 'f', 2), (2, 'f', 1)]) 1 "fff",
    [2,3,4] ~=? extend (tableToDelta [(1, 'f', 2), (2, 'f', 1), (1, 'f', 3), (2, 'f', 4), (3, 'f', 3), (4, 'f', 4)]) 1 "fff"
    ]

allStringsTests = TestList [
    [""] ~=? allStrings "" !! 0,
    [""] ~=? allStrings "ahdsfljkas" !! 0,
    ["a"] ~=? allStrings "a" !! 1,
    ["aaaaaaaaaa"] ~=? allStrings "a" !! 10,
    ["a", "b", "c"] ~=? allStrings "abc" !! 1,
    ["a", "b", "c"] ~=? allStrings "cba" !! 1,
    ["x","y","z"] ~=? allStrings "zxy" !! 1,
    ["aa","ab","ac","ax","ay","az","ba",
    "bb","bc","bx","by","bz","ca","cb",
    "cc","cx","cy","cz","xa","xb","xc",
    "xx","xy","xz","ya","yb","yc","yx",
    "yy","yz","za","zb","zc","zx","zy","zz"] 
    ~=? allStrings "cbazyx" !! 2,
    ["aa", "ab", "ba", "bb"] ~=? allStrings "ab" !! 2,
    ["aaa","aab","aba","abb","baa","bab","bba","bbb"] ~=? allStrings "ab" !! 3,
    ["aaa","aam","aaz","ama","amm","amz","aza",
    "azm","azz","maa","mam","maz","mma","mmm","mmz",
    "mza","mzm","mzz","zaa","zam","zaz","zma","zmm",
    "zmz","zza","zzm","zzz"] ~=? allStrings "zam" !! 3,
    ["dd","di","do","dz","id","ii","io","iz","od",
    "oi","oo","oz","zd","zi","zo","zz"] ~=? allStrings "zoid" !! 2
    ]

empty = Automaton [0] ['a'] [] 0 [0]

ex = Automaton [0,1,2]['a','b'][(0,'a',1),(1,'a',2),(0,'b',0),(1,'b',1),(2,'b',2),(1,'a',0)] 0 [2]

a1 = Automaton [0,1] ['a'] [(0,'a',1),(1,'a',0)] 0 [0]
b1 = Automaton [0,1] ['b', 'a'] [(0,'a',1),(1,'a',0), (0,'b',1),(1,'b',0)] 0 [0]

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
    [("a",[])] ~=? possibleOutcomes empty 1 !! 1,
    [("a",[0])] ~=? possibleOutcomes a1 1 !! 1,
    [("a",[1])] ~=? possibleOutcomes a1 0 !! 1,
    [("aa",[1])] ~=? possibleOutcomes a1 1 !! 2,
    [("aa",[0])] ~=? possibleOutcomes a1 0 !! 2,
    [("aaa",[0])] ~=? possibleOutcomes a1 1 !! 3,
    [("aaa",[1])] ~=? possibleOutcomes a1 0 !! 3,
    [("a",[1]),("b",[1])] ~=? possibleOutcomes b1 0 !! 1,
    [("a",[0]),("b",[0])] ~=? possibleOutcomes b1 1 !! 1,
    [("aa",[0]),("ab",[0]),("ba",[0]),("bb",[0])] ~=? possibleOutcomes b1 0 !! 2,
    [("aa",[1]),("ab",[1]),("ba",[1]),("bb",[1])] ~=? possibleOutcomes b1 1 !! 2

    ]

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

a2 = Automaton [0,1] ['a'] [(0,'a',1)] 0 [0]

languageTests = TestList [
    ["","aa"] ~=? take 2 (language a1),
    ["","aa","aaaa","aaaaaa","aaaaaaaa","aaaaaaaaaa","aaaaaaaaaaaa"] ~=? take 7 (language a1),
    ["aa","aab"] ~=? take 2 (language ex),
    ["aa","aab","aba","baa","aaaa"] ~=? take 5 (language ex),
    ["","aa","ab","ba","bb"] ~=? take 5 (language b1),
    [""] ~=? take 1 (language empty),
    [""] ~=? take 3 (language empty),
    ["ab","bb"] ~=? take 2 (language ex2),
    ["ab","bb","cb"] ~=? take 3 (language ex2),
    ["a"] ~=? take 1 (language finite)
    ]

eq :: Automaton -> Automaton -> Bool
eq (Automaton s1 a1 ts1 i1 f1) (Automaton s2 a2 ts2 i2 f2) =
    s1 == s2 &&
    a1 == a2 &&
    ts1 == ts2 &&
    i1 == i2 &&
    f1 == f2

removeUselessTests = 
        let     a3 = (removeUseless a2) 
                f = (removeUseless finite)
                a = (removeUseless a1)
                ex3 = (removeUseless ex2)
                b2 = (removeUseless b1)
                em1 = (removeUseless empty)
        in
    TestList [
        True ~=? eq a3 (Automaton [0] ['a'] [] 0 [0]),
        True ~=? eq f (Automaton [0,1] ['a'] [(0,'a',1)] 0 [1]),
        True ~=? eq a (Automaton [0,1] ['a'] [(0,'a',1),(1,'a',0)] 0 [0]),
        True ~=? eq ex3 (Automaton [0,1,2]
                            ['a','b','c']
                            [(0,'a',1),
                            (0,'b',1),
                            (0,'c',1),
                            (1,'a',1),
                            (1,'b',2),
                            (1,'c',1),
                            (2,'c',1),
                            (2,'b',2),
                            (1,'a',0)] 0 [2]),
        True ~=? eq b2 (Automaton [0,1] ['b', 'a'] [(0,'a',1),(1,'a',0),(0,'b',1),(1,'b',0)] 0 [0]),
        True ~=? eq em1 (Automaton [0] ['a'] [] 0 [0])
    ]


infinite = Automaton [0,1] ['a'] [(0,'a',0)] 0 [0]

isFiniteLanguageTests = TestList [
    True ~=? isFiniteLanguage a2,
    False ~=? isFiniteLanguage infinite,
    True ~=? isFiniteLanguage finite,
    False ~=? isFiniteLanguage a1,
    False ~=? isFiniteLanguage ex
    ]

f = Automaton [0,1,2] ['a', 'b'] [(0,'a',1), (1,'b',2), (0,'b',0)] 0 [2]
f2 = Automaton 

language'Tests = TestList [
    [""] ~=? language' a2,
    ["a"] ~=? language' finite,
	["","a","aa"] ~=? take 3 language' infinite
    ]

a3 = Automaton [0,1,2] ['a','b'] [(0,' ',2),(0,'a',1),(2,'b',0)] 0 [1]
a4 = Automaton [0,1,2,3] ['a','b'] [(0,' ',2),(0,' ',1),(2,'b',3),(1,'b',3),(2,' ',3)] 0 [1]
a5 = Automaton [0,1,2,3] ['a','c'] [(0,'a',1),(2,'c',3)] 0 [3]
a6 = Automaton [0,1,2,3,4,5] ['a','b'] [(0,' ',1),(0,' ',4),(1,'a',2),(2,' ',3),(2,'a',3),(4,'b',5)] 0 [3]
a7 = Automaton [0,1,2,3,4,5] ['a','b'] [(0,' ',1),(0,' ',4),(1,' ',2),(2,' ',3),(2,'a',3),(4,'b',5)] 0 [3]


epsilonClosureTests = TestList [
    [0,2] ~=? epsilonClosure a3 [0],
	[1] ~=? epsilonClosure a4 [1],	
	[0,1,2,3] ~=? epsilonClosure a4 [0,2],
	[] ~=? epsilonClosure a4 [],
	[1,2,3] ~=? epsilonClosure a5 [1,2,3],
	[1] ~=? epsilonClosure a5 [1,4,5],
	[0,1,4] ~=? epsilonClosure a6 [0],
	[2,3,4] ~=? epsilonClosure a6 [2,4],
	[0,1,2,3,4] ~=? epsilonClosure a7 [0]
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
