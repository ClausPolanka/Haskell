module Aufgabe6Exercise4Test where

import HUnit
import Aufgabe6

mapLTSimple = 
    TestCase (assertEqual "New LTree" (LNode 10 []) (mapLT (*2) (LNode 5 [])))

funkProgAcceptanceTest2 = 
    TestCase (assertEqual "New LTree" 
        (LNode 10 [LNode 20 [], LNode 30 [LNode 40 [], LNode 50 []]]) 
        (mapLT (*2) (LNode 5 [LNode 10 [], LNode 15 [LNode 20 [], LNode 25 []]])))

funkProgAcceptanceTest3 = 
    TestCase (assertEqual "New LTree" 
        (LNode 10 [LNode 20 [LNode 30 [], LNode 40 [LNode 50 []]]])
        (mapLT (*2) (LNode 5 [LNode 10 [LNode 15 [], LNode 20 [LNode 25 []]]])))

allTests = 
    TestList [
        TestLabel 
        " Given an tree containing only one node, calculate this node's value." 
        mapLTSimple,

        TestLabel 
        " FunkProg Acceptance Test 2." 
        funkProgAcceptanceTest2,

        TestLabel 
        " FunkProg Acceptance Test 3." 
        funkProgAcceptanceTest3
    ]

main = do runTestTT allTests