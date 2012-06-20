module Aufgabe6Exercise3Test where

import HUnit
import Aufgabe6

mkControlSimple = 
    TestCase (assertEqual "Control" "lmr" (mkControl "abcldefmxyzrabc"))

mkControlAdvanced = 
    TestCase (assertEqual "Control" "rrllmmrr" (mkControl "rrxxlabcldmefmxyzrabrc"))

fooBar = 
    TestCase (assertEqual "Result" 3 (apply "aslfjarsöfjml" 1 (Leaf (*3))))

fooBarLeft = 
    TestCase (assertEqual "Result" 45 
        (apply "l" 3 (Node (*3) (Leaf (*5)) (Leaf (*7)) (Leaf (*4))) ))

fooBarMiddle = 
    TestCase (assertEqual "Result" 63 
        (apply "m" 3 (Node (*3) (Leaf (*5)) (Leaf (*7)) (Leaf (*4))) ))

fooBarRight = 
    TestCase (assertEqual "Result" 36 
        (apply "r" 3 (Node (*3) (Leaf (*5)) (Leaf (*7)) (Leaf (*4))) ))

funkProgAcceptanceTest1 =
    TestCase (assertEqual "Result" 13 
        (apply ['r', 'l', 'l', 'm'] 5 (Leaf (\x -> 2 * x + 3))))

funkProgAcceptanceTest2 =
    TestCase (assertEqual "Result" 70 
        (apply ['r', 'l', 'l', 'm'] 5 (Node (*2) (Leaf (*3)) (Leaf (*5)) (Leaf (*7)))))

funkProgAcceptanceTest3 =
    TestCase (assertEqual "Result" 55 
        (apply ['m', 'l'] 5 (Node (*2) 
                                (Leaf (*3)) 
                                (Node (+1) (Leaf (*5)) (Leaf (+5)) (Leaf (+2))) 
                                (Leaf (*7)))))
funkProgAcceptanceTest4 =
    TestCase (assertEqual "Result" 10 
        (apply [] 5 (Node (*2) (Leaf (*3)) (Leaf (*5)) (Leaf (*7)))))

allTests = 
    TestList [
        TestLabel 
        " Given a random string, delete all characters different from l, m and r." 
        mkControlSimple,

        TestLabel 
        " Given an advanced random string, delete all characters different from l, m and r." 
        mkControlAdvanced,

        TestLabel 
        " Apply function *3 to given tree." 
        fooBar,

        TestLabel 
        " Apply functions to a given tree containing 2 child nodes." 
        fooBarLeft,

        TestLabel 
        " Apply functions to a given tree containing 2 child nodes." 
        fooBarMiddle,

        TestLabel 
        " Apply functions to a given tree containing 2 child nodes." 
        fooBarRight,

        TestLabel 
        " FunkProg Acceptance Test 1." 
        funkProgAcceptanceTest1,

        TestLabel 
        " FunkProg Acceptance Test 2." 
        funkProgAcceptanceTest2,

        TestLabel 
        " FunkProg Acceptance Test 3." 
        funkProgAcceptanceTest3,

        TestLabel 
        " FunkProg Acceptance Test 4." 
        funkProgAcceptanceTest4
    ]

main = do runTestTT allTests