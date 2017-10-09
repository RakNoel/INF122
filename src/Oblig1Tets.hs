module Oblig1Tets where

import Test.HUnit
import Oblig1

-- MAIN
tests = runTestTT $ TestList [
        TestLabel "test1" test1,
        TestLabel "test2" test2,
        TestLabel "test3" test3,
        TestLabel "test4" test4,
        TestLabel "test5" test5,
        TestLabel "test6" test6,
        TestLabel "test7" test7,
        TestLabel "test8" test8,
        TestLabel "test9" test9,
        TestLabel "test10" test10,
        TestLabel "test11" test11,
        TestLabel "test12" test12,
        --TestLabel "test13" test13,
        TestLabel "test14" test14
    ]

test1 = TestCase (assertEqual
        "Normal easy parse:"
        (Mul (Nr 2) (Sum (Nr 3) (Nr 4)))
        (parse "* 2 + 3 4")
    )

test2 = TestCase (assertEqual
        "LET parsing:"
        (Let "X" (Sum (Nr 1) (Nr 2)) (Mul (Var "X") (Sum (Nr 2) (Min (Var "X")))))
        (parse "let X = + 1 2 in * X + 2 - X")
    )

test3 = TestCase (assertEqual
        "evi with let expression:"
        (-3)
        (evi "let X = + 1 2 in * X + 2 - X")
    )

test4 = TestCase (assertEqual
        "evb with let expression:"
        False
        (evb "let X = + 1 2 in * X + 2 - X")
    )

test5 = TestCase (assertEqual
        "evi with simple let nr:"
        2
        (evi "let X = 1 in let X = 2 in X")
    )

test6 = TestCase (assertEqual
        "evb with simple let nr:"
        False
        (evb "let X = 1 in let X = 2 in X")
    )

test7 = TestCase (assertEqual
        "evi with simple let and if:"
        6
        (evi "let X = + 1 2 in if X + 1 X * 2 X")
    )

test8 = TestCase (assertEqual
        "evb with simple let and if:"
        True
        (evb "let X = + 1 2 in if X + 1 X * 2 X")
    )

test9 = TestCase (assertEqual
        "evi simple"
        8
        (evi "* let X = 3 in + X 1 2")
    )

test10 = TestCase (assertEqual
        "evb simple"
        False
        (evb "* let X = 3 in + X 1 2")
    )

test11 = TestCase (assertEqual
        "evi doubble let:"
        24
        (evi "let A = 2 in * let B = 3 in let A = 4 in * A B A")
    )

test12 = TestCase (assertEqual
        "evb doubble let:"
        False
        (evb "let A = 2 in * let B = 3 in let A = 4 in * A B A")
    )

{- Should produce an error!
test13 = TestCase (assertEqual
        "FAIL TEST:"
        0
        (evi "* let X = 3 in + X 1 X")
    )
-}

test14 = TestCase (assertEqual
        "Let X = 2 in Let X = (X + 2) == 4"
        4
        (evi "let X = 2 let X = + X 2 X")
    )