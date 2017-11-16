module Oblig2Tests where

import Test.HUnit
import Leirvaag


-- MAIN
tests = runTestTT $ TestList [
        TestLabel "test1" test1,
        TestLabel "test2" test2,
        TestLabel "test2" test3
    ]

test1 = TestCase (assertEqual
        "Get neibhrs:"
        [(19,20),(21,20),(20,21),(20,19),(21,21),(19,19),(19,21),(21,19)]
        (getNeibrs (20,20) 30)
    )

test2 = TestCase (assertEqual
        "Get neibhrs EDGE:"
        [(2,1),(1,2),(2,2)]
        (getNeibrs (1,1) 10)
    )

test3 = TestCase (assertEqual
        "Remove duplicates"
        (mergeSort [(19,20),(21,20),(20,21),(20,19),(21,21),(19,19),(19,21),(21,19)])
        (mergeSort $ rmdupes [(19,20),(21,20),(20,21),(19,19),(19,21),(21,19),(20,19),(21,21),(19,19),(19,21),(21,19)])
    )