module Uke1Tests where

import Test.QuickCheck
import Uke1


{-# ANN module ("HLint: ignore Redundant bracket"::String) #-}
{-# ANN module ("HLint: ignore Use last"::String) #-}
{-# ANN module ("HLint: ignore Use foldr"::String) #-}


chktaskOne = quickCheck
    (\i ->
        (taskOne (i :: Int)) == (taskOne' (i :: Int))
    )

chksum' = quickCheck
    (\i ->
        (sum' (i :: [Int])) == (sum (i :: [Int]))
    )

chkplu = quickCheck
    (\rList rInt ->
        (plu rList rInt) == map (+rInt) rList
    )

chkpali = quickCheck
    (\rStr ->
        pali rStr == (rStr == reverse rStr)
    )

chkAll :: IO()
chkAll = do
    chktaskOne
    chksum'
    chkplu
    chkpali