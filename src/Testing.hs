module Testing where

import Data.List
import Test.QuickCheck

permsList = permutations [0,1,2,3,4,5,6,7]

mergeSort:: (Ord a) => [a] -> [a]
mergeSort [] = []
mergeSort [x] = [x]
mergeSort xs = mergeSort (take half xs) `merge` mergeSort (drop half xs)
    where
        half = length xs `div` 2

merge:: (Ord a) => [a] -> [a] -> [a]
merge [] xs = xs
merge xs [] = xs
merge (x:xs) (y:ys)
    | x > y = y : ( (x:xs) `merge` ys )
    | x < y = x : ( xs `merge` (y:ys) )
    | otherwise = [x,y] ++ ( xs `merge` ys )

fack:: Int -> Int
fack 0 = 1
fack x = x * fack (x-1)

chkMergeSort = quickCheck
    (\i -> mergeSort (i::[Int]) == sort (i::[Int]))

chkPermsList = length permsList == fack 8