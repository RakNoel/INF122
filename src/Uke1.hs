module Uke1 where

import Test.QuickCheck

{-# ANN module ("HLint: ignore Redundant bracket"::String) #-}
{-# ANN module ("HLint: ignore Use last"::String) #-}
{-# ANN module ("HLint: ignore Use foldr"::String) #-}

-- 1.1
double' :: (Num t) => t -> t
double' x = x * 2

taskOne :: (Num t) => t -> t
taskOne x = double' $ double' x
taskOne' x = 4 * x

-- 1.2
sum' :: (Num t) => [t] -> t
sum' [] = 0
sum' xs = head xs + sum' (tail xs)

{-
    sum' [x] = x + sum'[]
    sum' [x] = x + 0
    sum' [x] = x
-}

-- 1.3
product' :: (Num t) => [t] -> t
product' [] = 1
product' (x:xs) = x * product' xs

-- 1.4
qs :: (Ord a) => [a] -> [a]
qs [] = []
qs (x:xs) = qs lower ++ [x] ++ qs larger
    where
        lower = [y | y <- xs, y < x]
        larger = [y | y <- xs, y >= x]

qs' :: (Ord a) => [a] -> [a]
qs' [] = []
qs' (x:xs) = qs' larger ++ [x] ++ qs' lower
    where
        lower = [y | y <- xs, y < x]
        larger = [y | y <- xs, y >= x]

-- 1.5
{-
    This function will not only sort but also drop numbers that are equal
    which means sorting [2,2,1,3,1] would result in [1,2,3]
-}

qsUnique :: (Ord a) => [a] -> [a]
qsUnique [] = []
qsUnique (x:xs) = qsUnique lower ++ [x] ++ qsUnique larger
    where
        lower = [y | y <- xs, y < x]
        larger = [y | y <- xs, y > x]

-- 2.1
{- Shoot me plz -}

-- 2.2
{-
    (2^3)*4
    (2*3) + (4*5)
    2+(3*(4^5))
-}

-- 2.3
n = a `div` length xs
    where
        a = 10
        xs = [1..5]

-- 2.4
last' xs = head $ reverse xs -- Aids

-- 2.5
init' xs = reverse $ drop 1 ( reverse xs ) --Aids

-- B.1
plu :: [Int] -> Int -> [Int]
plu k n = [e + n  | e <- k]

-- B.2
pali :: String -> Bool
pali [] = True
pali [x] = True
pali (x:xs) = ( x == (last xs)) && ( pali (init xs) )

-- B.3
chkplu = quickCheck
    (\rList rInt ->
        (plu rList rInt) == map (+rInt) rList
    )

chkpali = quickCheck
    (\rStr ->
        pali rStr == (rStr == reverse rStr)
    )

-- C.1
listInverser :: (Ord a) => [[a]] -> [[a]]
listInverser = map reverse

-- D.1
del :: Int -> [Int]
del n = [x | x <- [1..n], ((x `mod` 3) == 0) || ((x `mod` 5) == 0) ]

-- D.2
dell :: Int -> [Int] -> [Int]
dell _ [] = []
dell n (j:js) = [x | x <- [1..n], ((x `mod` j) == 0) ] ++ (dell n js)