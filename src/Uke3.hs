module Uke3 where
-- A

-- 4.3
safetail :: [a] -> [a]
safetail xs = if null xs then [] else tail xs

safetail' :: [a] -> [a]
safetail' xs
    | null xs = []
    | otherwise = tail xs

safetail'' :: [a] -> [a]
safetail'' [] = []
safetail'' xs = tail xs

-- 4.4
-- (||) :: Bool -> Bool -> Bool
-- True || True = True
-- True || False = True
-- False || True = True
-- False || False = False
--
-- (||) :: Bool -> Bool -> Bool
-- False || False = False
-- _ || _ = True
--
-- (||) :: Bool -> Bool -> Bool
-- False || b = b
-- True || _ = True
--
-- (||) :: Bool -> Bool -> Bool
-- a || a = a
-- _ || _ = True

-- 4.5
-- (&&) :: Bool -> Bool -> Bool
-- a && a = a
-- a && b = False

-- B
{- f x = 1^2 + 2^2 + … + x^2 -}
f :: Int -> Int
f 0 = 0
f x = ( x^2 ) + f (x - 1)

f' :: Int -> Int
f' x = sum [n^2 | n <- [1..x]]

f'' :: Int -> Int
f'' x = sum $ map (^2) [1..x]
-- f'' = sum $ map (\x -> (x^2)) [1..x]

-- C
luhnDouble :: Int -> Int
luhnDouble x = (x*2) `mod` 9

luhn :: Int -> Int -> Int -> Int -> Bool
luhn a b c d = mod (luhnDouble c + luhnDouble a + b + d) 10 == 0

-- D
tolist :: Int -> [Int]
tolist 0 = []
tolist x = tolist (div x 10) ++ [mod x 10]

-- tolist' :: Int -> [Int]
-- tolist' x = [i | i <- , mod x i == 0 ]

-- E

-- 5.2
grid :: Int -> Int -> [(Int, Int)]
grid x y = [(i,j) | i <- [0..x], j <- [0..y] ]

-- 5.3
square :: Int -> [(Int, Int)]
square x = [(i,j) | i <- [0..x], j <- [0..x], j /= i ]

-- 5.4
replicate' :: Int -> a -> [a]
replicate' x a = [a | _ <- [1..x] ]