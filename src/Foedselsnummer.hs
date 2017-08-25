module Foedselsnummer where

illegalNumberException = error "Illegal number combination"

{- DONT WRITE ANY PIN IN HERE, ITS ON GITHUB! -}

{-
    These are functions to calculate the control
    numbers on a Norwegian - BirthNumber
-}

-- Control number one
getC1 :: [Int] -> Int
getC1 (d1:d2:m1:m2:y1:y2:i1:i2:i3:_)
    | calc == 0 = calc
    | calc < 10 = 11 - calc
    | otherwise = illegalNumberException
        where
            calc =
                ( (3*d1) + (7*d2) + (6*m1)
                + (1*m2) + (8*y1) + (9*y2)
                + (4*i1) + (5*i2) + (2*i3)
                ) `mod` 11

-- Control number two
getC2 :: [Int] -> Int -> Int
getC2 (d1:d2:m1:m2:y1:y2:i1:i2:i3:_) c1
    | calc == 0 = calc
    | calc < 10 = 11 - calc
    | otherwise =  illegalNumberException
        where
            calc =
                ( (5*d1) + (4*d2) + (3*m1)
                + (2*m2) + (7*y1) + (6*y2)
                + (5*i1) + (4*i2) + (3*i3)
                + (2*c1)
                ) `mod` 11

-- The complete control-number c1 and c2
getCC :: [Int] -> (Int,Int)
getCC xs = (c1, c2)
    where
        c1 = getC1 xs
        c2 = getC2 xs c1

checkNumber :: [Int] -> Bool
checkNumber xs = (c1 == cc1) && (c2 == cc2)
    where
        cc1 = fst ccc
        cc2 = snd ccc
        ccc = getCC xs
        c1 = xs !! 10
        c2 = xs !! 11