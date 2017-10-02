{- Oblig 1, av Oskar L. F. Leirvåg -}

module Oblig1 where
import Data.Char

data Ast = Nr Int | Sum Ast Ast | Mul Ast Ast | Min Ast deriving (Eq, Show)

parse :: String -> Ast
parse xs = fst $ parseExpr xs

parseExpr :: String -> (Ast, String)
parseExpr ('+':' ':xs) =
    let (a,b) = parseExpr xs in
        if null b then (a,b)
        else let (x,z) = parseExpr b in
            (Sum a x, z)

parseExpr ('-':' ':xs) =
    let (a,b) = parseExpr xs in
            (Min a, b)

parseExpr ('*':' ':xs) =
    let (a,b) = parseExpr xs in
        if null b then (a,b)
        else let (x,z) = parseExpr b in
            (Mul a x, z)

parseExpr xs =
    let (a,b) = parseNr xs in (Nr (read a), b)

parseNr :: String -> (String, String)
parseNr [] = ([],[])
parseNr (x:xs)
    | isDigit x = let (a,b) = parseNr xs in (x : a , b)
    | otherwise = ([], xs)


evi::String -> Int
evi xs = evival $ parse xs

evival::Ast -> Int
evival (Sum a b) = evival a + evival b
evival (Mul a b) = evival a * evival b
evival (Min a) = -evival a
evival (Nr a) = a

evb::String -> Bool
evb xs = evbval $ parse xs

evbval:: Ast -> Bool
evbval (Sum a b) = evbval a || evbval b
evbval (Mul a b) = evbval a && evbval b
evbval (Min a) = not $ evbval a
evbval (Nr a)
    | a `mod` 2 == 0 = False
    | otherwise = True