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

--evi::String -> Int

--evb::String -> Bool