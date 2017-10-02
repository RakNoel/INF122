{- Oblig 1, av Oskar L. F. Leirvåg -}

module Oblig1 where
import Data.Char

data Ast = Nr Int | Sum Ast Ast | Mul Ast Ast | Min Ast | If Ast Ast Ast | Let String Ast Ast | Var String
    deriving (Eq, Show)

parse :: String -> Ast
parse = fst . parseExpr . words

parseExpr :: [String] -> (Ast, [String])
parseExpr ("+":xs) =
    let (a,b) = parseExpr xs in
        if null b then (a,b)
        else let (x,z) = parseExpr b in
            (Sum a x, z)

parseExpr ("-":xs) =
    let (a,b) = parseExpr xs in
            (Min a, b)

parseExpr ("*":xs) =
    let (a,b) = parseExpr xs in
        if null b then (a,b)
        else let (x,z) = parseExpr b in
            (Mul a x, z)

parseExpr ("if":xs) =
    let (x,a) = parseExpr xs in
    let (y,b) = parseExpr a in
    let (z,c) = parseExpr b in
        (If x y z, c)

parseExpr ("let":x:"=":xs) =
    let (func, b) = getVarExpr xs in
    let (c, d) = parseExpr b in
    if isUpper (head x) && length x == 1 then (Let x (fst (parseExpr func)) c, d )
    else error "Illegal varriable"

parseExpr (x:xs)
    | isDigit (head x) = (Nr (read x), xs)
    | isUpper (head x) = (Var x, xs)

parseExpr _ = error "No pattern match"

getVarExpr :: [String] -> ([String],[String])
getVarExpr [] = ([],[])
getVarExpr ("in":xs) = ([],xs)
getVarExpr (x:xs) = let (a,b) = getVarExpr xs in (x : a, b)

evi::String -> Int
evi xs = evival $ parse xs

evival::Ast -> Int
evival (Sum a b) = evival a + evival b
evival (Mul a b) = evival a * evival b
evival (Min a) = -evival a
evival (If ev a b)
    | evival ev == 0 = evival a
    | otherwise = evival b
evival (Nr a) = a

evb::String -> Bool
evb xs = evbval $ parse xs

evbval:: Ast -> Bool
evbval (Sum a b) = evbval a || evbval b
evbval (Mul a b) = evbval a && evbval b
evbval (Min a) = not $ evbval a
evbval (If ev a b )
    | evbval ev = evbval a
    | otherwise = evbval b
evbval (Nr a)
    | a `mod` 2 == 0 = False
    | otherwise = True

