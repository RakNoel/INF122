{- Oblig 1, av Oskar L. F. Leirvåg -}

module Oblig1 where
import Data.Char

data Ast = Nr Int | Sum Ast Ast | Mul Ast Ast | Min Ast | If Ast Ast Ast | Let String Ast Ast | Var String
    deriving (Eq, Show)

--MAIN
parse :: String -> Ast
parse = fst . parseExpr . words

--Parser
parseExpr :: [String] -> (Ast, [String])
parseExpr ("+":xs) =
    let (a,b) = parseExpr xs in
        if null b then (a,b)
        else let (x,z) = parseExpr b in (Sum a x, z)

parseExpr ("-":xs) =
    let (a,b) = parseExpr xs in (Min a, b)

parseExpr ("*":xs) =
    let (a,b) = parseExpr xs in
        if null b then (a,b)
        else let (x,z) = parseExpr b in (Mul a x, z)

parseExpr ("if":xs) =
    (If x y z, c)
        where
            (x,a) = parseExpr xs
            (y,b) = parseExpr a
            (z,c) = parseExpr b

parseExpr ("let":x:"=":xs) =
    let (func, b) = getVarExpr xs ; (c, d) = parseExpr b in
        if isUpper (head x) && length x == 1 then (Let x (fst $ parseExpr func) c, d )
        else error "Illegal varriable"

parseExpr (x:xs)
    | isDigit (head x) = (Nr (read x), xs)
    | isUpper (head x) = (Var x, xs)

parseExpr _ = error "No pattern match"

--Read let variable expression
getVarExpr :: [String] -> ([String],[String])
getVarExpr [] = ([],[])
getVarExpr ("in":xs) = ([],xs)
getVarExpr (x:xs) = let (a,b) = getVarExpr xs in (x : a, b)

--Evaluate integer
evi::String -> Int
evi xs = eval (parse xs) 0 (+) (*) negate id (== 0)

--Evaluate Boolean
evb::String -> Bool
evb xs = eval (parse xs) False (||) (&&) not odd id


{-
    This eval function takes an Abstract Syntax tree and many functons to recursivly use theese functions
    and produce a result.
-}
--      Var Var-value    Sum              Mul          Min         Handling      If val        Result
eval :: Ast -> a -> (a -> a -> a) -> (a -> a -> a) -> (a -> a) -> (Int -> a) -> (a -> Bool) -> a
eval (If var t f) z f1 f2 f3 e1 e2    = if e2 (eval var z f1 f2 f3 e1 e2)  then eval t z f1 f2 f3 e1 e2 else eval f z f1 f2 f3 e1 e2
eval (Let _ val ex) z f1 f2 f3 e1 e2  = eval ex (eval val z f1 f2 f3 e1 e2) f1 f2 f3 e1 e2
eval (Sum a b) z f1 f2 f3 e1 e2       = f1 (eval a z f1 f2 f3 e1 e2) (eval b z f1 f2 f3 e1 e2)
eval (Mul a b) z f1 f2 f3 e1 e2       = f2 (eval a z f1 f2 f3 e1 e2) (eval b z f1 f2 f3 e1 e2)
eval (Min a) z f1 f2 f3 e1 e2         = f3 (eval a z f1 f2 f3 e1 e2)
eval (Nr a) _ _ _ _ e _               = e a
eval (Var _) z _ _ _ _ _              = z

