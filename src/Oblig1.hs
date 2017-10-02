{- Oblig 1, av Oskar L. F. Leirvåg -}

module Oblig1 where
import Data.Char

data Ast = Nr Int | Sum Ast Ast | Mul Ast Ast | Min Ast | If Ast Ast Ast | Let String Ast Ast | Var String
    deriving (Eq, Show)

parse :: String -> Ast
parse = fst . parseExpr . words

parseExpr :: [String] -> (Ast, [String])
parseExpr ("+":xs) = let (a,b) = parseExpr xs in if null b then (a,b) else let (x,z) = parseExpr b in (Sum a x, z)
parseExpr ("-":xs) = let (a,b) = parseExpr xs in (Min a, b)
parseExpr ("*":xs) = let (a,b) = parseExpr xs in if null b then (a,b) else let (x,z) = parseExpr b in (Mul a x, z)
parseExpr ("if":xs) = let (x,a) = parseExpr xs in let (y,b) = parseExpr a in let (z,c) = parseExpr b in (If x y z, c)
parseExpr ("let":x:"=":xs) = let (func, b) = getVarExpr xs in let (c, d) = parseExpr b in
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
evi xs = eval (parse xs) ([],0) (+) (*) (negate) (\x -> x) (\x -> (x == 0))

evb::String -> Bool
evb xs = eval (parse xs) ([],True) (||) (&&) not (\x -> (mod x 2 == 1)::Bool) (\x -> x)

{-      Var     Var-value     Sum              Mul              Min         Handling      If val       -}
eval :: Ast -> (String, a) -> (a -> a -> a) -> (a -> a -> a) -> (a -> a) -> (Int -> a) -> (a -> Bool) -> a
eval (If var t f) (x,z) f1 f2 f3 e1 e2  = if e2 (eval var (x,z) f1 f2 f3 e1 e2)  then eval t (x,z) f1 f2 f3 e1 e2 else eval f (x,z) f1 f2 f3 e1 e2
eval (Let st val ex) (x,z) f1 f2 f3 e1 e2 = eval ex (st, eval val (x,z) f1 f2 f3 e1 e2) f1 f2 f3 e1 e2
eval (Var st) (x,z) _ _ _ _ _ = z
eval (Sum a b) (x,z) f1 f2 f3 e1 e2 = f1 (eval a (x,z) f1 f2 f3 e1 e2) (eval b (x,z) f1 f2 f3 e1 e2)
eval (Mul a b) (x,z) f1 f2 f3 e1 e2 = f2 (eval a (x,z) f1 f2 f3 e1 e2) (eval b (x,z) f1 f2 f3 e1 e2)
eval (Min a) (x,z) f1 f2 f3 e1 e2 = f3 (eval a (x,z) f1 f2 f3 e1 e2)
eval (Nr a) (_,_) _ _ _ e _ = e a