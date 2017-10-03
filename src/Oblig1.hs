{- Oblig 1, av Oskar L. F. Leirvåg -}

module Oblig1 where
import Data.Char

data Ast = Nr Int | Sum Ast Ast | Mul Ast Ast | Min Ast | If Ast Ast Ast | Let String Ast Ast | Var String
    deriving (Eq, Show)

--MAIN
parse :: String -> Ast
parse xs = let a = fst $ parseExpr $ words xs ; vars = varCheck a [] in
    if null vars then a
    else error $ "Variable(s): " ++ show vars ++ " is undeclared in its scope"

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
    let (func, b) = parseExpr xs ; (c, d) = parseExpr b in
        if isUpper (head x) && length x == 1 then (Let x func c, d )
        else error $ "Illegal varriable" ++ x ++ ". Must be one letter upper case"

parseExpr ("in":xs) = parseExpr xs

parseExpr (x:xs)
    | isDigit (head x) = (Nr (read x), xs)
    | isUpper (head x) = (Var x, xs)

parseExpr (x:xs) = error $ "No parse match for: '" ++ x ++ "' before " ++ show xs
parseExpr _ = error "Reached bottom but no result"

--Evaluate integer
evi::String -> Int
evi xs = eval (parse xs) [] (+) (*) negate id (== 0)

--Evaluate Boolean
evb::String -> Bool
evb xs = eval (parse xs) [] (||) (&&) not odd id


{-
    This eval function takes an Abstract Syntax tree and many functions to recursively use these functions
    and produce a result. Looks nightmareish and still does when you understand it
-}
--      Var    Var-values    Sum              Mul          Min         Handling      If val        Result
eval :: Ast -> [(String, a)] -> (a -> a -> a) -> (a -> a -> a) -> (a -> a) -> (Int -> a) -> (a -> Bool) -> a
eval (If var t f) z f1 f2 f3 e1 e2      = if e2 (eval var z f1 f2 f3 e1 e2) then eval t z f1 f2 f3 e1 e2 else eval f z f1 f2 f3 e1 e2
eval (Let var val ex) z f1 f2 f3 e1 e2  = eval ex ((var, eval val z f1 f2 f3 e1 e2):z) f1 f2 f3 e1 e2
eval (Sum a b) z f1 f2 f3 e1 e2         = f1 (eval a z f1 f2 f3 e1 e2) (eval b z f1 f2 f3 e1 e2)
eval (Mul a b) z f1 f2 f3 e1 e2         = f2 (eval a z f1 f2 f3 e1 e2) (eval b z f1 f2 f3 e1 e2)
eval (Min a) z f1 f2 f3 e1 e2           = f3 (eval a z f1 f2 f3 e1 e2)
eval (Var v) z _ _ _ _ _                = head [y | (x,y) <- z, x == v]
eval (Nr a) _ _ _ _ e _                 = e a

{-
    This method will check trough the AST and return any variables that is undeclared
    within its scope.
-}
--          Ast     Let       Undeclared
varCheck :: Ast -> [String] -> [String]
varCheck (If var t f) p = varCheck var p ++ varCheck t p ++ varCheck f p
varCheck (Let s a b) p  = varCheck a (s:p) ++ varCheck b (s:p)
varCheck (Var s) p      = if s `elem` p then [] else [s]
varCheck (Sum a b) p    = varCheck a p ++ varCheck b p
varCheck (Mul a b) p    = varCheck a p ++ varCheck b p
varCheck (Min a) p      = varCheck a p
varCheck (Nr _) _       = []