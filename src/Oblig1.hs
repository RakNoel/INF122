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

--ParserTest
parseExpr :: [String] -> (Ast, [String])
parseExpr [] = error "Reached bottom but no result"
parseExpr (selector:list) =
    case selector of
        ""                  -> error "Reached bottom but no result"
        "+"                 -> if null r1 then (a1,r1) else (Sum a1 a2, r2)
        "*"                 -> if null r1 then (a1,r1) else (Mul a1 a2, r2)
        "-"                 -> (Min a1, r1)
        "if"                -> (If a1 a2 a3, r3)
        "="                 -> parseExpr list
        "in"                -> parseExpr list
        "let"               -> if isUpper f && null fs then (Let [f] if1 if2, ifr2)
                               else error $ "Illegal varriable" ++ f:fs ++ ". Must be one letter upper case"
        (x:xs)  | isDigit x -> (Nr (read (x:xs)), list)
                | isUpper x -> (Var [x], list)
                | otherwise -> error $ "No parse match for: '" ++  (x:"' before ") ++ show xs

    where
        (a1,r1) = parseExpr list ; (a2,r2) = parseExpr r1 ; (a3,r3) = parseExpr r2
        (f:fs) = head list ; (if1, ifr1) = parseExpr $ tail list ; (if2, ifr2) = parseExpr ifr1 -- Let exclusive

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
--                Var    Var-values       Sum              Mul              Min         Handling      If v        Result
eval :: (Eq a) => Ast -> [(String, a)] -> (a -> a -> a) -> (a -> a -> a) -> (a -> a) -> (Int -> a) -> (a -> Bool) -> a
eval ast z f1 f2 f3 e1 e2 = case ast of
    (If var t f)     -> if e2 $ evalEx var then evalEx t else evalEx f
    (Let var val ex) -> eval ex ((var, evalEx val):z) f1 f2 f3 e1 e2
    (Sum a b)        -> f1 (evalEx a) (evalEx b)
    (Mul a b)        -> f2 (evalEx a) (evalEx b)
    (Min a)          -> f3 (evalEx a)
    (Var v)          -> head [y | (x,y) <- z, x == v]
    (Nr a)           -> e1 a
    where evalEx xs = eval xs z f1 f2 f3 e1 e2

{-
    This method will check trough the AST and return any variables that is undeclared
    within its scope.
-}
--          Ast     Let       Undeclared
varCheck :: Ast -> [String] -> [String]
varCheck ast p = case ast of
    (If var t f) -> varCheckEx var ++ varCheckEx t ++ varCheckEx f
    (Let s a b)  -> varCheck a (s:p) ++ varCheck b (s:p)
    (Var s)      -> if s `elem` p then [] else [s]
    (Sum a b)    -> varCheckEx a ++ varCheckEx b
    (Mul a b)    -> varCheckEx a ++ varCheckEx b
    (Min a)      -> varCheckEx a
    (Nr _)       -> []
    where varCheckEx x = varCheck x p