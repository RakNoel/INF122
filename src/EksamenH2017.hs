module EksamenH2017 where

import Data.List
import Data.Char


harEl :: (t -> Bool) -> [t] -> Bool
harEl = any

el :: (t -> Bool) -> [t] -> t
el f (x:xs) = if f x then x else el f xs

gRep :: (t -> Bool) -> t -> [t] -> [t]
gRep f t = map (\x -> if f x then t else x)

data BT = B Int | N BT Int BT

elt :: BT -> Int -> Bool
elt tr x = case tr of
    B n -> n == x
    N r1 n r2 -> elt r1 x || n == x || elt r2 x

toL :: BT -> [Int]
toL tr = case tr of
    B n -> [n]
    N r1 n r2 -> toL r1 ++ [n] ++ toL r2

dup :: BT -> Bool
dup tr = length (toL tr) /= length (foldl (\xs x -> if x `elem` xs then xs else x:xs ) [] (toL tr))

naboL :: (Eq t) => [(t,t)] -> [(t,[t])]
naboL = foldl (\l (x,y) -> let fuc = ((==x).fst) in let elm = el fuc l in (if harEl fuc l then gRep fuc (fst elm, y : snd elm) l else (x,[y]):l)) []

kantL :: (Eq t) => [(t,[t])] -> [(t,t)]
kantL = foldl (\l (x,y) -> foldl (\sl e -> (x,e):sl) [] y ++ l) []

-- Velger Naboliste
naboer :: (Eq t) => [(t,[t])] -> t -> [t]
naboer lx n = if harEl ((==n).fst) lx then snd $ el ((==n).fst) lx else []

cyc :: (Eq t) => [(t,[t])] -> t -> [t]
cyc lx t = head $ bfs lx [] t (naboer lx t)


bfs :: (Eq t) => [(t,[t])] -> [t] -> t -> [t] -> [[t]]
bfs _ _ _ [] = []
bfs grp vis now (x:todo)
    | x `elem` vis = [reverse (now:vis) ++ [x]]
    | otherwise = concatMap (\x -> bfs grp (now:vis) x (nbh x)) (nbh now)
    where nbh = naboer grp

allcyc :: (Eq t) => [(t,[t])] -> [[t]]
allcyc lx = let res = map (\(a,b) -> cyc lx a) lx in if null res then [] else res

main :: IO()
main = do
    putStrLn "Hello, graph calculations"
    mainhelper []

mainhelper :: [(String,String)] -> IO()
mainhelper grp = do
    putStr "Current graph: "
    print (naboL grp)
    putStrLn "----------------------------"
    com <- getLine
    case words com of
        ("g":_) -> do putStrLn "Created new graph" ; mainhelper []
        ("k":x:y:_) -> do putStrLn ("Added (" ++ x ++ "," ++ y ++ ") to graph"); mainhelper ((x,y):grp)
        ("f":x:y:_) -> do putStrLn ("Removed (" ++ x ++ "," ++ y ++ ") from graph") ; mainhelper (filter (\(xx,yy) -> xx/=x || yy/=y) grp)
        ("s":_) -> let res = allcyc (naboL grp) in do putStr "Cycles: " ; print res ; mainhelper grp
        ("q":_) -> putStrLn "Bye"
        _ -> do putStrLn "Unknown command, try again" ; mainhelper grp