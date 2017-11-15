{- Oskar Leonhard Fretheim Leirvåg -}

module Main where

import Control.Concurrent
import System.Exit

type Pos = (Int, Int)
type Board = [Pos]

helpString :: String
helpString = "\n\n"
    ++ "This program will emulate Conway's Game-Of-Life in a grid of size N \n\n"
    ++ "Commands: \n"
    ++ "c x     ->  Creates and displays a new grid \n"
    ++ "n x y   ->  Creates live cell at position (x,y) \n"
    ++ "d x y   ->  Deletes / kills cell at position (x,y) \n"
    ++ "... to be continued \n"

isAlive :: Board -> Pos -> Bool
isAlive b p = p `elem` b

isDead :: Board -> Pos -> Bool
isDead b p = not $ isAlive b p

liveNeibrs :: Board -> Int -> Pos -> Int
liveNeibrs b s p = length $ filter (isAlive b) $ getNeibrs p s

getNeibrs :: Pos -> Int -> [Pos]
getNeibrs (x,y) s = map (`wrap` s) [(x-1,y),(x+1,y),(x,y+1),(x,y-1),(x+1,y+1),(x-1,y-1),(x-1,y+1),(x+1,y-1)]

wrap :: Pos -> Int -> Pos
wrap (x,y) s = (((x-1) `mod` s) + 1, ((y-1) `mod` s) + 1)

survivors :: Board -> Int -> [Pos]
survivors b s = [p | p <- b, liveNeibrs b s p `elem` [2,3]] -- where [2,3] is accepted alive neibrs for gamerules

births :: Board -> Int -> [Pos]
births b s = [p | p <- rmdupes $ concatMap (`getNeibrs` s) b, isDead b p, liveNeibrs b s p == 3]

rmdupes :: Eq a => [a] -> [a]
rmdupes [] = []
rmdupes (x:xs) = x : rmdupes (filter (/= x) xs)

removeItem :: Eq a => a -> [a] -> [a]
removeItem x = filter (/= x)

nextGen :: Board -> Int -> Board
nextGen b s = survivors b s ++ births b s

cls :: IO()
cls = putStr "\ESC[2J" --Works just as intended from book

boardToString :: Board -> Int -> [String]
boardToString b s = addCells b (drawEmptyBoard s)

addCells :: Board -> [String] -> [String]
addCells [] xs = xs
addCells ((x,y):b) xs = let fin = take (y-1) xs ++ [take ((x*2)-2) (xs!!(y-1)) ++ "X " ++ drop (x*2) (xs!!(y-1))] ++ drop y xs in
    addCells b fin

drawEmptyBoard :: Int -> [String]
drawEmptyBoard s = [drawEmptyLine s | _ <- [1..s]]

drawEmptyLine :: Int -> String
drawEmptyLine s = concat [". " | _ <- [1..s]] ++ "\n"

{-

  01 02 03 04 05 06 07 08 09 10
01 .  .  .  .  .  .  .  .  .  .
02 .  .  .  .  .  .  .  .  .  .
03 .  .  .  .  .  .  .  .  .  .
04 .  .  .  .  .  .  .  .  .  .
05 .  .  .  .  .  .  .  .  .  .
06 .  .  .  .  .  .  .  .  .  .
07 .  .  .  .  .  .  .  .  .  .
08 .  .  .  .  .  .  .  .  .  .
09 .  .  .  .  .  .  .  .  .  .
10 .  .  .  .  .  .  .  .  .  .

-}

menu :: Board -> Int -> IO()
menu b s = do
    mapM_ putStr (boardToString b s)
    command <- getLine
    cls
    case words command of
        ("c":x:_) -> menu [] (read x)                               -- [X]
        ("n":x:y:_) -> menu (rmdupes $ (read x, read y):b) s        -- [X] Probbably innefficient
        ("d":x:y:_) -> menu (removeItem (read x, read y) b) s       -- [X]
        ("s":x:y:_) -> putStrLn "redefine?"                         -- [ ] Thefq?
        ("b":x:y:_) -> putStrLn "redefine?"                         -- [ ] Dafuq?
        ("?":_) -> do putStr helpString ; menu b s                  -- [/]
        ("w":name:_) -> putStrLn "Write to file"                    -- [ ]
        ("r":name:_) -> putStrLn "Read from file"                   -- [ ]
        ("CR":_) -> menu (nextGen b s) s                            -- [X]
        ("l":x:_) -> animate b s (read x)                           -- [X]
        ("q":_) -> exitSuccess                                      -- [X]
        _  -> menu b s                                              -- [X]

animate :: Board -> Int -> Int -> IO()
animate b s r =
    if r == 0 then menu b s else do
        cls
        mapM_ putStr (boardToString b s)
        threadDelay 128000
        animate (nextGen b s) s (r - 1)

main :: IO()
main = do cls; putStr helpString ; menu [] 0