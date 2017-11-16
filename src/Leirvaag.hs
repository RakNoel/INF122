{-
    Oskar Leonhard Fretheim Leirvåg
    Spaceship:      s 2 3 b 3 3 30 1 1 4 1 5 2 5 3 5 4 4 4 3 4 2 4 1 3
    Reach stable:   s 2 3 b 3 3 30 12 16 18 16 13 16 14 16 16 16 17 16 15 17 15 15
    Pentadecathlon: s 2 3 b 3 3 30 11 15 12 15 13 16 13 14 19 15 14 15 16 15 15 15 17 15 20 15 18 14 18 16
-}

module Main where

import Control.Concurrent
import System.Exit

type Pos = (Int, Int)
type Board = [Pos]

helpString :: [Int] -> [Int] -> String
helpString rs rb = lineSplitter
        ++ "This program will emulate Conway's Game-Of-Life in a grid of size N \n\n"
        ++ "c n : \t\t create (and show) a new, empty board n × n \n"
        ++ "n x y : \t place a new living cell at (x,y) \n"
        ++ "d x y : \t make position (x,y) empty \n"
        ++ "s x y : \t redefine the automaton so that a living cell with [x..y] living neighbours survives \n"
        ++ "b x y : \t redefine the automaton so that an empty cell with [x..y] living neighbours becomes alive \n"
        ++ "? : \t\t show commands and the config for the current game \n"
        ++ "w name : \t write the current rules and board to the file name \n"
        ++ "r name : \t read the rules and board from the file name \n"
        ++ "CR : \t\t step one generation forward. \n"
        ++ "l x : \t\t animates x steps forward \n"
        ++ "cfg xs : \t\t reads paste as from file \n"
        ++ "q : \t\t quits the program."
        ++ lineSplitter
        ++ configToStr rs rb

configToStr :: [Int] -> [Int] -> String
configToStr rs rb = "Config: \n"
    ++ "Automation is set so that a living cell with [x..y] living neighbours survives: \t"
    ++ "[" ++ show xs ++ ".." ++ show ys ++ "] \n"
    ++ "Automation is set so that an empty cell with [x..y] living neighbours becomes alive: \t"
    ++ "[" ++ show xb ++ ".." ++ show yb ++ "]"
    ++ lineSplitter
    where
        xs = head rs ; ys = last rs
        xb = head rb ; yb = last rb

lineSplitter :: String
lineSplitter = "\n" ++ "==================================================" ++ "\n\n"

mergeSort:: (Ord a) => [a] -> [a]
mergeSort [] = []
mergeSort [x] = [x]
mergeSort xs = mergeSort (take half xs) `merge` mergeSort (drop half xs)
    where
        half = length xs `div` 2

merge:: (Ord a) => [a] -> [a] -> [a]
merge [] xs = xs
merge xs [] = xs
merge (x:xs) (y:ys)
    | x > y = y : ( (x:xs) `merge` ys )
    | x < y = x : ( xs `merge` (y:ys) )
    | otherwise = [x,y] ++ ( xs `merge` ys )

isAlive :: Board -> Pos -> Bool
isAlive b p = p `elem` b

isDead :: Board -> Pos -> Bool
isDead b p = not $ isAlive b p

liveNeibrs :: Board -> Int -> Pos -> Int
liveNeibrs b s p = length $ filter (isAlive b) $ getNeibrs p s

getNeibrs :: Pos -> Int -> [Pos]
getNeibrs (x,y) s = filter (\(a,b) -> a <= s && a > 0 && b <= s && b > 0)
    [(x-1,y),(x+1,y),(x,y+1),(x,y-1),(x+1,y+1),(x-1,y-1),(x-1,y+1),(x+1,y-1)]

--Unused, but is used to wrap board if set
wrap :: Pos -> Int -> Pos
wrap (x,y) s = (((x-1) `mod` s) + 1, ((y-1) `mod` s) + 1)

survivors :: Board -> Int -> [Int] -> [Pos]
survivors b s rs = [p | p <- b, liveNeibrs b s p `elem` rs] -- where [2,3] is accepted alive neibrs for gamerules

births :: Board -> Int -> [Int] -> [Pos]
births b s rb = [p | p <- rmdupes $ concatMap (`getNeibrs` s) b, isDead b p, liveNeibrs b s p `elem` rb]

rmdupes :: Eq a => [a] -> [a]
rmdupes [] = []
rmdupes (x:xs) = x : rmdupes (filter (/= x) xs)

removeItem :: Eq a => a -> [a] -> [a]
removeItem x = filter (/= x)

nextGen :: Board -> Int -> [Int] -> [Int] -> Board
nextGen b s rs rb = merge (survivors b s rs) (mergeSort $ births b s rb)

cls :: IO()
cls = putStr "\ESC[2J" --Works just as intended from book

boardToString :: Board -> Int -> [String]
boardToString b s = "Board:\n" : addNumbers s (addPos b (drawEmptyBoard (s + 1)) " X ")

addNumbers :: Int -> [String] -> [String]
addNumbers 0 xs = addPos [(0,0)] xs "   "
addNumbers s xs = addNumbers (s-1) $ addPos [(0,s), (s,0)] xs ((if s < 10 then '0':show s else show s) ++ " ")

addPos :: Board -> [String] -> String -> [String]
addPos [] xs _ = xs
addPos ((x',y'):b) xs s = addPos b fin s
    where
        x = x' + 1 ; y = y' + 1 --Were using x=0 and y=0 to show line/column numbers
        fin = start ++ mainline ++ end
        start = take (y-1) xs
        end = drop y xs
        workingLine = xs!!(y-1)
        tokenLength = length s
        mainline = [take ((x*tokenLength)-tokenLength) workingLine  ++ s ++ drop (x*tokenLength) workingLine]

drawEmptyBoard :: Int -> [String]
drawEmptyBoard s = [drawEmptyLine s | _ <- [1..s]]

drawEmptyLine :: Int -> String
drawEmptyLine s = concat [" . " | _ <- [1..s]] ++ "\n"

writeToFile :: String -> Board -> Int -> [Int] -> [Int] -> IO()
writeToFile f b s rs rb = writeFile f $ str ++ unwords (foldl (\x (a,o) -> show a:show o:x) [] b)
    where
        xs = head rs ; ys = last rs
        xb = head rb ; yb = last rb
        str =   "s " ++ show xs ++ " " ++ show ys ++
                " b " ++ show xb ++ " " ++ show yb ++
                " " ++ show s ++ " "


readFromFile :: String -> IO()
readFromFile s = do
    str <- readFile s
    case words str of
        ("s":x:y:"b":a:b:n:xs) -> menu (listToBoard $ map read xs) (read n) [read x..read y] [read a.. read b]
        ("b":a:b:"s":x:y:n:xs) -> menu (listToBoard $ map read xs) (read n) [read x..read y] [read a.. read b]
        _ -> do putStrLn "Format error in file" ; menu [] 10 [2,3] [3]

listToBoard :: [Int] -> Board
listToBoard [] = []
listToBoard (x:y:xs) = (x,y) : listToBoard xs
listToBoard _ = []


menu :: Board -> Int -> [Int] -> [Int] -> IO()
menu b s rs rb = do
    mapM_ putStr (boardToString b s)
    command <- getLine
    cls
    case words command of
        ("c":x:_)    -> menu [] (read x) rs rb
        ("n":x:y:_)  -> menu (rmdupes $ (read x, read y):b) s rs rb
        ("d":x:y:_)  -> menu (removeItem (read x, read y) b) s rs rb
        ("s":x:y:_)  -> let r' = [read x .. read y] in do putStrLn $ configToStr r' rb ; menu b s r' rb
        ("b":x:y:_)  -> let r' = [read x .. read y] in do putStrLn $ configToStr r' rb ; menu b s rs r'
        ("?":_)      -> do putStr $ helpString rs rb ; menu b s rs rb
        ("w":name:_) -> do writeToFile name b s rs rb ; menu b s rs rb
        ("r":name:_) -> readFromFile name
        ("CR":_)     -> do if b' == b && not (null b) then putStrLn "STABLE CONFIG" else putStrLn "" ; menu b' s rs rb
        ("l":x:_)    -> animate b s (read x) rs rb
        ("cfg":"s":x:y:"b":a:z:n:xs) -> menu (listToBoard $ map read xs) (read n) [read x..read y] [read a.. read z]
        ("q":_)      -> exitSuccess
        _            -> do putStrLn "Unknow command or missing parameters, use ? for help"; menu b s rs rb
    where b' = nextGen b s rs rb

animate :: Board -> Int -> Int -> [Int] -> [Int] -> IO()
animate b s r rs rb =
    if r == 0 || b' == b then do cls ; if b' == b then putStrLn "STABLE CONFIG" else putStrLn ""; menu b s rs rb
    else do
        mapM_ putStr (boardToString b s)
        threadDelay 256000
        cls
        animate (nextGen b s rs rb) s (r - 1) rs rb
    where b' = nextGen b s rs rb

main :: IO()
main = do cls ; putStr $ helpString [2,3] [3] ; menu [] 10 [2,3] [3]