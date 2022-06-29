import Data.Char
import Control.Monad
import System.Console.ANSI
import System.Random
import Control.Concurrent (threadDelay)
import System.IO (hFlush, stdout)
import Text.Printf(printf)

type Name = String
type Round = Int
type Plays = Int
type Count = Int

data Move = Rock | Paper | Scissors
    deriving (Show, Eq, Read)

data Result = Win | Lose | Draw
    deriving (Show, Eq)

check :: Move -> Move -> Result
check Rock Paper        = Lose
check Rock Scissors     = Win
check Paper Rock        = Win
check Paper Scissors    = Lose
check Scissors Rock     = Lose
check Scissors Paper    = Win
check _ _               = Draw

-- Name Based Strategy

nameLength :: String -> Int
nameLength x = length x `mod` 3

nameNum :: String -> [Int]
nameNum = map (subtract 64 . ord . toUpper)

cnameStrat :: [Int] -> [Int]
cnameStrat []        = []
cnameStrat xs  = map (`mod` 3) xs

numMove :: Int -> Move
numMove x
    | x == 0 = Rock
    | x == 1 = Paper
    | x == 2 = Scissors

nameMove :: [Int] -> [Move]
nameMove []     = []
nameMove xs     = map numMove (cnameStrat xs)

-- Beats Strategy
-- Take name strategy and play the moves that beat it

beatMove :: Move -> Move
beatMove x
    | x == Rock = Paper
    | x == Paper = Scissors
    | x == Scissors = Rock

-- Losing Move
-- Reverse Beats Move

loseMove :: Move -> Move
loseMove x
    | x == Rock = Scissors
    | x == Paper = Rock
    | x == Scissors = Paper

-- Random Move
-- random Move calculator based on name and round variables

randomMove :: Int -> Move
randomMove x = numMove $ randomList $ randomVal x

randomVal :: Int -> Int
randomVal x = fst $ random (mkStdGen x)

randomList :: Int -> Int
randomList x = randomVal x `mod` 3

-- Game Counter Helper Functions

inc :: Int -> Int
inc x = x + 1

dec :: Int -> Int
dec x = x - 1

-- Init Strategy

getStrategy :: Int -> Name -> [Move]
getStrategy n p = nameMove (cnameStrat $ nameNum p) ++ getStrategy (n - 1) p

-- Strategy Adjustment Method

changeStrategy :: Int -> Move -> Int -> Move
changeStrategy x y z
    | x == 0 = y
    | x == 1 = loseMove y
    | x == 2 = beatMove y
    | x == 3 = randomMove ((length (show y)) * z)

-- Strategy Config Logic

strategy :: [Move] -> Int -> Int -> Int -> Move
strategy s c w l 
  | (w + l) < c =
    if w > l 
    then 
        changeStrategy 3 ( s !! c) c -- Random
    else if w == l then 
        changeStrategy 1 ( s !! c) c -- Lose
        else 
            changeStrategy 2 ( s !! c) c -- Beats
  | w > l = 
    changeStrategy 3 ( s !! c) c -- Random
  | w < l =
    changeStrategy 0 ( s !! c) c -- Name
  | otherwise = changeStrategy 0 ( s !! c) c -- Name

-- User Input Logic

myMove :: String -> Move
myMove m
    | m == "r" = Rock
    | m == "p" = Paper
    | m == "s" = Scissors

-- UI Navigation For Printing and Removing Instructions

pause :: IO ()
pause = do
    hFlush stdout
    threadDelay 1000000 -- 1 second pause

clearText3 :: IO ()
clearText3 = do
    clearLine
    cursorUp 1
    clearLine
    cursorUp 1
    clearLine
    cursorUp 1
    clearLine

clearText2 :: IO ()
clearText2 = do
    clearLine
    cursorUp 1
    clearLine
    cursorUp 1
    clearLine

clearText1 :: IO ()
clearText1 = do
    clearLine
    cursorUp 1
    clearLine

-- UI Print Game Config

setName :: Name -> IO ()
setName p = do
    setCursorPosition 0 (35 - length p)
    putStrLn p
    setCursorPosition 3 (54 - length p)
    putStrLn p
    setCursorPosition 9 0

setRounds :: Int -> IO ()
setRounds n = do
    setCursorPosition 3 7
    print n
    setCursorPosition 6 0

setGame :: Int -> IO ()
setGame n = do
    setCursorPosition 2 14
    print n
    setCursorPosition 9 0   

scoreboard :: Name -> (Int, Int) -> IO ()
scoreboard p (w, l) = do
    putStrLn $ p ++ " " ++ show w ++ " vs " ++ show l ++ " Croc"
    setCursorPosition 3 58
    print w
    setCursorPosition 4 58
    print l
    setCursorPosition 9 0

-- Main Game

main :: IO ()
main =
    ui >>
    setCursorPosition 6 0 >>
    putStrLn "Let's play Rock Paper Scissors!" >>
    configGame

configGame :: IO ()
configGame = do
    putStrLn "What is your name?"
    p <- getLine
    setName p
    clearText3
    putStrLn $ "Hey " ++ p ++ " Ready to play?"
    pause
    clearText1
    putStrLn "Best of ... ?"
    n <- readLn :: IO Int
    putStrLn $ "Best of " ++ show n ++ ", let's go!"
    setGame n
    pause
    clearText3
    play n p 0 0 (getStrategy ((2 * n) ^ 2) p) 0 0

play :: Plays -> Name -> Round -> Count -> [Move] -> Int -> Int ->  IO ()
play n p r c s w l =
    if n > 0 then do
        setRounds n 
        putStrLn "Pick Rock, Paper or Scissors" 
        m <- getLine
        putStrLn $ show (myMove m) ++ " vs " ++ show (strategy s c w l)
        case myMove m `check` strategy s c w l of
            Win  -> clearText3 >> do
                putStrLn "You Win!" >> do
                        scoreboard p (inc w, l)
                        pause
                        clearText3 >> do
                            play (dec n) p (inc r) (inc c) s (inc w) l 
            Lose -> clearText3 >> do
                putStrLn "You Lose!" >> do
                    scoreboard p (w, inc l)
                    pause
                    clearText3 >> do
                        play (dec n) p (inc r) (inc c) s w (inc l) 
            Draw -> clearText3 >> do
                putStrLn "It's a Draw!" >> do
                    scoreboard p (w, l)
                    pause
                    clearText3 >> do
                        play n p r (inc c) s w l 
    else do
        setRounds n
        putStrLn "The Winner Is..."
        if w > l then putStrLn p
        else putStrLn "Croc!"
        endGame

-- UI 

ui :: IO ()
ui = do
    clearScreen
    setCursorPosition 0 0
    putStrLn "| Rock Paper Scissors |             vs  Croc  | V. 0.2.0.0 |" -- 0
    cursorDownLine 1
    putStrLn "Game: Best Of x                                  Scoreboard    " -- 2
    putStrLn "Round: x                                                : 0    " -- 3
    putStrLn "                                                  Croc  : 0    " -- 4
    cursorDownLine 5
    -- 6
    -- 7
    -- 8
    -- 9
    putStrLn "Controls:   | Rock      | Paper     | Scissors  |              " -- 10
    putStrLn "            | [R]       | [P]       | [S]       |              " -- 11

-- End Game Set Cursor Below UI

endGame :: IO ()
endGame = do
    setCursorPosition 12 0


