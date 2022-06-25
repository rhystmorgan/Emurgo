import Data.Char 
import Control.Monad 

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

-- Name Based Croc Strategy

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

beatsMove :: Int -> Move
beatsMove x
    | x == 0 = Paper
    | x == 1 = Scissors 
    | x == 2 = Rock 

beatsStrat :: [Int] -> [Move]
beatsStrat []     = []
beatsStrat xs     = map beatsMove (cnameStrat xs)

-- Losing Move
-- Reverse Beats Move

loseMove :: Int -> Move 
loseMove x
    | x == 0 = Scissors
    | x == 1 = Rock 
    | x == 2 = Paper

loseStrat :: [Int] -> [Move]
loseStrat []     = []
loseStrat xs     = map loseMove (cnameStrat xs)

inc :: Int -> Int 
inc x = x + 1

dec :: Int -> Int
dec x = x - 1

getStrategy :: Name -> [Move]
getStrategy p = nameMove (cnameStrat $ nameNum p) ++ beatsStrat (cnameStrat $ nameNum p) ++ loseStrat (cnameStrat $ nameNum p)

strategy :: [Move] -> Int -> Move
strategy s c = s !! c

scoreboard :: Name -> (Int, Int) -> String
scoreboard p (w, l) = p ++ " " ++ show w ++ " vs " ++ show l ++ " Croc"

--

main = 
    putStrLn "Let's play Rock Paper Scissors!" >>
    playerDetails 

playerDetails :: IO ()
playerDetails = do
    putStrLn "What is your name?"
    p <- getLine 
    putStrLn $ "Hey " ++ p ++ " Ready to play?"
    putStrLn "Best of 3, let's go!"
    play 3 p 0 0 (getStrategy p) 0 0

play :: Plays -> Name -> Round -> Count -> [Move] -> Int -> Int -> IO ()
play n p r c s w l = 
    if n > 0 then do
        putStrLn "Pick Rock, Paper or Scissors"
        myMove <- readLn
        putStrLn $ show myMove ++ " vs " ++ show (strategy s c)
        case myMove `check` strategy s c of
            Win  -> putStrLn "You Win!" >> do
                putStrLn (scoreboard p (inc w, l)) >> do
                    play (dec n) p (inc r) (inc c) s (inc w) l
            Lose -> putStrLn "You Lose!" >> do
                putStrLn (scoreboard p (w, inc l)) >> do
                    play (dec n) p (inc r) (inc c) s w (inc l)
            Draw -> putStrLn "It's a Draw!" >> do
                putStrLn (scoreboard p (w, l)) >> do    
                    play n p r (inc c) s w l
    else do
        putStrLn "The Winner Is..."
        if w > l then putStrLn p 
        else putStrLn "Croc!"
