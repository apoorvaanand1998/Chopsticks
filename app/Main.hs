{-#LANGUAGE TupleSections #-}

module Main where

import Data.List ( delete, nub, sort )
import System.Environment ( getArgs )
import qualified Data.Set as SET
import qualified Data.Tree as VT
import qualified Data.Tree.Pretty as PT

type Player    = [Int] -- each int represents the number of fingers up in each hand, and we're allowed to have multiple hands
-- should be sorted
type GameState = (Player, Player) -- fst is currently playing player and snd is player who will play next

data DecisionTree a = DecisionTree a [DecisionTree a] deriving Show -- 'a' holds GameState

possibleGameStates :: GameState -> [GameState]
possibleGameStates g = 
    let
        attacks = allAttacks2 g
        dnts    = allDnTs2 g
    in
        attacks ++ dnts

mod5nSortAll :: GameState -> GameState
mod5nSortAll (cp, np) = (sort $ map (`mod` 5) cp, sort $ map (`mod` 5) np)

all0 :: Player -> Bool
all0 = all (== 0)

allAttacks :: GameState -> [GameState]
allAttacks (cp, np) = [ (hand1 + hand2 : delete hand2 np , cp) | hand1 <- cp, hand2 <- np, hand2 > 0]

allTransfers :: GameState -> [GameState]
allTransfers (cp, np) = 
    let
        allPossible = dntHelper cp
    in
        map (np,) allPossible

dntHelper :: Player -> [Player]
dntHelper []     = []
dntHelper hands = [(h-taken) : (h1+taken) : delete h (delete h1 hands) | 
                   h <- hands, 
                   h1 <- hands, 
                   h /= h1 || elem h1 (delete h hands), 
                   h > 0, 
                   h1 > 0,
                   taken <- [1..h]]

createDecisionTree :: SET.Set GameState -> GameState -> DecisionTree GameState
createDecisionTree s gs@(cp,np) 
    | all0 cp || all0 np || SET.member gs s = DecisionTree gs []
    | otherwise                             = DecisionTree gs (map (createDecisionTree (SET.insert gs s))  (possibleGameStates gs))
-- keeps on running
-- to investigate, convert DecisionTree to Vanilla Rose Tree from the Data.Tree package 
-- this is so we can use prettyPrint functions to see wtf is going on


theTree :: DecisionTree GameState
theTree = createDecisionTree SET.empty ([1,1], [1,1])

theActualTree :: Int -> DecisionTree GameState -> VT.Tree String
theActualTree _ (DecisionTree x []) = VT.Node (show x) []
theActualTree 0 (DecisionTree x xs) = VT.Node (show x) []
theActualTree h (DecisionTree x xs) = VT.Node (show x) (map (theActualTree (h-1)) xs)

forPrinting = PT.drawVerticalTree $ theActualTree 9 theTree -- even with this it still continues printing, therefore the error is related to not depth, but one 
                                       -- of the levels of the DecisionTree having a list that keeps on mutating producing a tree of infinite breadth

-- okay, the code for what is happening in the list comprehensions is complicated
-- let's commit a cardinal sin and create our own l[x] = y operation

changeAt :: [a] -> Int -> a -> [a]
changeAt l i to = take i l ++ (to : drop (i+1) l) -- ew, ik

-- now let's use changeAt to create new attacks and dnt functions

allAttacks2 :: GameState -> [GameState]
allAttacks2 g@(cp, np) =
    let
        nHands  = length np - 1
        whichs  = [0..nHands]

        aGameStateChange :: GameState -> Int -> Int -> GameState
        aGameStateChange (cp, np) which byWhat = if np !! which /= 0 then (changeAt np which (byWhat + (np !! which)), cp) else (np, cp)

        erry    = [(which, byWhat) | which <- whichs, byWhat <- cp]
    in
        nub $ map mod5nSortAll $ nub $ map (\(x, y) -> aGameStateChange g x y) erry

allDnTs2 :: GameState -> [GameState]
allDnTs2 g@(cp, np) = 
    let
        nHands = length cp - 1
        whichs = [0..nHands]
        
        byWhats :: Int -> [Int]
        byWhats finger = [1..finger]

        byWhatsAll = concatMap byWhats cp

        aGameStateChange :: GameState -> Int -> Int -> Int -> GameState
        aGameStateChange (cp, np) which byWhat from = 
            let
                firstChange  = changeAt cp which (byWhat + (cp !! which))
                secondChange = changeAt firstChange from ((cp !! from) - byWhat)
            in
                if ((cp !! which /= 0) && (cp !! from /= 0)) then (np, secondChange) else (np, cp)

        erry   = [(which, byWhat, from) | which <- whichs, from <- whichs, from /= which, byWhat <- byWhats (cp !! which)]
    in
        nub $ map mod5nSortAll $ nub $ map (\(x, y, z) -> aGameStateChange g x y z) erry

-- FML, so it wasn't the attacks and the divisions and transfers functions
-- the infinite looping still exists        

-- nvm it's not infinite looping, the state space is just huge

-- lets count what the number of reachable states are
-- at every level

countGameStateLevel :: [GameState] -> Int -> (Int, [GameState], Int)
countGameStateLevel gs times = do
    -- runs allPossibleMoves on given list of gamestates and returns the count of them
    let allPoss = concatMap possibleGameStates gs in (length allPoss, allPoss, times+1)

countGameState :: Int -> [GameState] -> Int -> Int -> IO Int
countGameState t0 gs uptoLevel totGameStates = do
    let (nGameStates, newGS, t) = countGameStateLevel gs t0
    if t >= uptoLevel 
        then return totGameStates
        else countGameState t newGS uptoLevel (totGameStates+nGameStates)

main :: IO ()
main = do
    let initGS = [([1,1],[1,1])]
    putStrLn "Enter the number of levels you want explored in the Decision Tree"
    r <- getLine
    let ri = read r :: Int
    res <- countGameState 0 initGS ri 0
    print res