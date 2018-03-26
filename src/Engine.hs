module Engine
    ( Strategy(..)
    , computerChoose
    )
    where

import System.Random  
import Data.List ( sortOn )

import GameState ( PlayGameState(..), All_State(..), possibleMoves )
import Board ( Move(..), FilledRow(..), movePos )
import WeightingFactor ( WeightingFactor, highestWeight, mediumWeight, lowestWeight, isHighestWeight, isMediumWeight, isLowestWeight )
import Position ( isCornerPos, isCornerNeighborPos )

data Strategy 
    = RandomStrategy
    | SearchDepth_0
    -- | SearchDepth_1
    -- | SearchDepth_2
    -- ... todo
        deriving (Eq, Show)


computerChoose :: Strategy -> PlayGameState -> IO Move 
computerChoose strat playGameState = do
    let moves = possibleMoves $ PlayState playGameState 

    case strat of
        RandomStrategy -> do
            gen <- getStdGen  
            let (randN, _) = randomR (0, length moves - 1) gen :: (Int, StdGen)
            return $ moves !! randN

        SearchDepth_0 -> do
            return $ bestMove moves   


bestMove :: [Move] -> Move
bestMove moves = 
    let
        highest = maxFlipsSorted $ filter (\ x -> isHighestWeight $ weightingFactor x) moves
        medium  = maxFlipsSorted $ filter (\ x -> isMediumWeight  $ weightingFactor x) moves
        lowest  = maxFlipsSorted $ filter (\ x -> isLowestWeight  $ weightingFactor x) moves

        sortedCandidates = lowest ++ medium ++ highest
    in
        last sortedCandidates


maxFlipsSorted :: [Move] -> [Move]            
maxFlipsSorted moves = 
    let
        flipCount :: [FilledRow] -> Int
        flipCount = \ filledRows -> sum $ map (\ (FilledRow xs) -> length xs) filledRows
    in
        sortOn (\ move -> flipCount $ _outflanks move) moves


weightingFactor :: Move -> WeightingFactor
weightingFactor move
    | isCornerPos         $ movePos move = highestWeight
    | isCornerNeighborPos $ movePos move = lowestWeight
    | otherwise                          = mediumWeight