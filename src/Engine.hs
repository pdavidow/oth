module Engine
    ( Strategy(..)
    , computerChoose
    )
    where

import System.Random  
import Data.Function ( (&) )
import Data.List ( sortOn )

import GameState ( PlayGameState(..), All_State(..), possibleMoves )
import Board ( Move(..), FilledRow(..), movePos )
import WeightingFactor ( WeightingFactor, highestWeight, mediumWeight, lowestWeight, isHighestWeight, isMediumWeight, isLowestWeight )
import Position ( isCornerPos, isCornerNeighborPos )

data Strategy 
    = RandomStrategy
    | SearchDepth_0
        deriving (Eq, Show)


computerChoose :: Strategy -> PlayGameState -> IO Move -- todo return Maybe ?
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
    -- take corners, consider max-flips; avoid corner neighbors
    let
        -- todo wasteful?
        highest = maxFlipsSorted $ filter (\ x -> isHighestWeight $ weightingFactor x) moves
        medium  = maxFlipsSorted $ filter (\ x -> isMediumWeight  $ weightingFactor x) moves
        lowest  = maxFlipsSorted $ filter (\ x -> isLowestWeight  $ weightingFactor x) moves

        sortedCandidates = lowest ++ medium ++ highest
    in
        last sortedCandidates


-- todo unused?
-- maxFlips :: [Move] -> Int            
-- maxFlips moves = 
--     let
--         flipCount :: [FilledRow] -> Int
--         flipCount = \ filledRows -> sum $ map (\ (FilledRow xs) -> length xs) filledRows
--     in
--         (zip [(0 :: Int)..] moves)
--             & map (\ ((i, move)) -> (i, flipCount $ _outflanks move))
--             & sortOn (\ ((_, x)) -> x)
--             & last
--             & fst


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