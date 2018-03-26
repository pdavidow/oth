module Engine
    ( Strategy(..)
    , computerChoose
    )
    where

import System.Random  
import Data.Function ( (&) )
import Data.List ( sortOn )

import GameState ( PlayGameState(..), All_State(..), possibleMoves )
import Board ( Move(..), FilledRow(..) )

data Strategy 
    -- take corners, avoid corner neighbors, otherwise maxflips
    = RandomStrategy
    | MaxFlipsStrategy
        deriving (Eq, Show)


computerChoose :: Strategy -> PlayGameState -> IO Int
computerChoose strat playGameState = do
    let moves = possibleMoves $ PlayState playGameState

    case strat of
        RandomStrategy -> do
            gen <- getStdGen  
            let (randN, _) = randomR (0, length moves - 1) gen :: (Int, StdGen)
            return randN

        MaxFlipsStrategy -> do
            return $ maxFlips moves   


maxFlips :: [Move] -> Int            
maxFlips moves = 
    let
        flipCount :: [FilledRow] -> Int
        flipCount = \ filledRows -> sum $ map (\ (FilledRow xs) -> length xs) filledRows
    in
        (zip [(0 :: Int)..] moves)
            & map (\ ((i, move)) -> (i, flipCount $ _outflanks move))
            & sortOn (\ ((_, x)) -> x)
            & last
            & fst