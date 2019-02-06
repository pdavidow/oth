module Main where

import Player ( makePlayerBlack, makePlayerWhite )
import PlayerType ( PlayerType(..) )
import Sequencer ( moveSequence )
import State ( makeHistory ) 
import Engine ( Strategy(..), SearchDepth(..), SuggestionSearchDepth(..) )
import Color ( Color(..) )
import Lib ( getValidChoice )


main :: IO () 
main = do   
    blackPlayerType <- getBlack_PlayerType
    whitePlayerType <- getWhite_PlayerType

    moveSequence  
        ( makePlayerBlack blackPlayerType
        , makePlayerWhite whitePlayerType
        )
        makeHistory


getPerson :: Color -> IO PlayerType 
getPerson color = do
    let options = [1..10]
    let prompt = "Enter " ++ show color ++ " person-player suggestion-search-depth as 1 through 10"
    n <- getValidChoice prompt options

    return $ Person $ SuggestionSearchDepth $ case n of
        1 -> SearchDepth_1
        2 -> SearchDepth_2
        3 -> SearchDepth_3
        4 -> SearchDepth_4
        5 -> SearchDepth_5
        6 -> SearchDepth_6
        7 -> SearchDepth_7
        8 -> SearchDepth_8
        9 -> SearchDepth_9
        10 -> SearchDepth_10
        _ -> SearchDepth_1 -- should never get here


getBlack_PlayerType :: IO PlayerType 
getBlack_PlayerType = 
    getPerson Black


getWhite_PlayerType :: IO PlayerType 
getWhite_PlayerType = do
    let options = [0] ++ [1..10] ++ [100]
    let prompt = "Enter WHITE player as 0:Computer-Random, (1 through 10):Computer-SearchDepth, 100:Person"
    n <- getValidChoice prompt options

    case n of
        0  -> return $ Computer $ RandomPick
        1  -> return $ Computer $ SearchDepth SearchDepth_1
        2  -> return $ Computer $ SearchDepth SearchDepth_2
        3  -> return $ Computer $ SearchDepth SearchDepth_3
        4  -> return $ Computer $ SearchDepth SearchDepth_4
        5  -> return $ Computer $ SearchDepth SearchDepth_5
        6  -> return $ Computer $ SearchDepth SearchDepth_6
        7  -> return $ Computer $ SearchDepth SearchDepth_7
        8  -> return $ Computer $ SearchDepth SearchDepth_8
        9  -> return $ Computer $ SearchDepth SearchDepth_9
        10 -> return $ Computer $ SearchDepth SearchDepth_10
        100 -> getPerson White
        _ ->  return $ Computer $ RandomPick -- should never get here