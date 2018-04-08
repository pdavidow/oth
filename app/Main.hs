module Main where

import Player ( makePlayerBlack, makePlayerWhite )
import PlayerType ( PlayerType(..) )
import Sequencer ( moveSequence )
import State ( Tagged_State(..), makeStartState )
import Engine ( Strategy(..) )
import Lib ( getValidChoice )


main :: IO () 
main = do   
    playerType <- getWhite_PlayerType

    moveSequence 
        ( (makePlayerBlack Person)
        , (makePlayerWhite playerType) 
        )
        (Tagged_StartState makeStartState)  


getWhite_PlayerType :: IO PlayerType 
getWhite_PlayerType = do
    let options = [0..10] ++ [100]
    let prompt = "WHITE player is 0:Random, 1:SearchDepth 1, 2:SearchDepth 2, 3:SearchDepth 3, 4:SearchDepth 4, 5:SearchDepth 5, 6:SearchDepth 6, 7:SearchDepth 7, 8:SearchDepth 8, 9:SearchDepth 9, 10:SearchDepth 10, 100:Person"
    n <- getValidChoice prompt options

    return $ case n of
        0 -> Computer RandomPick
        1 -> Computer SearchDepth_1
        2 -> Computer SearchDepth_2
        3 -> Computer SearchDepth_3
        4 -> Computer SearchDepth_4
        5 -> Computer SearchDepth_5
        6 -> Computer SearchDepth_6
        7 -> Computer SearchDepth_7
        8 -> Computer SearchDepth_8
        9 -> Computer SearchDepth_9
        10 -> Computer SearchDepth_10
        100 -> Person
        _ -> Person -- should never get here