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
    let options = [0, 1, 2]
    -- todo
    -- let prompt = "WHITE player is 0:Person, 1:Computer-Random, 2:Computer-SearchDepth-0, 3:Computer-SearchDepth-1, 4:Computer-SearchDepth-2, 5:Computer-SearchDepth-3, 6:Computer-SearchDepth-4"
    let prompt = "WHITE player is 0:Person, 1:Computer-Random"
    n <- getValidChoice prompt options

    return $ case n of
        0 -> Person
        1 -> Computer RandomPick
        2 -> Computer SearchDepth_0
        3 -> Computer SearchDepth_1
        4 -> Computer SearchDepth_2
        5 -> Computer SearchDepth_3
        6 -> Computer SearchDepth_4
        _ -> Person -- should never get here