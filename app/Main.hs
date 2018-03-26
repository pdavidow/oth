module Main where

import Player ( PlayerWhite, makePlayerBlack, makePlayerWhite )
import PlayerType ( PlayerType(..) )
import Sequencer ( moveSequence )
import GameState ( makePlayGameState )
import Engine ( Strategy(..) )
import Lib ( getValidChoice )


main :: IO () 
main = do   
    playerType <- getWhite_PlayerType

    moveSequence 
        ( (makePlayerBlack Person)
        , (makePlayerWhite playerType) 
        )
        makePlayGameState  


getWhite_PlayerType :: IO PlayerType
getWhite_PlayerType = do
    let options = [0, 1, 2]
    let prompt = "WHITE player is 0:Person, 1:Computer-Random, 2:Computer-Max-Flips"
    n <- getValidChoice prompt options

    return $ case n of
        0 -> Person
        1 -> Computer RandomStrategy
        2 -> Computer MaxFlipsStrategy
        _ -> Person