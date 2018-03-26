module Sequencer
    ( moveSequence
    ) 
    where

import Player ( PlayerBlack, PlayerWhite, All_Player(..), playerTypeFrom, playerColor )
import PlayerType ( PlayerType(..) )
import GameState ( PlayGameState(..), All_State(..), applyMove, nextToMove, possibleMoves, gameStateDisplay, gameSummary, winner, boardWithFlipCountDisplay )
import Board ( Move, movePosChoices )
import Disk ( Color(..) )
import Position ( Position )
import Display ( movePosChoicesNomenclature )
import Engine ( computerChoose )
import Lib ( getValidChoice )

 
moveSequence :: (PlayerBlack, PlayerWhite) -> PlayGameState -> IO ()
moveSequence ps playGameState = do       
    let taggedState = PlayState playGameState 
    let taggedPlayer = playerNextToMove ps playGameState
    putStrLn ""
    putStrLn $ gameStateDisplay Nothing taggedState

    move <- case playerTypeFrom taggedPlayer of
        Person -> do
            moveIndex <- personChoose (playerColor taggedPlayer) playGameState
            return $ (possibleMoves taggedState) !! moveIndex
        Computer strategy -> do 
            computerChoose strategy playGameState

    advance ps move playGameState


advance :: (PlayerBlack, PlayerWhite) -> Move -> PlayGameState -> IO ()
advance ps move playGameState = do    
    let taggedState = applyMove move playGameState

    case taggedState of 
        PlayState x ->
            moveSequence ps x

        EndState x -> do
            putStrLn $ gameStateDisplay Nothing taggedState
            putStrLn ""
            putStrLn $ "GAME OVER! " ++ show (winner $ gameSummary x)
            putStrLn $ show $ gameSummary x -- todo prettify
            putStrLn ""
            putStrLn "########################################"
            putStrLn "FYI, here are the flip-counts:\n"
            putStrLn $ boardWithFlipCountDisplay taggedState
            putStrLn "########################################"
            return ()


playerNextToMove :: (PlayerBlack, PlayerWhite) -> PlayGameState -> All_Player
playerNextToMove (pb, pw) playGameState =
    case nextToMove $ PlayState playGameState of
        Black -> BlackPlayerTag pb
        White -> WhitePlayerTag pw


personChoose :: Color -> PlayGameState -> IO Int
personChoose color playGameState = do
    let moves = possibleMoves $ PlayState playGameState
    let numberedMovesWithPos = movePosChoices moves
    n <- handlePersonChoose color numberedMovesWithPos moves playGameState
    return n


handlePersonChoose :: Color -> [(Int, Position)] -> [Move] -> PlayGameState -> IO Int
handlePersonChoose color numberedMovesWithPos moves playGameState  = do
    n <- getMoveChoice color numberedMovesWithPos

    if n == choiceNumberFor_DisplayChoicesOnBoard then do
        putStrLn ""
        putStrLn $ gameStateDisplay (Just numberedMovesWithPos) $ PlayState playGameState
        handlePersonChoose color numberedMovesWithPos moves playGameState 
    else do 
        return $ n - 1 -- index is zero-based


getMoveChoice :: Color -> [(Int, Position)] -> IO Int
getMoveChoice color numberedMovesWithPos = do
    let posTags = Prelude.map fst numberedMovesWithPos
    let options = choiceNumberFor_DisplayChoicesOnBoard : posTags
    let nomenclature = movePosChoicesNomenclature numberedMovesWithPos
    let prompt = (colorString color) ++ " Moves: (" ++ show choiceNumberFor_DisplayChoicesOnBoard ++ ":show) " ++ nomenclature ++ "\nEnter choice"
    getValidChoice prompt options


colorString :: Color -> String
colorString c =
    case c of
        Black -> "BLACK"
        White -> "WHITE"


choiceNumberFor_DisplayChoicesOnBoard :: Int
choiceNumberFor_DisplayChoicesOnBoard = 
    99