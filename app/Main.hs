module Main where

import Text.Read

import GameState ( GameState(..), PlayGameState(..), EndGameState(..), All_State(..), applyMove, makePlayGameState, nextToMove, possibleMoves, gameStateDisplay, gameSummary, winner )
import Board ( Move, movePosChoices, movePosChoicesNomenclature )
import Disk ( Color(..) )
import Position ( Position )
 

main :: IO ()
main = do   
    play makePlayGameState 
 

play :: PlayGameState -> IO ()
play playGameState = do
    let taggedState = PlayState playGameState
    let playerColor = nextToMove taggedState
    let moves = possibleMoves taggedState
    let numberedMovesWithPos = movePosChoices moves
      
    putStrLn $ gameStateDisplay Nothing $ PlayState playGameState
    handleChoice playerColor numberedMovesWithPos moves playGameState 


handleChoice :: Color -> [(Int, Position)] -> [Move] -> PlayGameState -> IO ()
handleChoice playerColor numberedMovesWithPos moves playGameState  = do
    n <- getMoveChoice playerColor numberedMovesWithPos

    if n == choiceNumberFor_Resign then do
        return ()
    else if n == choiceNumberFor_DisplayChoicesOnBoard then do
        putStrLn $ gameStateDisplay (Just numberedMovesWithPos) $ PlayState playGameState
        handleChoice playerColor numberedMovesWithPos moves playGameState 
    else do
        let taggedState = applyMove (moves !! (n-1)) playGameState

        case taggedState of 
            PlayState x -> 
                play x

            EndState x -> do
                putStrLn $ gameStateDisplay Nothing taggedState
                putStrLn ""
                putStrLn $ "GAME OVER! " ++ show (winner $ gameSummary x)
                putStrLn $ show $ gameSummary x
                putStrLn ""
                return ()


getMoveChoice :: Color -> [(Int, Position)] -> IO Int
getMoveChoice playerColor numberedMovesWithPos = do
    let posTags = map fst numberedMovesWithPos
    let validChoices = choiceNumberFor_Resign : choiceNumberFor_DisplayChoicesOnBoard : posTags
    let nomenclature = movePosChoicesNomenclature numberedMovesWithPos
    let prompt = "Move choices for " ++ show playerColor ++ ": (" ++ show choiceNumberFor_Resign ++ ":resign, " ++ show choiceNumberFor_DisplayChoicesOnBoard ++ ":show) " ++ nomenclature ++ "\nEnter choice"
    getValidMoveChoice prompt validChoices


getValidMoveChoice :: String -> [Int] -> IO Int
getValidMoveChoice prompt validChoices = do
    let same = getValidMoveChoice prompt validChoices

    let
        again :: IO Int -> IO Int
        again f = do
            putStrLn "Invalid input, try again...  "
            f

    putStrLn prompt
    line <- getLine

    case (readMaybe line :: Maybe Int) of
        Just n ->
            if elem n validChoices then
                return n
            else do
                again same
        Nothing -> do
            again same


choiceNumberFor_DisplayChoicesOnBoard :: Int
choiceNumberFor_DisplayChoicesOnBoard = 
    99


choiceNumberFor_Resign :: Int
choiceNumberFor_Resign =
    0