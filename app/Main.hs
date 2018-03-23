module Main where

import Text.Read

import GameState ( GameState(..), PlayGameState(..), EndGameState(..), All_State(..), applyMove, makePlayGameState, nextToMove, possibleMoves, gameStateDisplay )
import Board ( Move, movePosChoices, movePosChoicesNomenclature, validMoves, filledPositions, boardSquaresColored, numSquaresColored )
import Disk ( Color(..) )
import Position ( Position )
 
  
choiceNumberFor_DisplayChoicesOnBoard :: Int
choiceNumberFor_DisplayChoicesOnBoard = 
    99


choiceNumberFor_Resign :: Int
choiceNumberFor_Resign =
    0


main :: IO ()
main = do   
    --let playerColor = Black
    --let board = initialBoard
    --let moves = validMoves playerColor board

    let playGameState = makePlayGameState
    let playerColor = nextToMove $ PlayState playGameState
    let moves = possibleMoves $ PlayState playGameState
    let numberedMovesWithPos = movePosChoices moves
    --isHighlightMove <- getIsHighlightMove
    
    putStrLn $ gameStateDisplay Nothing $ PlayState playGameState

    n <- getMoveChoice playerColor numberedMovesWithPos

    if n == choiceNumberFor_Resign then do
        return ()
    else if n == choiceNumberFor_DisplayChoicesOnBoard then do
        putStrLn $ gameStateDisplay (Just numberedMovesWithPos) $ PlayState playGameState
    else do
        let tagged = applyMove (moves !! (n-1)) playGameState
        putStrLn $ gameStateDisplay Nothing tagged


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

{- 
getIsHighlightMove :: IO Bool
getIsHighlightMove = do
    let same = getIsHighlightMove

    let
        again :: IO Bool -> IO Bool
        again f = do
            putStrLn "Invalid input, try again...  "
            f

    putStrLn "Highlight current move? (T/F)"
    line <- getLine
    putStrLn line

    case (readMaybe line :: Maybe String) of
        Just s ->
            if s == "T" then
                return True
            else if s == "F" then
                return False
            else do
                again same
        Nothing -> do
            again same 
-}