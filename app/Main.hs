module Main where

import Text.Read
import Board ( Move, applyMove, initialBoard, board_DisplayString, moveChoices, validMoves, filledPositions, boardSquaresColored, numSquaresColored )
import Disk ( Color(..) )

main :: IO ()
main = do   
    let playerColor = Black
    let board = initialBoard
    let moves = validMoves playerColor board
    putStrLn $ board_DisplayString board

    putStrLn $ show $ numSquaresColored board
    n <- getMoveChoice moves

    if n == 0 then do
        return ()
    else do
        let board2 = applyMove (moves !! (n-1)) board
        putStrLn "\n"
        putStrLn $ board_DisplayString board2


getMoveChoice :: [Move] -> IO Int
getMoveChoice moves = do
    let choices = [(0::Int)..length moves]
    let prompt = "Move choices for Black: (0:resign) " ++ moveChoices moves ++ "\nEnter choice"
    getValidMoveChoice prompt choices


getValidMoveChoice :: String -> [Int] -> IO Int
getValidMoveChoice prompt choices = do
    let same = getValidMoveChoice prompt choices
    putStrLn prompt
    line <- getLine

    case (readMaybe line :: Maybe Int) of
        Just n ->
            if elem n choices then
                return n
            else do
                again same
        Nothing -> do
            again same


again :: IO Int -> IO Int
again f = do
    putStrLn "Invalid input, try again...  "
    f
