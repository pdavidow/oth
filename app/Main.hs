module Main where

import Text.Read
import Board ( Move, applyMove, initialBoard, board_DisplayString, boardWithValidMoves_DisplayString, movePosChoices, movePosChoicesNomenclature, validMoves, filledPositions, boardSquaresColored, numSquaresColored )
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
    let playerColor = Black
    let board = initialBoard
    let moves = validMoves playerColor board
    let numberedMovesWithPos = movePosChoices moves
    putStrLn $ board_DisplayString board

    --putStrLn $ show $ numSquaresColored board
    n <- getMoveChoice playerColor numberedMovesWithPos

    if n == 0 then do
        return ()
    else if n == choiceNumberFor_DisplayChoicesOnBoard then do
        putStrLn "\n"
        putStrLn $ boardWithValidMoves_DisplayString numberedMovesWithPos board
    else do
        let board2 = applyMove (moves !! (n-1)) board
        putStrLn "\n"
        putStrLn $ board_DisplayString board2


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


again :: IO Int -> IO Int
again f = do
    putStrLn "Invalid input, try again...  "
    f
