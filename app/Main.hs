module Main where

import Board ( applyMove, initialBoard, board_DisplayString, moveChoices, validMoves, filledPositions, boardSquaresColored, numSquaresColored )
import Disk ( Color(..) )

main :: IO ()
main = do   let playerColor = Black
            let board = initialBoard
            let moves = validMoves playerColor board
            putStrLn $ board_DisplayString board

            putStrLn $ show $ numSquaresColored board

            putStrLn $ "Move choices for Black: " ++ moveChoices moves
            putStrLn "Enter choice"
            n <- readLn -- todo validate entry
            let board2 = applyMove (moves !! (n-1)) board
            putStrLn "\n"
            putStrLn $ board_DisplayString board2

            -- putStrLn $ show $ filledPositions White board2
            -- putStrLn $ show $ filledPositions Black board2
            -- putStrLn $ show $ boardSquaresColored White board2
            -- putStrLn $ show $ boardSquaresColored Black board2
            putStrLn $ show $ numSquaresColored board2
