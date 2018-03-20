module Main where

import Board ( applyMove, initialBoard, board_DisplayString, moveChoices, validMoves, filledPositions, squaresColored )
import Disk ( Color(..) )

main :: IO ()
main = do   let playerColor = Black
            let board = initialBoard
            let moves = validMoves playerColor board
            putStrLn $ board_DisplayString board
            putStrLn $ "Move choices for Black: " ++ moveChoices moves
            putStrLn "Please enter choice#"
            n <- readLn
            let board2 = applyMove (moves !! (n-1)) board
            putStrLn $ board_DisplayString board2

            -- putStrLn $ show $ filledPositions White board2
            -- putStrLn $ show $ filledPositions Black board2
            -- putStrLn $ show $ squaresColored White board2
            -- putStrLn $ show $ squaresColored Black board2