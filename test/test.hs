-- http://documentup.com/feuerbach/tasty

import Test.Tasty
import Test.Tasty.SmallCheck as SC
import Test.Tasty.QuickCheck as QC
import Test.Tasty.HUnit

import Data.Function ( (&) )

import Board ( Color(..), initialBoard, validMoves, board_DisplayString,              makeBoard, place, makeWhiteDisk, makeBlackDisk, boardAt )
import Position ( PosRow(..), radiatingPosRows )


main = defaultMain tests

tests :: TestTree
tests = testGroup "Tests" [unitTests] 

-- invariant: flipCount for 4 corners == 0 


board_Figure2 =
    let
      board = makeBoard
    in
        board -- http://www.boardgamecapital.com/game_rules/othello.pdf
            & place makeWhiteDisk (boardAt board (3,3)) 
            & place makeWhiteDisk (boardAt board (3,7))
            & place makeWhiteDisk (boardAt board (7,5))

            & place makeBlackDisk (boardAt board (4,3))
            & place makeBlackDisk (boardAt board (4,6))
            & place makeBlackDisk (boardAt board (5,3))
            & place makeBlackDisk (boardAt board (5,5))
            & place makeBlackDisk (boardAt board (6,3))
            & place makeBlackDisk (boardAt board (6,4))
            & place makeBlackDisk (boardAt board (7,4))


unitTests = testGroup "Unit tests"
  [ testCase "board_DisplayString initialBoard" $
    board_DisplayString initialBoard @?= "   A  B  C  D  E  F  G  H\n1  -  -  -  -  -  -  -  - \n2  -  -  -  -  -  -  -  - \n3  -  -  -  -  -  -  -  - \n4  -  -  -  o  x  -  -  - \n5  -  -  -  x  o  -  -  - \n6  -  -  -  -  -  -  -  - \n7  -  -  -  -  -  -  -  - \n8  -  -  -  -  -  -  -  - \n"
  
  , testCase "radiatingPosRows (1,1)" $
    radiatingPosRows (1,1)  @?= [PosRow [(2,1),(3,1),(4,1),(5,1),(6,1),(7,1),(8,1)],PosRow [(1,2),(1,3),(1,4),(1,5),(1,6),(1,7),(1,8)],PosRow [(2,2),(3,3),(4,4),(5,5),(6,6),(7,7),(8,8)]]

  , testCase "raysFrom (1,8)" $
    radiatingPosRows (1,8)  @?= [PosRow [(2,8),(3,8),(4,8),(5,8),(6,8),(7,8),(8,8)],PosRow [(1,7),(1,6),(1,5),(1,4),(1,3),(1,2),(1,1)],PosRow [(2,7),(3,6),(4,5),(5,4),(6,3),(7,2),(8,1)]]

  , testCase "raysFrom (8,8)" $
    radiatingPosRows (8,8)  @?= [PosRow [(7,8),(6,8),(5,8),(4,8),(3,8),(2,8),(1,8)],PosRow [(8,7),(8,6),(8,5),(8,4),(8,3),(8,2),(8,1)],PosRow [(7,7),(6,6),(5,5),(4,4),(3,3),(2,2),(1,1)]]

  , testCase "raysFrom (8,1)" $
    radiatingPosRows (8,1)  @?= [PosRow [(7,1),(6,1),(5,1),(4,1),(3,1),(2,1),(1,1)],PosRow [(8,2),(8,3),(8,4),(8,5),(8,6),(8,7),(8,8)],PosRow [(7,2),(6,3),(5,4),(4,5),(3,6),(2,7),(1,8)]]

  , testCase "validMoves Black (5,6)" $
    radiatingPosRows (5,6)  @?= [PosRow [(4,6),(3,6),(2,6),(1,6)],PosRow [(6,6),(7,6),(8,6)],PosRow [(5,7),(5,8)],PosRow [(5,5),(5,4),(5,3),(5,2),(5,1)],PosRow [(4,7),(3,8)],PosRow [(4,5),(3,4),(2,3),(1,2)],PosRow [(6,7),(7,8)],PosRow [(6,5),(7,4),(8,3)]]
  
  , testCase "validMoves Black initialBoard" $
    (show $ validMoves Black initialBoard) @?= "[Move {_square = EmptySquare (4,3), _outflanks = [FilledRow [FilledSquare (4,4) Disk {_initColor = White, _flipCount = 0}]]},Move {_square = EmptySquare (3,4), _outflanks = [FilledRow [FilledSquare (4,4) Disk {_initColor = White, _flipCount = 0}]]},Move {_square = EmptySquare (6,5), _outflanks = [FilledRow [FilledSquare (5,5) Disk {_initColor = White, _flipCount = 0}]]},Move {_square = EmptySquare (5,6), _outflanks = [FilledRow [FilledSquare (5,5) Disk {_initColor = White, _flipCount = 0}]]}]"

  , testCase "validMoves Black board_Figure2" $
    (show $ validMoves Black board_Figure2) @?= "[Move {_square = EmptySquare (2,3), _outflanks = [FilledRow [FilledSquare (3,3) Disk {_initColor = White, _flipCount = 0}]]},Move {_square = EmptySquare (2,8), _outflanks = [FilledRow [FilledSquare (3,7) Disk {_initColor = White, _flipCount = 0}]]},Move {_square = EmptySquare (7,6), _outflanks = [FilledRow [FilledSquare (7,5) Disk {_initColor = White, _flipCount = 0}]]},Move {_square = EmptySquare (8,6), _outflanks = [FilledRow [FilledSquare (7,5) Disk {_initColor = White, _flipCount = 0}]]}]"

  , testCase "validMoves White board_Figure2" $
    (show $ validMoves White board_Figure2) @?= "[Move {_square = EmptySquare (4,2), _outflanks = [FilledRow [FilledSquare (5,3) Disk {_initColor = Black, _flipCount = 0},FilledSquare (6,4) Disk {_initColor = Black, _flipCount = 0}]]},Move {_square = EmptySquare (7,3), _outflanks = [FilledRow [FilledSquare (6,3) Disk {_initColor = Black, _flipCount = 0},FilledSquare (5,3) Disk {_initColor = Black, _flipCount = 0},FilledSquare (4,3) Disk {_initColor = Black, _flipCount = 0}],FilledRow [FilledSquare (7,4) Disk {_initColor = Black, _flipCount = 0}],FilledRow [FilledSquare (6,4) Disk {_initColor = Black, _flipCount = 0},FilledSquare (5,5) Disk {_initColor = Black, _flipCount = 0},FilledSquare (4,6) Disk {_initColor = Black, _flipCount = 0}]]}]"
  ]


