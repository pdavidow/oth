-- http://documentup.com/feuerbach/tasty

import Test.Tasty
import Test.Tasty.SmallCheck as SC
import Test.Tasty.QuickCheck as QC
import Test.Tasty.HUnit

import Data.Function ( (&) )

import Board ( EmptySquare(..), Move(..), FilledRow(..), BoardSquare(..), initialBoard, validMoves, boardDisplay, boardFromConfig, toPos, applyBoardMove, filledPositions, boardWithValidMovesDisplay, movePosChoices)
import Position ( PosRow(..), radiatingPosRows )
import Disk ( Color(..) )
import GameState ( All_State(..), applyMove, makePlayGameState, nextToMove, possibleMoves, gameStateDisplay, blackAndWhiteUnusedDiskCounts )

main = defaultMain tests

tests :: TestTree
tests = testGroup "Tests" [unitTests] 

-- invariant: flipCount for 4 corners == 0 


filledRowToPosRow :: FilledRow -> PosRow
filledRowToPosRow (FilledRow xs) =
  PosRow $ xs
      & map (\ x -> toPos $ Board_FilledSquare x)


board_Figure2 =
    boardFromConfig -- page 2: http://www.boardgamecapital.com/game_rules/othello.pdf
        [ (White,(3,3)), (White,(3,7)), (White,(7,5)),
          (Black,(4,3)), (Black,(4,6)), (Black,(5,3)), (Black,(5,5)), (Black,(6,3)), (Black,(6,4)), (Black,(7,4))
        ]


unitTests = testGroup "Unit tests" $
    [ testGroup "module Position" $
        [ testCase "radiatingPosRows (1,1)" $
          radiatingPosRows (1,1)  @?= [PosRow [(2,1),(3,1),(4,1),(5,1),(6,1),(7,1),(8,1)], PosRow [(1,2),(1,3),(1,4),(1,5),(1,6),(1,7),(1,8)], PosRow [(2,2),(3,3),(4,4),(5,5),(6,6),(7,7),(8,8)]]

        , testCase "radiatingPosRows (1,8)" $
          radiatingPosRows (1,8)  @?= [PosRow [(2,8),(3,8),(4,8),(5,8),(6,8),(7,8),(8,8)], PosRow [(1,7),(1,6),(1,5),(1,4),(1,3),(1,2),(1,1)], PosRow [(2,7),(3,6),(4,5),(5,4),(6,3),(7,2),(8,1)]]

        , testCase "radiatingPosRows (8,8)" $
          radiatingPosRows (8,8)  @?= [PosRow [(7,8),(6,8),(5,8),(4,8),(3,8),(2,8),(1,8)], PosRow [(8,7),(8,6),(8,5),(8,4),(8,3),(8,2),(8,1)], PosRow [(7,7),(6,6),(5,5),(4,4),(3,3),(2,2),(1,1)]]

        , testCase "radiatingPosRows (8,1)" $
          radiatingPosRows (8,1)  @?= [PosRow [(7,1),(6,1),(5,1),(4,1),(3,1),(2,1),(1,1)], PosRow [(8,2),(8,3),(8,4),(8,5),(8,6),(8,7),(8,8)], PosRow [(7,2),(6,3),(5,4),(4,5),(3,6),(2,7),(1,8)]]

        , testCase "radiatingPosRows Black (5,6)" $
          radiatingPosRows (5,6)  @?= [PosRow [(4,6),(3,6),(2,6),(1,6)], PosRow [(6,6),(7,6),(8,6)], PosRow [(5,7),(5,8)], PosRow [(5,5),(5,4),(5,3),(5,2),(5,1)], PosRow [(4,7),(3,8)], PosRow [(4,5),(3,4),(2,3),(1,2)], PosRow [(6,7),(7,8)], PosRow [(6,5),(7,4),(8,3)]]
        ]

    , testGroup "module Board" $       
        [ testCase "board_DisplayString initialBoard" $
        boardDisplay initialBoard @?= "   A  B  C  D  E  F  G  H  \n1  .  .  .  .  .  .  .  . \n2  .  .  .  .  .  .  .  . \n3  .  .  .  .  .  .  .  . \n4  .  .  .  O  X  .  .  . \n5  .  .  .  X  O  .  .  . \n6  .  .  .  .  .  .  .  . \n7  .  .  .  .  .  .  .  . \n8  .  .  .  .  .  .  .  . \n"
          

        , testCase "boardWithValidMoves_DisplayString" $
          let
              board = initialBoard
          in
            boardWithValidMovesDisplay (movePosChoices $ validMoves Black board) board @?= "   A  B  C  D  E  F  G  H  \n1  .  .  .  .  .  .  .  . \n2  .  .  .  .  .  .  .  . \n3  .  .  .  2  .  .  .  . \n4  .  .  1  O  X  .  .  . \n5  .  .  .  X  O  4  .  . \n6  .  .  .  .  3  .  .  . \n7  .  .  .  .  .  .  .  . \n8  .  .  .  .  .  .  .  . \n"

        , testGroup "validMoves Black initialBoard" $
            let
                moves = validMoves Black initialBoard

                move0 = moves !! 0
                move1 = moves !! 1
                move2 = moves !! 2
                move3 = moves !! 3

                (EmptySquare pos0 _) = _square move0
                (EmptySquare pos1 _) = _square move1
                (EmptySquare pos2 _) = _square move2
                (EmptySquare pos3 _) = _square move3

                outflanks0 = map filledRowToPosRow $ _outflanks move0
                outflanks1 = map filledRowToPosRow $ _outflanks move1
                outflanks2 = map filledRowToPosRow $ _outflanks move2
                outflanks3 = map filledRowToPosRow $ _outflanks move3
            in
                [ testCase "4 moves" $ 
                  length moves @?= 4

                , testCase "move 0" $
                  pos0 @?= (4,3)

                , testCase "outflanks 0" $
                  outflanks0 @?= [PosRow [(4,4)]] 

                , testCase "move 1" $
                  pos1 @?= (3,4)     

                , testCase "outflanks 1" $
                  outflanks1 @?= [PosRow [(4,4)]]                   

                , testCase "move 2" $
                  pos2 @?= (6,5)    

                , testCase "outflanks 2" $
                  outflanks2 @?= [PosRow [(5,5)]]                        

                , testCase "move 3" $
                  pos3 @?= (5,6)           

                , testCase "outflanks 3" $
                  outflanks3 @?= [PosRow [(5,5)]]                                                                        
                ]

        , testGroup "validMoves Black board_Figure2" $
            let
                moves = validMoves Black board_Figure2

                move0 = moves !! 0
                move1 = moves !! 1
                move2 = moves !! 2
                move3 = moves !! 3

                (EmptySquare pos0 _) = _square move0
                (EmptySquare pos1 _) = _square move1
                (EmptySquare pos2 _) = _square move2
                (EmptySquare pos3 _) = _square move3

                outflanks0 = map filledRowToPosRow $ _outflanks move0
                outflanks1 = map filledRowToPosRow $ _outflanks move1
                outflanks2 = map filledRowToPosRow $ _outflanks move2
                outflanks3 = map filledRowToPosRow $ _outflanks move3
            in
                [ testCase "4 moves" $ 
                  length moves @?= 4

                , testCase "move 0" $
                  pos0 @?= (2,3)

                , testCase "outflanks 0" $
                  outflanks0 @?= [PosRow [(3,3)]] 

                , testCase "move 1" $
                  pos1 @?= (2,8)     

                , testCase "outflanks 1" $
                  outflanks1 @?= [PosRow [(3,7)]]                   

                , testCase "move 2" $
                  pos2 @?= (7,6)    

                , testCase "outflanks 2" $
                  outflanks2 @?= [PosRow [(7,5)]]                        

                , testCase "move 3" $
                  pos3 @?= (8,6)           

                , testCase "outflanks 3" $
                  outflanks3 @?= [PosRow [(7,5)]]                                                                        
                ]   

        , testGroup "validMoves White board_Figure2" $
            let
                moves = validMoves White board_Figure2

                move0 = moves !! 0
                move1 = moves !! 1

                (EmptySquare pos0 _) = _square move0
                (EmptySquare pos1 _) = _square move1

                outflanks0 = map filledRowToPosRow $ _outflanks move0
                outflanks1 = map filledRowToPosRow $ _outflanks move1
            in
                [ testCase "2 moves" $ 
                  length moves @?= 2

                , testCase "move 0" $
                  pos0 @?= (4,2)

                , testCase "outflanks 0" $
                  outflanks0 @?= [PosRow [(5,3), (6,4)]] 

                , testCase "move 1" $
                  pos1 @?= (7,3)     

                , testCase "outflanks 1" $
                  outflanks1 @?= 
                      [ PosRow [(6,3), (5,3), (4,3)]
                      , PosRow [(7,4)]
                      , PosRow [(6,4), (5,5), (4,6)]
                      ]                                                                                      
                ]  

        , testGroup "apply: validMoves White board_Figure2" $
            [ testGroup "move0" $
                let
                  boardBefore = board_Figure2
                  moves = validMoves White boardBefore
                  move0 = moves !! 0
                  boardAfter = applyBoardMove move0 boardBefore
                in
                    [ testCase "white positions" $ 
                      filledPositions White boardAfter @?= [(3,3), (3,7), (4,2), (5,3), (6,4), (7,5)]

                    , testCase "black positions" $
                      filledPositions Black boardAfter @?= [(4,3), (4,6), (5,5), (6,3), (7,4)]                                                                                   
                    ]  

            , testGroup "move1" $
                let
                    boardBefore = board_Figure2
                    moves = validMoves White boardBefore
                    move1 = moves !! 1
                    boardAfter = applyBoardMove move1 boardBefore
                in
                    [ testCase "white positions" $ 
                      filledPositions White boardAfter @?= [(3,3), (3,7), (4,3), (4,6), (5,3), (5,5), (6,3), (6,4), (7,3), (7,4), (7,5)]

                    , testCase "black positions" $
                      filledPositions Black boardAfter @?= []                                                                                   
                    ]    
              ]
          ]
    , testGroup "module GameState" $ 
        let
          playGameState1 = makePlayGameState
          tagged1 = PlayState playGameState1
          moves1 = possibleMoves tagged1

          tagged2 = applyMove (head moves1) playGameState1
          moves2 = possibleMoves tagged2
          (PlayState playGameState2) = tagged2

          tagged3 = applyMove (head moves2) playGameState2

          numberedMovesWithPos1 = movePosChoices moves1
          numberedMovesWithPos2 = movePosChoices moves2

          display1 = gameStateDisplay Nothing tagged1
          display2 = gameStateDisplay (Just numberedMovesWithPos1) tagged1
          display3 = gameStateDisplay Nothing tagged2
          display4 = gameStateDisplay (Just numberedMovesWithPos2) tagged2

          (b1, w1) = blackAndWhiteUnusedDiskCounts tagged1
          (b2, w2) = blackAndWhiteUnusedDiskCounts tagged2
          (b3, w3) = blackAndWhiteUnusedDiskCounts tagged3

          (c1, c2, c3) = (nextToMove tagged1, nextToMove tagged2, nextToMove tagged3)
        in
            [ testCase "initial: gameStateDisplay Nothing" $ 
              display1 @?= "   A  B  C  D  E  F  G  H  \n1  .  .  .  .  .  .  .  . \n2  .  .  .  .  .  .  .  . \n3  .  .  .  .  .  .  .  . \n4  .  .  .  O  X  .  .  . \n5  .  .  .  X  O  .  .  . \n6  .  .  .  .  .  .  .  . \n7  .  .  .  .  .  .  .  . \n8  .  .  .  .  .  .  .  . \n\nAvailable Disks\nBlack: XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX\nWhite: OOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOO"   
              
            , testCase "initial: gameStateDisplay (Just numberedMovesWithPos)" $ 
              display2 @?= "   A  B  C  D  E  F  G  H  \n1  .  .  .  .  .  .  .  . \n2  .  .  .  .  .  .  .  . \n3  .  .  .  2  .  .  .  . \n4  .  .  1  O  X  .  .  . \n5  .  .  .  X  O  4  .  . \n6  .  .  .  .  3  .  .  . \n7  .  .  .  .  .  .  .  . \n8  .  .  .  .  .  .  .  . \n\nAvailable Disks\nBlack: XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX\nWhite: OOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOO"   

            , testCase "After move C4: gameStateDisplay Nothing" $ 
              display3 @?= "   A  B  C  D  E  F  G  H  \n1  .  .  .  .  .  .  .  . \n2  .  .  .  .  .  .  .  . \n3  .  .  .  .  .  .  .  . \n4  .  .  X  X  X  .  .  . \n5  .  .  .  X  O  .  .  . \n6  .  .  .  .  .  .  .  . \n7  .  .  .  .  .  .  .  . \n8  .  .  .  .  .  .  .  . \n\nAvailable Disks\nBlack: XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX\nWhite: OOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOO"  
 
            , testCase "After move C4: gameStateDisplay (Just numberedMovesWithPos)" $ 
              display4 @?= "   A  B  C  D  E  F  G  H  \n1  .  .  .  .  .  .  .  . \n2  .  .  .  .  .  .  .  . \n3  .  .  1  .  3  .  .  . \n4  .  .  X  X  X  .  .  . \n5  .  .  2  X  O  .  .  . \n6  .  .  .  .  .  .  .  . \n7  .  .  .  .  .  .  .  . \n8  .  .  .  .  .  .  .  . \n\nAvailable Disks\nBlack: XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX\nWhite: OOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOO"

            , testCase "initial unused disk counts" $ 
              (b1, w1) @?= (32, 32) 

            , testCase "unused disk counts after 1st move (Black)" $ 
              (b2, w2) @?= (31, 32)   

            , testCase "unused disk counts after 2nd move (White)" $ 
              (b3, w3) @?= (31, 31)  

            , testCase "nextToMove for first 3 moves" $ 
              (c1, c2, c3) @?= (Black, White, Black)                  
            ]  
    ]

