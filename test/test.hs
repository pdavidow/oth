-- http://documentup.com/feuerbach/tasty
 
import Test.Tasty
import Test.Tasty.SmallCheck as SC
import Test.Tasty.QuickCheck as QC
import Test.Tasty.HUnit
 
import Data.Function ( (&) )
import Data.List ( foldl' )
 
import Board ( Board, EmptySquare(..), FilledSquare, Move(..), Outflanks(..), FilledRow(..), Tagged_Square(..), emptySquares, initialBoard, validMoves, boardFromConfig, toPos, applyBoardMove, filledPositions, movePosChoices, diskFrom, filledSquares, boardAt) --, flipAt)
import Position ( PosRow(..), radiatingPosRows )
import Disk ( Color(..), flipCount )
import State ( CoreState(..), StartState(..), MidState(..), EndState(..), Tagged_State(..), EndReason(..), applyMove, makeStartState, priorMoveColor, actual_NextMoves_FromTaggedState, actual_BlackAndWhiteUnusedDiskCounts_FromTaggedState, board_FromTaggedState)
import UnusedDiskCount ( Tagged_UnusedDiskCount(..), countFrom, decreaseByOne )
import BoardSize ( boardSize )
import Position ( Position, makeSomePosition, posCoords )
import Display ( boardDisplay, boardWithFlipCountDisplay, gameStateDisplay, showMoveNumInEmptySquare )

main = defaultMain tests

tests :: TestTree
tests = testGroup "Tests" [unitTests] 

filledRowToPosRow :: FilledRow -> PosRow
filledRowToPosRow (FilledRow xs) =
  PosRow $ xs
      & map (\ x -> toPos $ Tagged_FilledSquare x)


board_Figure2 :: Board 
board_Figure2 =
  boardFromConfig 
    [ (White, (makeSomePosition 3 3))
    , (White, (makeSomePosition 3 7))
    , (White, (makeSomePosition 7 5))
    , (Black, (makeSomePosition 4 3))
    , (Black, (makeSomePosition 4 6))
    , (Black, (makeSomePosition 5 3))
    , (Black, (makeSomePosition 5 5))
    , (Black, (makeSomePosition 6 3))
    , (Black, (makeSomePosition 6 4))
    , (Black, (makeSomePosition 7 4))
    ]


boardWithValidMovesDisplay :: [(Int, Position)] -> Board -> String
boardWithValidMovesDisplay showMoves board = 
    boardDisplay 
        (Just $ showMoveNumInEmptySquare showMoves)
        Nothing
        board


unitTests = testGroup "Unit tests" $
    [ testGroup "module Position" $
        [ testCase "radiatingPosRows (Position 1 1)" $
            radiatingPosRows (makeSomePosition 1 1)  @?= 
                [ PosRow 
                    [ (makeSomePosition 2 1)
                    , (makeSomePosition 3 1)
                    , (makeSomePosition 4 1)
                    , (makeSomePosition 5 1)
                    , (makeSomePosition 6 1) 
                    , (makeSomePosition 7 1)
                    , (makeSomePosition 8 1)
                    ]
                , PosRow 
                    [ (makeSomePosition 1 2)
                    , (makeSomePosition 1 3)
                    , (makeSomePosition 1 4)
                    , (makeSomePosition 1 5)
                    , (makeSomePosition 1 6)
                    , (makeSomePosition 1 7)
                    , (makeSomePosition 1 8)
                    ]
                , PosRow 
                    [ (makeSomePosition 2 2)
                    , (makeSomePosition 3 3)
                    , (makeSomePosition 4 4)
                    , (makeSomePosition 5 5)
                    , (makeSomePosition 6 6)
                    , (makeSomePosition 7 7)
                    , (makeSomePosition 8 8)
                    ]
                ]

        , testCase "radiatingPosRows (Position 1 8)" $
            radiatingPosRows (makeSomePosition 1 8)  @?= 
              [ PosRow 
                  [ (makeSomePosition 2 8)
                  , (makeSomePosition 3 8)
                  , (makeSomePosition 4 8)
                  , (makeSomePosition 5 8)
                  , (makeSomePosition 6 8) 
                  , (makeSomePosition 7 8)
                  , (makeSomePosition 8 8)
                  ]
              , PosRow 
                  [ (makeSomePosition 1 7)
                  , (makeSomePosition 1 6)
                  , (makeSomePosition 1 5)
                  , (makeSomePosition 1 4)
                  , (makeSomePosition 1 3)
                  , (makeSomePosition 1 2)
                  , (makeSomePosition 1 1)
                  ]
              , PosRow 
                  [ (makeSomePosition 2 7)
                  , (makeSomePosition 3 6)
                  , (makeSomePosition 4 5)
                  , (makeSomePosition 5 4)
                  , (makeSomePosition 6 3)
                  , (makeSomePosition 7 2)
                  , (makeSomePosition 8 1)
                  ]
              ]

        , testCase "radiatingPosRows (Position 8 8)" $
            radiatingPosRows (makeSomePosition 8 8)  @?= 
              [ PosRow 
                  [ (makeSomePosition 7 8)
                  , (makeSomePosition 6 8)
                  , (makeSomePosition 5 8)
                  , (makeSomePosition 4 8)
                  , (makeSomePosition 3 8) 
                  , (makeSomePosition 2 8)
                  , (makeSomePosition 1 8)
                  ]
              , PosRow 
                  [ (makeSomePosition 8 7)
                  , (makeSomePosition 8 6)
                  , (makeSomePosition 8 5)
                  , (makeSomePosition 8 4)
                  , (makeSomePosition 8 3)
                  , (makeSomePosition 8 2)
                  , (makeSomePosition 8 1)
                  ]
              , PosRow 
                  [ (makeSomePosition 7 7)
                  , (makeSomePosition 6 6)
                  , (makeSomePosition 5 5)
                  , (makeSomePosition 4 4)
                  , (makeSomePosition 3 3)
                  , (makeSomePosition 2 2)
                  , (makeSomePosition 1 1)
                  ]
              ]

        , testCase "radiatingPosRows (Position 8 1)" $
            radiatingPosRows (makeSomePosition 8 1)  @?= 
              [ PosRow 
                  [ (makeSomePosition 7 1)
                  , (makeSomePosition 6 1)
                  , (makeSomePosition 5 1)
                  , (makeSomePosition 4 1)
                  , (makeSomePosition 3 1) 
                  , (makeSomePosition 2 1)
                  , (makeSomePosition 1 1)
                  ]
              , PosRow 
                  [ (makeSomePosition 8 2)
                  , (makeSomePosition 8 3)
                  , (makeSomePosition 8 4)
                  , (makeSomePosition 8 5)
                  , (makeSomePosition 8 6)
                  , (makeSomePosition 8 7)
                  , (makeSomePosition 8 8)
                  ]
              , PosRow 
                  [ (makeSomePosition 7 2)
                  , (makeSomePosition 6 3)
                  , (makeSomePosition 5 4)
                  , (makeSomePosition 4 5)
                  , (makeSomePosition 3 6)
                  , (makeSomePosition 2 7)
                  , (makeSomePosition 1 8)
                  ]
              ]

        , testCase "radiatingPosRows Black (Position 5 6)" $
            radiatingPosRows (makeSomePosition 5 6)  @?= 
              [ PosRow 
                  [ (makeSomePosition 4 6)
                  , (makeSomePosition 3 6)
                  , (makeSomePosition 2 6)
                  , (makeSomePosition 1 6)
                  ]
              , PosRow 
                  [ (makeSomePosition 6 6)
                  , (makeSomePosition 7 6)
                  , (makeSomePosition 8 6)
                  ]
              , PosRow 
                  [ (makeSomePosition 5 7)
                  , (makeSomePosition 5 8)
                  ]
              , PosRow 
                  [ (makeSomePosition 5 5)
                  , (makeSomePosition 5 4)
                  , (makeSomePosition 5 3)
                  , (makeSomePosition 5 2)
                  , (makeSomePosition 5 1)
                  ]
              , PosRow 
                  [ (makeSomePosition 4 7)
                  , (makeSomePosition 3 8)
                  ]
              , PosRow 
                  [ (makeSomePosition 4 5)
                  , (makeSomePosition 3 4)
                  , (makeSomePosition 2 3)
                  , (makeSomePosition 1 2)
                  ]
              , PosRow 
                  [ (makeSomePosition 6 7)
                  , (makeSomePosition 7 8)
                  ]
              , PosRow 
                  [ (makeSomePosition 6 5)
                  , (makeSomePosition 7 4)
                  , (makeSomePosition 8 3)
                  ]
              ]
          ]

    , testGroup "module Board" $       
        [ testCase "board_DisplayString initialBoard" $
          boardDisplay Nothing Nothing initialBoard @?= "    A    B    C    D    E    F    G    H    \n1   .    .    .    .    .    .    .    .  \n\n2   .    .    .    .    .    .    .    .  \n\n3   .    .    .    .    .    .    .    .  \n\n4   .    .    .    o    x    .    .    .  \n\n5   .    .    .    x    o    .    .    .  \n\n6   .    .    .    .    .    .    .    .  \n\n7   .    .    .    .    .    .    .    .  \n\n8   .    .    .    .    .    .    .    .  "

        , testCase "boardWithValidMoves_DisplayString" $
          let
              board = initialBoard
          in
              boardWithValidMovesDisplay (movePosChoices $ validMoves Black board) board @?= "    A    B    C    D    E    F    G    H    \n1   .    .    .    .    .    .    .    .  \n\n2   .    .    .    .    .    .    .    .  \n\n3   .    .    .    2    .    .    .    .  \n\n4   .    .    1    o    x    .    .    .  \n\n5   .    .    .    x    o    4    .    .  \n\n6   .    .    .    .    3    .    .    .  \n\n7   .    .    .    .    .    .    .    .  \n\n8   .    .    .    .    .    .    .    .  "

        , testGroup "validMoves Black initialBoard" $
            let
                moves = validMoves Black initialBoard

                (Move _ (EmptySquare pos0 _) (Outflanks o0)) = moves !! 0
                (Move _ (EmptySquare pos1 _) (Outflanks o1)) = moves !! 1
                (Move _ (EmptySquare pos2 _) (Outflanks o2)) = moves !! 2
                (Move _ (EmptySquare pos3 _) (Outflanks o3)) = moves !! 3

                outflanks0 = map filledRowToPosRow o0
                outflanks1 = map filledRowToPosRow o1
                outflanks2 = map filledRowToPosRow o2
                outflanks3 = map filledRowToPosRow o3
            in
                [ testCase "4 moves" $ 
                  length moves @?= 4

                , testCase "move 0" $
                  posCoords pos0 @?= (4,3)

                , testCase "outflanks 0" $
                  outflanks0 @?= [PosRow [(makeSomePosition 4 4)]] 

                , testCase "move 1" $
                  posCoords pos1 @?= (3,4)     

                , testCase "outflanks 1" $
                  outflanks1 @?= [PosRow [(makeSomePosition 4 4)]]                   

                , testCase "move 2" $
                  posCoords pos2 @?= (6,5)    

                , testCase "outflanks 2" $
                  outflanks2 @?= [PosRow [(makeSomePosition 5 5)]]                        

                , testCase "move 3" $
                  posCoords pos3 @?= (5,6)           

                , testCase "outflanks 3" $
                  outflanks3 @?= [PosRow [(makeSomePosition 5 5)]]                                                                        
                ]

        , testGroup "validMoves Black board_Figure2" $
            let
                moves = validMoves Black board_Figure2

                (Move _ (EmptySquare pos0 _) (Outflanks o0)) = moves !! 0
                (Move _ (EmptySquare pos1 _) (Outflanks o1)) = moves !! 1
                (Move _ (EmptySquare pos2 _) (Outflanks o2)) = moves !! 2
                (Move _ (EmptySquare pos3 _) (Outflanks o3)) = moves !! 3

                outflanks0 = map filledRowToPosRow o0
                outflanks1 = map filledRowToPosRow o1
                outflanks2 = map filledRowToPosRow o2
                outflanks3 = map filledRowToPosRow o3
            in
                [ testCase "4 moves" $ 
                  length moves @?= 4

                , testCase "move 0" $
                  posCoords pos0 @?= (2,3)

                , testCase "outflanks 0" $
                  outflanks0 @?= [PosRow [(makeSomePosition 3 3)]] 

                , testCase "move 1" $
                  posCoords pos1 @?= (2,8)     

                , testCase "outflanks 1" $
                  outflanks1 @?= [PosRow [(makeSomePosition 3 7)]]                   

                , testCase "move 2" $
                  posCoords pos2 @?= (7,6)    

                , testCase "outflanks 2" $
                  outflanks2 @?= [PosRow [(makeSomePosition 7 5)]]                        

                , testCase "move 3" $
                  posCoords pos3 @?= (8,6)           

                , testCase "outflanks 3" $
                  outflanks3 @?= [PosRow [(makeSomePosition 7 5)]]                                                                        
                ]   

        , testGroup "validMoves White board_Figure2" $
            let
                moves = validMoves White board_Figure2

                (Move _ (EmptySquare pos0 _) (Outflanks o0)) = moves !! 0
                (Move _ (EmptySquare pos1 _) (Outflanks o1)) = moves !! 1

                outflanks0 = map filledRowToPosRow o0
                outflanks1 = map filledRowToPosRow o1
            in
                [ testCase "2 moves" $ 
                  length moves @?= 2

                , testCase "move 0" $
                  posCoords pos0 @?= (4,2)

                , testCase "outflanks 0" $
                  outflanks0 @?= [PosRow [(makeSomePosition 5 3), (makeSomePosition 6 4)]] 

                , testCase "move 1" $
                  posCoords pos1 @?= (7,3)     

                , testCase "outflanks 1" $
                  outflanks1 @?= 
                      [ PosRow [(makeSomePosition 6 3), (makeSomePosition 5 3), (makeSomePosition 4 3)]
                      , PosRow [(makeSomePosition 7 4)]
                      , PosRow [(makeSomePosition 6 4), (makeSomePosition 5 5), (makeSomePosition 4 6)]
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
                        filledPositions White boardAfter @?= 
                            [ (makeSomePosition 3 3)
                            , (makeSomePosition 3 7)
                            , (makeSomePosition 4 2)
                            , (makeSomePosition 5 3)
                            , (makeSomePosition 6 4)
                            , (makeSomePosition 7 5)
                            ]

                    , testCase "black positions" $
                        filledPositions Black boardAfter @?= 
                            [ (makeSomePosition 4 3)
                            , (makeSomePosition 4 6)
                            , (makeSomePosition 5 5)
                            , (makeSomePosition 6 3)
                            , (makeSomePosition 7 4)
                            ]                                                                                   
                    ]  

            , testGroup "move1" $
                let
                    boardBefore = board_Figure2
                    moves = validMoves White boardBefore
                    move1 = moves !! 1
                    boardAfter = applyBoardMove move1 boardBefore
                in
                    [ testCase "white positions" $ 
                        filledPositions White boardAfter @?= 
                            [ (makeSomePosition 3 3)
                            , (makeSomePosition 3 7)
                            , (makeSomePosition 4 3)
                            , (makeSomePosition 4 6)
                            , (makeSomePosition 5 3)
                            , (makeSomePosition 5 5)
                            , (makeSomePosition 6 3)
                            , (makeSomePosition 6 4)
                            , (makeSomePosition 7 3)
                            , (makeSomePosition 7 4)
                            , (makeSomePosition 7 5)
                            ]

                    , testCase "black positions" $
                      filledPositions Black boardAfter @?= []                                                                                   
                    ]    
              ]
          ]

    , testGroup "module State" $ 
        let
            taggedState1 = Tagged_StartState makeStartState
            moves1 = actual_NextMoves_FromTaggedState taggedState1
            move1 = head moves1

            taggedState2 = applyMove move1 taggedState1
            moves2 = actual_NextMoves_FromTaggedState taggedState2
            move2 = head moves2

            taggedState3 = applyMove move2 taggedState2
            moves3 = actual_NextMoves_FromTaggedState taggedState3
            move3 = head moves3

            taggedState4 = applyMove move3 taggedState3          

            numberedMovesWithPos1 = movePosChoices moves1
            numberedMovesWithPos2 = movePosChoices moves2

            display1 = gameStateDisplay Nothing taggedState1
            display2 = gameStateDisplay (Just numberedMovesWithPos1) taggedState1
            display3 = gameStateDisplay Nothing taggedState2
            display4 = gameStateDisplay (Just numberedMovesWithPos2) taggedState2

            (b1, w1) = actual_BlackAndWhiteUnusedDiskCounts_FromTaggedState taggedState1
            (b2, w2) = actual_BlackAndWhiteUnusedDiskCounts_FromTaggedState taggedState2
            (b3, w3) = actual_BlackAndWhiteUnusedDiskCounts_FromTaggedState taggedState3

            (Tagged_MidState (MidState priorMove2 _ _)) = taggedState2 -- if not midstate (due to bug), then raise exception which is fine
            (Tagged_MidState (MidState priorMove3 _ _)) = taggedState3 -- if not midstate (due to bug), then raise exception which is fine
            (Tagged_MidState (MidState priorMove4 _ _)) = taggedState4 -- if not midstate (due to bug), then raise exception which is fine

            (c2, c3, c4) = (priorMoveColor priorMove2, priorMoveColor priorMove3, priorMoveColor priorMove4)
          in
              [ testCase "initial: gameStateDisplay Nothing" $ 
                display1 @?= "    A    B    C    D    E    F    G    H    \n1   .    .    .    .    .    .    .    .  \n\n2   .    .    .    .    .    .    .    .  \n\n3   .    .    .    .    .    .    .    .  \n\n4   .    .    .    o    x    .    .    .  \n\n5   .    .    .    x    o    .    .    .  \n\n6   .    .    .    .    .    .    .    .  \n\n7   .    .    .    .    .    .    .    .  \n\n8   .    .    .    .    .    .    .    .  \n\nAwaiting your first move...\n\nAvailable Disks\nBlack 32: x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x\nWhite 32: o o o o o o o o o o o o o o o o o o o o o o o o o o o o o o o o"
                
              , testCase "initial: gameStateDisplay (Just numberedMovesWithPos)" $ 
                display2 @?= "    A    B    C    D    E    F    G    H    \n1   .    .    .    .    .    .    .    .  \n\n2   .    .    .    .    .    .    .    .  \n\n3   .    .    .    2    .    .    .    .  \n\n4   .    .    1    o    x    .    .    .  \n\n5   .    .    .    x    o    4    .    .  \n\n6   .    .    .    .    3    .    .    .  \n\n7   .    .    .    .    .    .    .    .  \n\n8   .    .    .    .    .    .    .    .  \n\nAwaiting your first move...\n\nAvailable Disks\nBlack 32: x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x\nWhite 32: o o o o o o o o o o o o o o o o o o o o o o o o o o o o o o o o"

              , testCase "After move C4: gameStateDisplay Nothing" $ 
                display3 @?= "    A    B    C    D    E    F    G    H    \n1   .    .    .    .    .    .    .    .  \n\n2   .    .    .    .    .    .    .    .  \n\n3   .    .    .    .    .    .    .    .  \n\n4   .    .   =X=  -x-   x    .    .    .  \n\n5   .    .    .    x    o    .    .    .  \n\n6   .    .    .    .    .    .    .    .  \n\n7   .    .    .    .    .    .    .    .  \n\n8   .    .    .    .    .    .    .    .  \n\nBLACK moved to: C4\n\nAvailable Disks\nBlack 31: x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x\nWhite 32: o o o o o o o o o o o o o o o o o o o o o o o o o o o o o o o o"
  
              , testCase "After move C4: gameStateDisplay (Just numberedMovesWithPos)" $ 
                display4 @?= "    A    B    C    D    E    F    G    H    \n1   .    .    .    .    .    .    .    .  \n\n2   .    .    .    .    .    .    .    .  \n\n3   .    .    1    .    3    .    .    .  \n\n4   .    .   =X=  -x-   x    .    .    .  \n\n5   .    .    2    x    o    .    .    .  \n\n6   .    .    .    .    .    .    .    .  \n\n7   .    .    .    .    .    .    .    .  \n\n8   .    .    .    .    .    .    .    .  \n\nBLACK moved to: C4\n\nAvailable Disks\nBlack 31: x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x\nWhite 32: o o o o o o o o o o o o o o o o o o o o o o o o o o o o o o o o"

              , testCase "initial unused disk counts" $ 
                (b1, w1) @?= (32, 32) 

              , testCase "unused disk counts after 1st move (Black)" $ 
                (b2, w2) @?= (31, 32)   

              , testCase "unused disk counts after 2nd move (White)" $ 
                (b3, w3) @?= (31, 31)  

              , testCase "priorMoveColor for first 3 states after initial state" $ 
                (c2, c3, c4) @?= (Black, White, Black)    
              
              , testGroup "Black uses very last disk on first move (contrived)" $ 
                  let
                      startState@(StartState c n (CoreState b w board)) = makeStartState
                      (tb, tw) = (Tagged_BlackUnusedDiskCount b, Tagged_WhiteUnusedDiskCount w)
                      (nb, nw) = actual_BlackAndWhiteUnusedDiskCounts_FromTaggedState $ Tagged_StartState startState

                      (Tagged_BlackUnusedDiskCount b') = iterate decreaseByOne tb !! (nb - 1) 
                      (Tagged_WhiteUnusedDiskCount w') = iterate decreaseByOne tw !! nw 

                      taggedState1 = Tagged_StartState $ StartState c n $ CoreState b' w' board
                      moves1 = actual_NextMoves_FromTaggedState taggedState1

                      (Tagged_EndState (EndState _ endReason _)) = applyMove (head moves1) taggedState1 -- if it's not an end-state (due to bug), exception will be raised from pattern-matching, which is part of the test
                  in
                      [ testCase "first move results in: Tagged_EndState, NoUnusedDisksForBot" $ 
                        endReason @?= NoUnusedDisksForBoth
                      ]

              , testGroup "Black on first move is confronted with full board (contrived)" $
                  let
                      (StartState c n (CoreState b w board)) = makeStartState

                      board'= boardFromConfig [ (White,(makeSomePosition i j))  | i <- [1..(boardSize)], j <- [1..(boardSize-1)] ]

                      taggedState1 = Tagged_StartState $ StartState c n $ CoreState b w board'
                      move = Move Black (head $ emptySquares board') $ Outflanks []

                      (Tagged_EndState (EndState _ endReason _)) = applyMove (head moves1) taggedState1 -- if it's not an end-state (due to bug), exception will be raised from pattern-matching, which is part of the test
                  in
                      [ testCase "first move results in: Tagged_EndState, NoValidMoves" $ 
                        endReason @?= NoValidMoves
                      ]

              , testGroup "White with no disks for his first move, is given one by Black (contrived)" $
                  let
                      startState@(StartState c n (CoreState b w board)) = makeStartState
                      (tb, tw) = (Tagged_BlackUnusedDiskCount b, Tagged_WhiteUnusedDiskCount w)
                      (nb, nw) = actual_BlackAndWhiteUnusedDiskCounts_FromTaggedState $ Tagged_StartState startState

                      (Tagged_WhiteUnusedDiskCount w') = iterate decreaseByOne tw !! nw

                      taggedState1 = Tagged_StartState $ StartState c n $ CoreState b w' board
                      moves1 = actual_NextMoves_FromTaggedState taggedState1

                      taggedState2 = applyMove (head moves1) taggedState1
                      moves2 = actual_NextMoves_FromTaggedState taggedState2
          
                      taggedState3 = applyMove (head moves2) taggedState2

                      (b1, w1) = actual_BlackAndWhiteUnusedDiskCounts_FromTaggedState taggedState1
                      (b2, w2) = actual_BlackAndWhiteUnusedDiskCounts_FromTaggedState taggedState2
                  in
                      [ testCase "initial disk counts" $  
                        (b1, w1) @?= (32, 0) 
    
                      , testCase "Black after using disk for his first move, then transfers another to White -- prior to White's first move" $ 
                        (b2, w2) @?= (30, 1)   
                      ]   
              -------------------------------------------------------------------------------------------                        
              -- to test this section, temp uncomment out -- but need to expose normally unexposed Board.flipAt       
              -------------------------------------------------------------------------------------------  
              -- , testGroup "Flip initial disks in progressive amounts, then boardWithFlipCountDisplay (contrived)" $
              --     let
              --         board = board_FromTaggedState $ Tagged_StartState makeStartState

              --         f :: Position -> Board -> Board
              --         f = \ pos board -> flipAt (boardAt board pos) board
              
              --         board' = filledSquares board
              --             & map (toPos . Tagged_FilledSquare)
              --             & zip [(1 :: Int)..]
              --             & foldl' (\ acc ((i, pos)) -> iterate (f pos) acc !! i ) board
              
              --         flippedCounts = filledSquares board'
              --             & map (\ x -> flipCount $ diskFrom x)
              --     in
              --         [ testCase "(verify test logic)" $ 
              --           flippedCounts @?= [1,2,3,4]

              --         , testCase "boardWithFlipCountDisplay" $
              --           boardWithFlipCountDisplay board' @?= "    A    B    C    D    E    F    G    H    \n1   .    .    .    .    .    .    .    .  \n\n2   .    .    .    .    .    .    .    .  \n\n3   .    .    .    .    .    .    .    .  \n\n4   .    .    .    1    2    .    .    .  \n\n5   .    .    .    3    4    .    .    .  \n\n6   .    .    .    .    .    .    .    .  \n\n7   .    .    .    .    .    .    .    .  \n\n8   .    .    .    .    .    .    .    .  "
              --         ]
              -------------------------------------------------------------------------------------------  
              ]
    ]

    