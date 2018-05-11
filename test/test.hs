-- http://documentup.com/feuerbach/tasty
 
import Test.Tasty
import Test.Tasty.SmallCheck as SC
import Test.Tasty.QuickCheck as QC
import Test.Tasty.HUnit
 
import Data.Function ( (&) )
import Data.List ( foldl' )
import Data.Maybe ( fromJust )
import qualified Data.List.NonEmpty as NE ( filter, fromList, last )
 
import Board ( Board, EmptySquare(..), FilledSquare, Move(..), Outflanks(..), FilledRow(..), Tagged_Square(..), emptySquares, initialBoard, validMoves, boardFromConfig, toPos, applyBoardMove, filledPositions, movePosChoices, diskFrom, filledSquares, boardAt) --, flipAt)
import Position ( PosRow(..), radiatingPosRows )
import Disk ( Color(..), flipCount )
import State ( CoreState(..), StartState(..), MidState(..), EndState(..), Tagged_State(..), MidStatus(..), EndStatus(..), makeStartState, priorMoveColor, actual_NextMoves_FromTaggedState, actual_UnusedDiskCounts_FromTaggedState_BlackWhite, board_FromTaggedState, nextMoveColor_FromTaggedState, makeHistory, applyMoveOnHistory, undoHistoryOnce, isForfeitTurn )
import UnusedDiskCount ( Tagged_UnusedDiskCount(..), countFrom, decreaseByOne )
import BoardSize ( boardSize )
import Position ( Position, makeValidPosition, posCoords )
import Display ( boardDisplay, boardWithFlipCountDisplay, gameStateDisplay, showMoveNumInEmptySquare )
import BlackWhite ( blacksWhites )
import Lib ( mapTakeWhile )

main = defaultMain tests

tests :: TestTree
tests = testGroup "Tests" [unitTests] 

filledRowToPosRow :: FilledRow -> PosRow
filledRowToPosRow (FilledRow xs) =
  PosRow $ xs
      & map (\ x -> toPos $ Tagged_FilledSquare x)


board_Figure2 :: Board 
board_Figure2 =
    -- Figure 2, from page 2 of http://www.boardgamecapital.com/game_rules/othello.pdf
    boardFromConfig 
        [ (White, (makeValidPosition 3 3))
        , (White, (makeValidPosition 3 7))
        , (White, (makeValidPosition 7 5))
        , (Black, (makeValidPosition 4 3))
        , (Black, (makeValidPosition 4 6))
        , (Black, (makeValidPosition 5 3))
        , (Black, (makeValidPosition 5 5))
        , (Black, (makeValidPosition 6 3))
        , (Black, (makeValidPosition 6 4))
        , (Black, (makeValidPosition 7 4))
        ]


boardWithValidMovesDisplay :: [(Int, Position)] -> Board -> String
boardWithValidMovesDisplay showMoves board = 
    boardDisplay 
        (Just $ showMoveNumInEmptySquare showMoves)
        Nothing
        board


unitTests = testGroup "Unit tests" $
    [ testGroup "module Lib" $
        [ testCase "mapTakeWhile" $
          (mapTakeWhile (*2) (< 10) [1,2,3,4,5,6,7]) @?= [2,4,6,8]
        ]

    , testGroup "module Position" $
        [ testCase "radiatingPosRows (Position 1 1)" $
            radiatingPosRows (makeValidPosition 1 1)  @?= 
                [ PosRow 
                    [ (makeValidPosition 2 1)
                    , (makeValidPosition 3 1)
                    , (makeValidPosition 4 1)
                    , (makeValidPosition 5 1)
                    , (makeValidPosition 6 1) 
                    , (makeValidPosition 7 1)
                    , (makeValidPosition 8 1)
                    ]
                , PosRow 
                    [ (makeValidPosition 1 2)
                    , (makeValidPosition 1 3)
                    , (makeValidPosition 1 4)
                    , (makeValidPosition 1 5)
                    , (makeValidPosition 1 6)
                    , (makeValidPosition 1 7)
                    , (makeValidPosition 1 8)
                    ]
                , PosRow 
                    [ (makeValidPosition 2 2)
                    , (makeValidPosition 3 3)
                    , (makeValidPosition 4 4)
                    , (makeValidPosition 5 5)
                    , (makeValidPosition 6 6)
                    , (makeValidPosition 7 7)
                    , (makeValidPosition 8 8)
                    ]
                ]

        , testCase "radiatingPosRows (Position 1 8)" $
            radiatingPosRows (makeValidPosition 1 8)  @?= 
              [ PosRow 
                  [ (makeValidPosition 2 8)
                  , (makeValidPosition 3 8)
                  , (makeValidPosition 4 8)
                  , (makeValidPosition 5 8)
                  , (makeValidPosition 6 8) 
                  , (makeValidPosition 7 8)
                  , (makeValidPosition 8 8)
                  ]
              , PosRow 
                  [ (makeValidPosition 1 7)
                  , (makeValidPosition 1 6)
                  , (makeValidPosition 1 5)
                  , (makeValidPosition 1 4)
                  , (makeValidPosition 1 3)
                  , (makeValidPosition 1 2)
                  , (makeValidPosition 1 1)
                  ]
              , PosRow 
                  [ (makeValidPosition 2 7)
                  , (makeValidPosition 3 6)
                  , (makeValidPosition 4 5)
                  , (makeValidPosition 5 4)
                  , (makeValidPosition 6 3)
                  , (makeValidPosition 7 2)
                  , (makeValidPosition 8 1)
                  ]
              ]

        , testCase "radiatingPosRows (Position 8 8)" $
            radiatingPosRows (makeValidPosition 8 8)  @?= 
              [ PosRow 
                  [ (makeValidPosition 7 8)
                  , (makeValidPosition 6 8)
                  , (makeValidPosition 5 8)
                  , (makeValidPosition 4 8)
                  , (makeValidPosition 3 8) 
                  , (makeValidPosition 2 8)
                  , (makeValidPosition 1 8)
                  ]
              , PosRow 
                  [ (makeValidPosition 8 7)
                  , (makeValidPosition 8 6)
                  , (makeValidPosition 8 5)
                  , (makeValidPosition 8 4)
                  , (makeValidPosition 8 3)
                  , (makeValidPosition 8 2)
                  , (makeValidPosition 8 1)
                  ]
              , PosRow 
                  [ (makeValidPosition 7 7)
                  , (makeValidPosition 6 6)
                  , (makeValidPosition 5 5)
                  , (makeValidPosition 4 4)
                  , (makeValidPosition 3 3)
                  , (makeValidPosition 2 2)
                  , (makeValidPosition 1 1)
                  ]
              ]

        , testCase "radiatingPosRows (Position 8 1)" $
            radiatingPosRows (makeValidPosition 8 1)  @?= 
              [ PosRow 
                  [ (makeValidPosition 7 1)
                  , (makeValidPosition 6 1)
                  , (makeValidPosition 5 1)
                  , (makeValidPosition 4 1)
                  , (makeValidPosition 3 1) 
                  , (makeValidPosition 2 1)
                  , (makeValidPosition 1 1)
                  ]
              , PosRow 
                  [ (makeValidPosition 8 2)
                  , (makeValidPosition 8 3)
                  , (makeValidPosition 8 4)
                  , (makeValidPosition 8 5)
                  , (makeValidPosition 8 6)
                  , (makeValidPosition 8 7)
                  , (makeValidPosition 8 8)
                  ]
              , PosRow 
                  [ (makeValidPosition 7 2)
                  , (makeValidPosition 6 3)
                  , (makeValidPosition 5 4)
                  , (makeValidPosition 4 5)
                  , (makeValidPosition 3 6)
                  , (makeValidPosition 2 7)
                  , (makeValidPosition 1 8)
                  ]
              ]

        , testCase "radiatingPosRows Black (Position 5 6)" $
            radiatingPosRows (makeValidPosition 5 6)  @?= 
              [ PosRow 
                  [ (makeValidPosition 4 6)
                  , (makeValidPosition 3 6)
                  , (makeValidPosition 2 6)
                  , (makeValidPosition 1 6)
                  ]
              , PosRow 
                  [ (makeValidPosition 6 6)
                  , (makeValidPosition 7 6)
                  , (makeValidPosition 8 6)
                  ]
              , PosRow 
                  [ (makeValidPosition 5 7)
                  , (makeValidPosition 5 8)
                  ]
              , PosRow 
                  [ (makeValidPosition 5 5)
                  , (makeValidPosition 5 4)
                  , (makeValidPosition 5 3)
                  , (makeValidPosition 5 2)
                  , (makeValidPosition 5 1)
                  ]
              , PosRow 
                  [ (makeValidPosition 4 7)
                  , (makeValidPosition 3 8)
                  ]
              , PosRow 
                  [ (makeValidPosition 4 5)
                  , (makeValidPosition 3 4)
                  , (makeValidPosition 2 3)
                  , (makeValidPosition 1 2)
                  ]
              , PosRow 
                  [ (makeValidPosition 6 7)
                  , (makeValidPosition 7 8)
                  ]
              , PosRow 
                  [ (makeValidPosition 6 5)
                  , (makeValidPosition 7 4)
                  , (makeValidPosition 8 3)
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
                  outflanks0 @?= [PosRow [(makeValidPosition 4 4)]] 

                , testCase "move 1" $
                  posCoords pos1 @?= (3,4)     

                , testCase "outflanks 1" $
                  outflanks1 @?= [PosRow [(makeValidPosition 4 4)]]                   

                , testCase "move 2" $
                  posCoords pos2 @?= (6,5)    

                , testCase "outflanks 2" $
                  outflanks2 @?= [PosRow [(makeValidPosition 5 5)]]                        

                , testCase "move 3" $
                  posCoords pos3 @?= (5,6)           

                , testCase "outflanks 3" $
                  outflanks3 @?= [PosRow [(makeValidPosition 5 5)]]                                                                        
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
                  outflanks0 @?= [PosRow [(makeValidPosition 3 3)]] 

                , testCase "move 1" $
                  posCoords pos1 @?= (2,8)     

                , testCase "outflanks 1" $
                  outflanks1 @?= [PosRow [(makeValidPosition 3 7)]]                   

                , testCase "move 2" $
                  posCoords pos2 @?= (7,6)    

                , testCase "outflanks 2" $
                  outflanks2 @?= [PosRow [(makeValidPosition 7 5)]]                        

                , testCase "move 3" $
                  posCoords pos3 @?= (8,6)           

                , testCase "outflanks 3" $
                  outflanks3 @?= [PosRow [(makeValidPosition 7 5)]]                                                                        
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
                  outflanks0 @?= [PosRow [(makeValidPosition 5 3), (makeValidPosition 6 4)]] 

                , testCase "move 1" $
                  posCoords pos1 @?= (7,3)     

                , testCase "outflanks 1" $
                  outflanks1 @?= 
                      [ PosRow [(makeValidPosition 6 3), (makeValidPosition 5 3), (makeValidPosition 4 3)]
                      , PosRow [(makeValidPosition 7 4)]
                      , PosRow [(makeValidPosition 6 4), (makeValidPosition 5 5), (makeValidPosition 4 6)]
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
                            [ (makeValidPosition 3 3)
                            , (makeValidPosition 3 7)
                            , (makeValidPosition 4 2)
                            , (makeValidPosition 5 3)
                            , (makeValidPosition 6 4)
                            , (makeValidPosition 7 5)
                            ]

                    , testCase "black positions" $
                        filledPositions Black boardAfter @?= 
                            [ (makeValidPosition 4 3)
                            , (makeValidPosition 4 6)
                            , (makeValidPosition 5 5)
                            , (makeValidPosition 6 3)
                            , (makeValidPosition 7 4)
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
                            [ (makeValidPosition 3 3)
                            , (makeValidPosition 3 7)
                            , (makeValidPosition 4 3)
                            , (makeValidPosition 4 6)
                            , (makeValidPosition 5 3)
                            , (makeValidPosition 5 5)
                            , (makeValidPosition 6 3)
                            , (makeValidPosition 6 4)
                            , (makeValidPosition 7 3)
                            , (makeValidPosition 7 4)
                            , (makeValidPosition 7 5)
                            ]

                    , testCase "black positions" $
                      filledPositions Black boardAfter @?= []                                                                                   
                    ]    
              ]
          ]

    , testGroup "module State" $ 
        let
            history1 = makeHistory

            taggedState1 = NE.last history1
            moves1 = actual_NextMoves_FromTaggedState taggedState1
            move1 = head moves1
            history2 = applyMoveOnHistory move1 history1 

            taggedState2 = NE.last history2
            moves2 = actual_NextMoves_FromTaggedState taggedState2
            move2 = head moves2
            history3 = applyMoveOnHistory move2 history2     

            taggedState3 = NE.last history3
            moves3 = actual_NextMoves_FromTaggedState taggedState3
            move3 = head moves3
            history4 = applyMoveOnHistory move3 history3 

            taggedState4 = NE.last history4         

            numberedMovesWithPos1 = movePosChoices moves1
            numberedMovesWithPos2 = movePosChoices moves2

            display1 = gameStateDisplay Nothing taggedState1
            display2 = gameStateDisplay (Just numberedMovesWithPos1) taggedState1
            display3 = gameStateDisplay Nothing taggedState2
            display4 = gameStateDisplay (Just numberedMovesWithPos2) taggedState2

            (b1, w1) = blacksWhites $ actual_UnusedDiskCounts_FromTaggedState_BlackWhite taggedState1
            (b2, w2) = blacksWhites $ actual_UnusedDiskCounts_FromTaggedState_BlackWhite taggedState2
            (b3, w3) = blacksWhites $ actual_UnusedDiskCounts_FromTaggedState_BlackWhite taggedState3

            (Tagged_MidState (MidState priorMove2 _ _ _)) = taggedState2 -- if pattern match fails (due to bug), then raise exception which is fine
            (Tagged_MidState (MidState priorMove3 _ _ _)) = taggedState3 -- if pattern match fails (due to bug), then raise exception which is fine
            (Tagged_MidState (MidState priorMove4 _ _ _)) = taggedState4 -- if pattern match fails (due to bug), then raise exception which is fine

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
                      (nb, nw) = blacksWhites $ actual_UnusedDiskCounts_FromTaggedState_BlackWhite $ Tagged_StartState startState

                      (Tagged_BlackUnusedDiskCount b') = iterate decreaseByOne tb !! (nb - 1) 
                      (Tagged_WhiteUnusedDiskCount w') = iterate decreaseByOne tw !! nw 

                      taggedState1 = Tagged_StartState $ StartState c n $ CoreState b' w' board
                      history1 = NE.fromList $ [taggedState1]
                      moves1 = actual_NextMoves_FromTaggedState taggedState1

                      history2 = applyMoveOnHistory (head moves1) history1 
                      (Tagged_EndState (EndState _ endReason _)) = NE.last history2 -- if pattern match fails (due to bug), exception will be raised from pattern-matching, which is part of the test
                  in
                      [ testCase "first move results in: Tagged_EndState, NoUnusedDisksForBot" $ 
                        endReason @?= NoUnusedDisksForBoth
                      ]

              , testGroup "Black on first move is confronted with full White board -- except for last column which is blank (contrived)" $
                  let
                      (StartState c n (CoreState b w board)) = makeStartState

                      board' = boardFromConfig  [ (White,(makeValidPosition i j))  | i <- [1..(boardSize)], j <- [1..(boardSize-1)] ]

                      taggedState1 = Tagged_StartState $ StartState c n $ CoreState b w board'
                      history1 = NE.fromList $ [taggedState1]
                      move = Move Black (head $ emptySquares board') $ Outflanks []

                      history2 = applyMoveOnHistory move history1 
                      (Tagged_EndState (EndState _ endStatus _)) = NE.last history2 -- if pattern match fails (due to bug), exception will be raised from pattern-matching, which is part of the test
                  in
                      [ testCase "first move results in: Tagged_EndState, NoValidMoves" $ 
                        endStatus @?= NoValidMoves
                      ]

              , testGroup "Black on first move is confronted with full White board -- except for (1,1) which is Black, and last column which is blank (contrived)" $
                    let
                        (StartState c n (CoreState b w board)) = makeStartState

                        board' = boardFromConfig $ [(Black, (makeValidPosition 1 1))] ++ tail [ (White,(makeValidPosition i j))  | i <- [1..(boardSize)], j <- [1..(boardSize-1)] ]

                        taggedState1 = Tagged_StartState $ StartState c n $ CoreState b w board'
                        history1 = NE.fromList $ [taggedState1]
                        moves1 = actual_NextMoves_FromTaggedState taggedState1

                        history2 = applyMoveOnHistory (head moves1) history1 
                        taggedState2@(Tagged_MidState (MidState _ midStatus _ _)) = NE.last history2 -- if pattern match fails (due to bug), exception will be raised from pattern-matching, which is part of the test
                    in
                        [ testCase "First move results in: Tagged_MidState, ForfeitTurn_Rule2" $ 
                            midStatus @?= ForfeitTurn_Rule2

                        , testCase "Second move color is also Black" $ 
                            nextMoveColor_FromTaggedState taggedState2 @?= Just Black
                        ]
    
              , testGroup "White with no disks for his first move, is given one by Black (contrived)" $
                  let
                      startState@(StartState c n (CoreState b w board)) = makeStartState
                      (tb, tw) = (Tagged_BlackUnusedDiskCount b, Tagged_WhiteUnusedDiskCount w)
                      (nb, nw) = blacksWhites $ actual_UnusedDiskCounts_FromTaggedState_BlackWhite $ Tagged_StartState startState

                      (Tagged_WhiteUnusedDiskCount w') = iterate decreaseByOne tw !! nw

                      taggedState1 = Tagged_StartState $ StartState c n $ CoreState b w' board
                      history1 = NE.fromList $ [taggedState1]
                      moves1 = actual_NextMoves_FromTaggedState taggedState1

                      history2 = applyMoveOnHistory (head moves1) history1 
                      taggedState2@(Tagged_MidState (MidState _ midStatus2 _ _)) = NE.last history2
                      moves2 = actual_NextMoves_FromTaggedState taggedState2
          
                      history3 = applyMoveOnHistory (head moves2) history2
                      taggedState3@(Tagged_MidState (MidState _ midStatus3 _ _)) = NE.last history3

                      (b1, w1) = blacksWhites $ actual_UnusedDiskCounts_FromTaggedState_BlackWhite taggedState1
                      (b2, w2) = blacksWhites $ actual_UnusedDiskCounts_FromTaggedState_BlackWhite taggedState2
                  in
                      [ testCase "initial disk counts" $  
                        (b1, w1) @?= (32, 0) 
    
                      , testCase "Black after using disk for his first move, then transfers another to White -- prior to White's first move" $ 
                        (b2, w2) @?= (30, 1)   

                      , testCase "Go from TransferDisk_Rule9 to Normal" $ 
                        (midStatus2, midStatus3) @?= (TransferDisk_Rule9, Normal)   
                      ]

              , testGroup "Undo" $
                  [ testGroup "Actual game (not contrived)" $
                      let
                          history1 = makeHistory

                          taggedState1 = NE.last history1
                          move1 = head $ actual_NextMoves_FromTaggedState taggedState1 -- black C4
                          history2 = applyMoveOnHistory move1 history1 

                          taggedState2 = NE.last history2
                          move2 = head $ actual_NextMoves_FromTaggedState taggedState2 -- white C3
                          history3 = applyMoveOnHistory move2 history2     
                          
                          taggedState3 = NE.last history3
                          move3 = head $ actual_NextMoves_FromTaggedState taggedState3 -- black C2
                          history4 = applyMoveOnHistory move3 history3    
                          
                          taggedState4 = NE.last history4
                          move4 = head $ actual_NextMoves_FromTaggedState taggedState4 -- white B2
                          history5 = applyMoveOnHistory move4 history4    
                          
                          taggedState5 = NE.last history5
                          move5 = head $ actual_NextMoves_FromTaggedState taggedState5 -- black A2
                          history6 = applyMoveOnHistory move5 history5 

                          taggedState6 = NE.last history6
                          move6 = head $ actual_NextMoves_FromTaggedState taggedState6 -- white A1
                          history7 = applyMoveOnHistory move6 history6    
                          
                          taggedState7 = NE.last history7
                          move7 = head $ actual_NextMoves_FromTaggedState taggedState7 -- black D3
                          history8 = applyMoveOnHistory move7 history7    
                          
                          taggedState8 = NE.last history8
                          move8 = head $ actual_NextMoves_FromTaggedState taggedState8 -- white A3
                          history9 = applyMoveOnHistory move8 history8  
                          
                          taggedState9 = NE.last history9
                          move9 = head $ actual_NextMoves_FromTaggedState taggedState9 -- black B3
                          history10 = applyMoveOnHistory move9 history9 

                          taggedState10 = NE.last history10
                          move10 = head $ actual_NextMoves_FromTaggedState taggedState10 -- white D2
                          history11 = applyMoveOnHistory move10 history10     
                          
                          taggedState11 = NE.last history11
                          move11 = head $ actual_NextMoves_FromTaggedState taggedState11 -- black B1
                          history12 = applyMoveOnHistory move11 history11    
                          
                          taggedState12 = NE.last history12
                          move12 = head $ actual_NextMoves_FromTaggedState taggedState12 -- white C1
                          history13 = applyMoveOnHistory move12 history12  
                          
                          taggedState13 = NE.last history13
                          move13 = head $ actual_NextMoves_FromTaggedState taggedState13 -- black D1
                          history14 = applyMoveOnHistory move13 history13

                          taggedState14 = NE.last history14
                          move14 = head $ actual_NextMoves_FromTaggedState taggedState14 -- white E1
                          history15 = applyMoveOnHistory move14 history14     
                          
                          taggedState15 = NE.last history15
                          move15 = head $ actual_NextMoves_FromTaggedState taggedState15 -- black E6
                          history16 = applyMoveOnHistory move15 history15    
                          
                          taggedState16 = NE.last history16
                          move16 = head $ actual_NextMoves_FromTaggedState taggedState16 -- white E2
                          history17 = applyMoveOnHistory move16 history16  
                          
                          taggedState17 = NE.last history17
                          move17 = head $ actual_NextMoves_FromTaggedState taggedState17 -- black F1
                          history18 = applyMoveOnHistory move17 history17

                          taggedState18 = NE.last history18
                          move18 = head $ actual_NextMoves_FromTaggedState taggedState18 -- white F2
                          history19 = applyMoveOnHistory move18 history18     
                          
                          taggedState19 = NE.last history19
                          move19 = head $ actual_NextMoves_FromTaggedState taggedState19 -- black F3
                          history20 = applyMoveOnHistory move19 history19    
                          
                          taggedState20 = NE.last history20
                          move20 = head $ actual_NextMoves_FromTaggedState taggedState20 -- white G1
                          history21 = applyMoveOnHistory move20 history20
                          
                          taggedState21 = NE.last history21                              -- forfeit
                          move21 = head $ actual_NextMoves_FromTaggedState taggedState21 -- white E3 
                          history22 = applyMoveOnHistory move21 history21

                          taggedState22 = NE.last history22                              -- forfeit
                          move22 = head $ actual_NextMoves_FromTaggedState taggedState22 -- white F4
                          history23 = applyMoveOnHistory move22 history22     
                          
                          taggedState23 = NE.last history23
                          move23 = head $ actual_NextMoves_FromTaggedState taggedState23 -- black G2
                          history24 = applyMoveOnHistory move23 history23    
                          
                          taggedState24 = NE.last history24
                          move24 = head $ actual_NextMoves_FromTaggedState taggedState24 -- white G3
                          history25 = applyMoveOnHistory move24 history24  
                          
                          taggedState25 = NE.last history25
                          move25 = head $ actual_NextMoves_FromTaggedState taggedState25 -- black H1
                          history26 = applyMoveOnHistory move25 history25                           
                      in
                          [ testCase "undo history1" $ 
                              let
                                  undone = undoHistoryOnce history1
                              in
                                  undone @?= Nothing

                          , testCase "undo history2" $ 
                              let
                                  undone = undoHistoryOnce history2
                              in
                                  undone @?= Nothing     

                          , testCase "undo history3" $ 
                              let
                                  undone = fromJust $ undoHistoryOnce history3
                              in
                                  undone @?= history1  

                          , testCase "undo history4" $ 
                              let
                                  undone = fromJust $ undoHistoryOnce history4
                              in
                                  undone @?= history2      

                          , testCase "undo history5" $ 
                              let
                                  undone = fromJust $ undoHistoryOnce history5
                              in
                                  undone @?= history3   

                          , testCase "undo history21" $ 
                              let
                                  undone = fromJust $ undoHistoryOnce history21
                              in
                                  undone @?= history20    

                          , testCase "undo history22" $ 
                              let
                                  undone = fromJust $ undoHistoryOnce history22
                              in
                                  undone @?= history21    

                          , testCase "undo history23" $ 
                              let
                                  undone = fromJust $ undoHistoryOnce history23
                              in
                                  undone @?= history19  

                          , testCase "undo history24" $ 
                              let
                                  undone = fromJust $ undoHistoryOnce history24
                              in
                                  undone @?= history22     

                          , testCase "undo history25" $ 
                              let
                                  undone = fromJust $ undoHistoryOnce history25
                              in
                                  undone @?= history23   
                                  
                          , testCase "forfeits" $ 
                              NE.filter isForfeitTurn history26 @?= [taggedState21, taggedState22]                                                                                                                                                                                                                                                                                 
                          ]
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

    