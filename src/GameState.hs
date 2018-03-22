module GameState
    ( GameState(..)
    , makeStartLifecycleGameState
    )
    where
 
import qualified Data.Map.Strict as Map ( (!) )

import Disk ( Color(..), toggleColor, iconChar )
import Board ( Board, Move(..), applyMove, initialBoard, numSquaresColored, validMoves, board_DisplayString ) 
import UnusedDiskCount ( BlackUnusedDiskCount, WhiteUnusedDiskCount, All_UnusedDiskCount(..), initialBlackUnusedDiskCount, initialWhiteUnusedDiskCount, decreaseByOne, isZeroCount, transferDiskTo, decreaseByOneFor, countFrom )
import SquareCount ( BlackSquareCount, WhiteSquareCount, All_SquareCount(..), makeBlackSquareCount, makeWhiteSquareCount, countFrom )

data GameState = GameState NextToMove PossibleMoves BlackUnusedDiskCount WhiteUnusedDiskCount Board deriving (Eq, Show)

newtype NextToMove = NextToMove Color deriving (Eq, Show)

newtype PossibleMoves = PossibleMoves [Move] deriving (Eq, Show)

newtype StartLifecycleGameState = StartLifecycleGameState GameState deriving (Eq, Show)

newtype MidLifecycleGameState   = MidLifecycleGameState GameState deriving (Eq, Show)

data EndLifecycleGameState   = EndLifecycleGameState EndReason GameState deriving (Eq, Show)

data EndReason
    = NoUnusedDisksForBoth
    | NoValidMoves
    -- todo  | more?
        deriving (Eq, Show)

data GameSummary = GameSummary EndReason BlackSquareCount WhiteSquareCount deriving (Eq, Show)

data All_LifecycleGameState
    = StartLifecycle StartLifecycleGameState
    | MidLifecycle   MidLifecycleGameState
    | EndLifecycle   EndLifecycleGameState
        deriving (Eq, Show)

data Winner
    = WinnerColor Color
    | Tie -- (is this theoretically possible?)
        deriving (Eq, Show)


initialNextToMove :: NextToMove
initialNextToMove =
    NextToMove Black


makeStartLifecycleGameState :: StartLifecycleGameState
makeStartLifecycleGameState =
    let
        nextToMove = initialNextToMove
        (NextToMove color) = nextToMove
        board = initialBoard
        moves = PossibleMoves $ validMoves color board
    in
        StartLifecycleGameState $ 
            GameState 
                nextToMove
                moves
                initialBlackUnusedDiskCount 
                initialWhiteUnusedDiskCount 
                board


-- startLifecycle :: StartLifecycleGameState -> MidLifecycleGameState
-- startLifecycle (StartLifecycleGameState ((NextToMove Color) bCount wCount board)) =
-- used monad stuff here?

  
isZeroUnusedDiskCount :: Color -> GameState -> Bool
isZeroUnusedDiskCount color (GameState _ _ b w _) =
    case color of
        Black -> isZeroCount $ BlackUnused b
        White -> isZeroCount $ WhiteUnused w


isZeroUnusedDiskCount_Tagged :: Color -> All_LifecycleGameState -> Bool
isZeroUnusedDiskCount_Tagged color tagged =
    case tagged of
        StartLifecycle _ -> False
        MidLifecycle (MidLifecycleGameState g)   -> isZeroUnusedDiskCount color g
        EndLifecycle (EndLifecycleGameState _ g) -> isZeroUnusedDiskCount color g


continueLifecycle :: MidLifecycleGameState -> EndLifecycleGameState
continueLifecycle mg@(MidLifecycleGameState g@(GameState n@(NextToMove color) p@(PossibleMoves moves) bCount wCount board)) =
    let
        oppColor = toggleColor color

        isZeroUnused = isZeroUnusedDiskCount_Tagged color $ MidLifecycle mg
        isZeroUnusedOpp = isZeroUnusedDiskCount_Tagged oppColor $ MidLifecycle mg

        end_NoUnusedDisksForBoth = EndLifecycleGameState NoUnusedDisksForBoth g
        continue_Opp = \ b w someBoard -> continueLifecycle $ MidLifecycleGameState $ GameState (NextToMove oppColor) (PossibleMoves $ validMoves oppColor someBoard) b w someBoard
        continue_Same = \ b w -> continueLifecycle $ MidLifecycleGameState $ GameState n p b w board 
    in
        if isZeroUnused then
            if isZeroUnusedOpp then -- Rule 10: When it is no longer possible for either player to move, the game is over.
                end_NoUnusedDisksForBoth
            else if length moves > 0 then -- Rule 9: If a player runs out of disks, but still has the opportunity to outflank an opposing disk on their turn, the opponent must give the player a disk to use. This can happen as many times as the player needs and can use a disk.
                let
                    m = transferDiskTo color bCount wCount

                    (BlackUnused bCount') = m Map.! Black
                    (WhiteUnused wCount') = m Map.! White
                in
                    continue_Same bCount' wCount'
            else
                continue_Opp bCount wCount board
        else
            let
                m = decreaseByOneFor color bCount wCount

                (BlackUnused bCount') = m Map.! Black
                (WhiteUnused wCount') = m Map.! White
            in
                --  somehow make move which gives board' ---------------------------------------------------------------
                end_NoUnusedDisksForBoth --continue_Opp bCount' wCount' board'


winner :: GameSummary -> Winner
winner (GameSummary _ b w) =  
    -- Rule 10: Disks are counted and the player with the majority of their color showing is the winner.
    let
        nBlack = SquareCount.countFrom $ Tagged_BlackSquareCount b
        nWhite = SquareCount.countFrom $ Tagged_WhiteSquareCount w
    in
        if nBlack > nWhite then
            WinnerColor Black
        else if nWhite > nBlack then
            WinnerColor White 
        else
            Tie


gameSummary :: EndLifecycleGameState -> GameSummary
gameSummary (EndLifecycleGameState reason (GameState _ _ _ _ board)) =
    let
        m = numSquaresColored board

        b = makeBlackSquareCount $ m Map.! Black
        w = makeWhiteSquareCount $ m Map.! White
    in
        GameSummary reason b  w


-- -- WRONG (try to decrease rule9) then move
-- blackMoved :: Board -> GameState -> GameState
-- blackMoved board (GameState b w _) =
--     let
--        (Tagged_BlackUnusedDiskCount b') = decreaseByOne $ Tagged_BlackUnusedDiskCount b
--     in
--         GameState b' w board


-- -- WRONG (try to decrease rule9) then move
-- whiteMoved :: Board -> GameState -> GameState
-- whiteMoved board (GameState b w _) =
--     let
--         (Tagged_WhiteUnusedDiskCount w') = decreaseByOne $ Tagged_WhiteUnusedDiskCount w
--     in
--         GameState b w' board


-- playMove :: Move -> GameState -> GameState
-- playMove move g@(GameState _ _ board) =
--     -- NEED TO DEAL WITH DISKCOUNT FIRST< TO SEE IF DEPLETED AND RULE 9
--     let
--         board' = applyMove move board
--     in
--         case _color move of 
--             Black -> blackMoved board' g
--             White -> whiteMoved board' g
         

gameState_DisplayString :: GameState -> String
gameState_DisplayString (GameState _ _ b w board) =
    let
        blackUnusedCount = UnusedDiskCount.countFrom $ BlackUnused b
        whiteUnusedCount = UnusedDiskCount.countFrom $ WhiteUnused w

        blackUnused = "Black: " ++ (replicate blackUnusedCount $ iconChar Black)
        whiteUnused = "White: " ++ (replicate blackUnusedCount $ iconChar White)

        header = 
            blackUnused ++ "\n" ++
            whiteUnused ++ "\n" ++
            "\n" 
    in
        header ++ board_DisplayString board