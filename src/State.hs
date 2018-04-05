{-# LANGUAGE InstanceSigs #-}
 
module State
    ( CoreState(..)
    , StartState(..)
    , MidState(..)
    , EndState(..)
    , Tagged_State(..)
    , PriorMove(..)
    , NextMoves(..)
    , EndReason(..)
    , GameSummary(..)
    , Winner(..)
    , makeStartState
    , priorMoveColor
    , applyMove
    , isTaggedEndState
    , gameSummary
    , winner
    , board_FromTaggedState 
    , coreState_FromTaggedState
    , nextMoveColor_FromTaggedState
    , mbPriorMove_FromTaggedState
    , actual_NextMoves_FromTaggedState
    , actual_BlackAndWhiteUnusedDiskCounts_FromTaggedState
    , actual_mbPriorMove_FromTaggedState
    )   
    where

import Data.Function ( (&) )
import Data.Tree.Game_tree.Game_tree
import qualified Data.Map.Strict as Map ( (!) )

import Disk ( Color(..), toggleColor )
import Board ( Board, Move(..), applyBoardMove, initialBoard, squaresColoredCount, validMoves, flipCount, movePos, moveColor, colorCount ) 
import UnusedDiskCount ( BlackUnusedDiskCount, WhiteUnusedDiskCount, Tagged_UnusedDiskCount(..), makeBlackUnusedDiskCount, makeWhiteUnusedDiskCount, isZeroCount, transferDiskTo, decreaseByOneFor, countFrom )
import SquareCount ( BlackSquareCount, WhiteSquareCount, Tagged_SquareCount(..), makeBlackSquareCount, makeWhiteSquareCount, countFrom )
import Position ( isCornerPos, isCornerNeighborPos )


data CoreState = CoreState BlackUnusedDiskCount WhiteUnusedDiskCount Board deriving (Eq, Show)

data StartState = StartState Color NextMoves CoreState deriving (Eq, Show)

data MidState = MidState PriorMove NextMoves CoreState deriving (Eq, Show)

data EndState = EndState PriorMove EndReason CoreState deriving (Eq, Show)

data Tagged_State
    = Tagged_StartState StartState
    | Tagged_MidState   MidState
    | Tagged_EndState   EndState
        deriving (Eq, Show)

newtype PriorMove = PriorMove Move deriving (Eq, Show)

newtype NextMoves = NextMoves [Move] deriving (Eq, Show)

data EndReason
    = NoUnusedDisksForBoth
    | NoValidMoves
        deriving (Eq, Show)

data GameSummary = GameSummary EndReason BlackSquareCount WhiteSquareCount deriving (Eq, Show)

data Winner
    = WinnerColor Color
    | Tie
        deriving (Eq, Show)

------------------

instance Game_tree Tagged_State 
    where

    is_terminal :: Tagged_State -> Bool
    is_terminal taggedState =
        case taggedState of
            Tagged_StartState _ -> False
            Tagged_MidState _   -> False
            Tagged_EndState _   -> True


    node_value :: Tagged_State -> Int
    node_value taggedState = 
        let
            cornerWeight :: Move -> Int
            cornerWeight move
                | isCornerPos         $ movePos move = 10
                | isCornerNeighborPos $ movePos move =  0
                | otherwise                          =  5
        in
            case taggedState of
                Tagged_StartState _  -> 
                    100 -- constant whatever

                Tagged_MidState (MidState (PriorMove move) _ _) -> 
                    cornerWeight move + flipCount move

                Tagged_EndState (EndState priorMove _ (CoreState _ _ board)) -> 
                    colorCount (priorMoveColor priorMove) board                            


    children :: Tagged_State -> [Tagged_State]
    children taggedState =
        actual_NextMoves_FromTaggedState taggedState
            & map (\ move -> applyMove move taggedState)

------------------

makeStartState :: StartState
makeStartState =
    let
        startColor = Black -- Rule 1: Black always moves first.
        board = initialBoard
        nextMoves = nextMovesFrom startColor board
    in
        StartState startColor nextMoves (CoreState makeBlackUnusedDiskCount makeWhiteUnusedDiskCount board)


priorMoveColor :: PriorMove -> Color 
priorMoveColor (PriorMove (Move color _ _)) =
    color


isTaggedEndState :: Tagged_State -> Bool
isTaggedEndState taggedState =
    case taggedState of
        Tagged_EndState _ -> True
        _ -> False


nextMovesFrom :: Color -> Board -> NextMoves
nextMovesFrom color board =
    NextMoves $ validMoves color board 


isZeroUnusedDiskCount :: Color -> CoreState -> Bool
isZeroUnusedDiskCount color (CoreState b w _) =
    case color of
        Black -> isZeroCount $ BlackUnused b
        White -> isZeroCount $ WhiteUnused w


applyMove :: Move -> Tagged_State -> Tagged_State
applyMove move taggedState =
    let
        makeMidState :: MidState
        makeMidState =
            let
                (CoreState b w board) = coreState_FromTaggedState taggedState
                color = moveColor move

                _map = decreaseByOneFor color b w -- for only one of them, whichever it is
                (BlackUnused b') = _map Map.! Black
                (WhiteUnused w') = _map Map.! White

                board' = applyBoardMove move board
                nexts = nextMovesFrom (toggleColor color) board'
            in
                MidState (PriorMove move) nexts (CoreState b' w' board')
    in
        case taggedState of
            Tagged_StartState _ -> processMidState makeMidState
            Tagged_MidState _   -> processMidState makeMidState
            Tagged_EndState _   -> taggedState


processMidState :: MidState -> Tagged_State
processMidState midState@(MidState priorMove nexts@(NextMoves moves) coreState@(CoreState b w board)) =
    let
        priorColor = priorMoveColor priorMove
        nextColor = toggleColor priorColor

        isZeroUnused_Prior = isZeroUnusedDiskCount priorColor coreState
        isZeroUnused_Next  = isZeroUnusedDiskCount nextColor  coreState
        
        end_NoValidMoves :: Tagged_State
        end_NoValidMoves = 
            Tagged_EndState $ EndState priorMove NoValidMoves coreState
        
        end_NoUnusedDisksForBoth :: Tagged_State
        end_NoUnusedDisksForBoth = 
            Tagged_EndState $ EndState priorMove NoUnusedDisksForBoth coreState

        transferDisk :: Tagged_State
        transferDisk = 
            let
                _map = transferDiskTo nextColor b w

                (BlackUnused b') = _map Map.! Black
                (WhiteUnused w') = _map Map.! White
            in
                Tagged_MidState $ MidState priorMove nexts (CoreState b' w' board)

        passThru :: Tagged_State
        passThru = 
            Tagged_MidState midState
    in
        if null moves then -- Rule 10: When it is no longer possible for either player to move, the game is over.
            end_NoValidMoves  
        else if isZeroUnused_Next then
            if isZeroUnused_Prior then -- Rule 10: When it is no longer possible for either player to move, the game is over.
                end_NoUnusedDisksForBoth 
            else -- Rule 9: If a player runs out of disks, but still has the opportunity to outflank an opposing disk on their turn, the opponent must give the player a disk to use. This can happen as many times as the player needs and can use a disk.
                transferDisk
        else
            passThru


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


gameSummary :: EndState -> GameSummary
gameSummary (EndState _ reason (CoreState _ _ board)) =
    let
        m = squaresColoredCount board

        b = makeBlackSquareCount $ m Map.! Black
        w = makeWhiteSquareCount $ m Map.! White
    in
        GameSummary reason b w


board_FromTaggedState :: Tagged_State -> Board
board_FromTaggedState taggedState =
    x where (CoreState _ _ x) = coreState_FromTaggedState taggedState


blackAndWhiteUnusedDiskCounts_FromTaggedState :: Tagged_State -> (BlackUnusedDiskCount, WhiteUnusedDiskCount)
blackAndWhiteUnusedDiskCounts_FromTaggedState taggedState =
    (b, w) 
        where (CoreState b w _) = coreState_FromTaggedState taggedState


coreState_FromTaggedState :: Tagged_State -> CoreState
coreState_FromTaggedState taggedState =
    case taggedState of
        Tagged_StartState (StartState _ _ x) -> x
        Tagged_MidState (MidState _ _ x)     -> x
        Tagged_EndState (EndState _ _ x)     -> x


nextMoveColor_FromTaggedState :: Tagged_State -> Color
nextMoveColor_FromTaggedState taggedState =  
    let
        priorMoveColorFrom :: PriorMove -> Color
        priorMoveColorFrom priorMove = 
            toggleColor $ priorMoveColor priorMove
    in
        case taggedState of
            Tagged_StartState (StartState color _ _) -> color
            Tagged_MidState (MidState priorMove _ _) -> priorMoveColorFrom priorMove
            Tagged_EndState (EndState priorMove _ _) -> priorMoveColorFrom priorMove
        

mbPriorMove_FromTaggedState :: Tagged_State -> Maybe PriorMove
mbPriorMove_FromTaggedState taggedState =    
    case taggedState of
        Tagged_StartState _                      -> Nothing
        Tagged_MidState (MidState priorMove _ _) -> Just priorMove
        Tagged_EndState (EndState priorMove _ _) -> Just priorMove


actual_mbPriorMove_FromTaggedState :: Tagged_State -> Maybe Move
actual_mbPriorMove_FromTaggedState taggedState = 
    mbPriorMove_FromTaggedState taggedState
        & fmap (\ (PriorMove move) -> move) 


actual_NextMoves_FromTaggedState :: Tagged_State -> [Move]
actual_NextMoves_FromTaggedState taggedState =
    case taggedState of
        Tagged_StartState (StartState _ (NextMoves x) _) -> x
        Tagged_MidState (MidState _ (NextMoves x) _)     -> x
        Tagged_EndState _                                -> []
        
    
actual_BlackAndWhiteUnusedDiskCounts_FromTaggedState :: Tagged_State -> (Int, Int)
actual_BlackAndWhiteUnusedDiskCounts_FromTaggedState taggedState =
    (UnusedDiskCount.countFrom $ BlackUnused b, UnusedDiskCount.countFrom $ WhiteUnused w)
        where (b, w) = blackAndWhiteUnusedDiskCounts_FromTaggedState taggedState