{-# LANGUAGE InstanceSigs #-}
 
module State
    ( CoreState(..) 
    , StartState(..)
    , MidState(..)
    , EndState(..)
    , Tagged_State(..)
    , PriorMove(..)
    , NextMoves(..)
    , MidStatus(..)
    , EndStatus(..)
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

data MidState = MidState PriorMove MidStatus NextMoves CoreState deriving (Eq, Show)

data EndState = EndState PriorMove EndStatus CoreState deriving (Eq, Show)

data Tagged_State
    = Tagged_StartState StartState
    | Tagged_MidState   MidState
    | Tagged_EndState   EndState
        deriving (Eq, Show)

newtype PriorMove = PriorMove Move deriving (Eq, Show)

newtype NextMoves = NextMoves [Move] deriving (Eq, Show)

data MidStatus
    = Normal
    | ForfeitTurn_Rule2
    | TransferDisk_Rule9
        deriving (Eq, Show)

data EndStatus
    = NoUnusedDisksForBoth
    | NoValidMoves
        deriving (Eq, Show)

data GameSummary = GameSummary EndStatus BlackSquareCount WhiteSquareCount deriving (Eq, Show)

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
                | isCornerPos         $ movePos move =  500
                | isCornerNeighborPos $ movePos move =    0
                | otherwise                          =  100
        in
            case taggedState of
                Tagged_StartState _  -> 
                    100 -- constant whatever

                Tagged_MidState (MidState priorMove@(PriorMove move) _ _ _) -> 
                    let
                        myColor = priorMoveColor priorMove
                        --weight = cornerWeight move + flipCount move
                        weight = cornerWeight move + flipCount move - (10 * (length $ actual_NextMoves_FromTaggedState taggedState))
                    in
                        polarizeOn myColor weight

                Tagged_EndState (EndState priorMove _ (CoreState _ _ board)) -> 
                    let
                        myColor = priorMoveColor priorMove
                        opColor = toggleColor myColor

                        myColorCount = colorCount myColor board
                        opColorCount = colorCount opColor board

                        weight = 
                            if myColorCount == opColorCount then
                                200
                            else if myColorCount > opColorCount then
                                1000
                            else
                                0
                    in
                        polarizeOn myColor weight



    children :: Tagged_State -> [Tagged_State]
    children taggedState =
        actual_NextMoves_FromTaggedState taggedState
            & map (\ move -> applyMove move taggedState)

------------------
-- todo used?
polarizeOn :: Color -> (Int -> Int)
polarizeOn color = 
    if color == Black then -- todo: userColor = Black
        (*) 1
    else
        (*) (-1)


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
        Black -> isZeroCount $ Tagged_BlackUnusedDiskCount b
        White -> isZeroCount $ Tagged_WhiteUnusedDiskCount w


applyMove :: Move -> Tagged_State -> Tagged_State
applyMove move taggedState =
    let
        makeMidState :: MidState
        makeMidState =
            let
                (CoreState b w board) = coreState_FromTaggedState taggedState
                color = moveColor move

                _map = decreaseByOneFor color b w -- for only one of them, whichever it is
                (Tagged_BlackUnusedDiskCount b') = _map Map.! Black
                (Tagged_WhiteUnusedDiskCount w') = _map Map.! White

                board' = applyBoardMove move board
                nexts = nextMovesFrom (toggleColor color) board'
            in
                MidState (PriorMove move) Normal nexts (CoreState b' w' board')
    in
        case taggedState of
            Tagged_StartState _ -> processMidState makeMidState
            Tagged_MidState _   -> processMidState makeMidState
            Tagged_EndState _   -> taggedState


processMidState :: MidState -> Tagged_State
processMidState midState@(MidState priorMove _ nexts@(NextMoves moves) coreState@(CoreState b w board)) =
    let
        priorColor = priorMoveColor priorMove
        nextColor = toggleColor priorColor

        isZeroUnused_Prior = isZeroUnusedDiskCount priorColor coreState
        isZeroUnused_Next  = isZeroUnusedDiskCount  nextColor coreState

        nexts'@(NextMoves priorColorMoves) = nextMovesFrom priorColor board
        
        end_NoValidMoves :: Tagged_State
        end_NoValidMoves = 
            Tagged_EndState $ EndState priorMove NoValidMoves coreState
        
        end_NoUnusedDisksForBoth :: Tagged_State
        end_NoUnusedDisksForBoth = 
            Tagged_EndState $ EndState priorMove NoUnusedDisksForBoth coreState

        forfeitTurn :: Tagged_State
        forfeitTurn = 
            Tagged_MidState $ MidState priorMove ForfeitTurn_Rule2 nexts' coreState

        transferDisk :: Tagged_State
        transferDisk = 
            let
                _map = transferDiskTo nextColor b w

                (Tagged_BlackUnusedDiskCount b') = _map Map.! Black
                (Tagged_WhiteUnusedDiskCount w') = _map Map.! White
            in
                Tagged_MidState $ MidState priorMove TransferDisk_Rule9 nexts (CoreState b' w' board)

        passThru :: Tagged_State
        passThru = 
            Tagged_MidState midState
    in
        if null moves then 
            if null priorColorMoves then
                end_NoValidMoves -- Rule 10: When it is no longer possible for either player to move, the game is over.
            else
                forfeitTurn -- Rule 2: If a player cannot outflank and flip at least one opposing disk, they forfeit their turn and their opponent moves again. 
        else if isZeroUnused_Next then
            if isZeroUnused_Prior then 
                end_NoUnusedDisksForBoth -- Rule 10: When it is no longer possible for either player to move, the game is over.
            else 
                transferDisk -- Rule 9: If a player runs out of disks, but still has the opportunity to outflank an opposing disk on their turn, the opponent must give the player a disk to use. This can happen as many times as the player needs and can use a disk.
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
        Tagged_MidState (MidState _ _ _ x)   -> x
        Tagged_EndState (EndState _ _ x)     -> x


nextMoveColor_FromTaggedState :: Tagged_State -> Maybe Color
nextMoveColor_FromTaggedState taggedState =  
    case taggedState of
        Tagged_StartState (StartState color _ _) -> 
            Just color

        Tagged_MidState (MidState priorMove midStatus _ _) ->
            Just $ case midStatus of
                Normal -> toggleColor $ priorMoveColor priorMove
                ForfeitTurn_Rule2 -> priorMoveColor priorMove
                TransferDisk_Rule9 -> toggleColor $ priorMoveColor priorMove

        Tagged_EndState _ -> 
            Nothing
        

mbPriorMove_FromTaggedState :: Tagged_State -> Maybe PriorMove
mbPriorMove_FromTaggedState taggedState =    
    case taggedState of
        Tagged_StartState _                        -> Nothing
        Tagged_MidState (MidState priorMove _ _ _) -> Just priorMove
        Tagged_EndState (EndState priorMove _ _)   -> Just priorMove


actual_mbPriorMove_FromTaggedState :: Tagged_State -> Maybe Move
actual_mbPriorMove_FromTaggedState taggedState = 
    mbPriorMove_FromTaggedState taggedState
        & fmap (\ (PriorMove move) -> move) 


actual_NextMoves_FromTaggedState :: Tagged_State -> [Move]
actual_NextMoves_FromTaggedState taggedState =
    case taggedState of
        Tagged_StartState (StartState _ (NextMoves x) _) -> x
        Tagged_MidState (MidState _ _ (NextMoves x) _)   -> x
        Tagged_EndState _                                -> []
        
    
actual_BlackAndWhiteUnusedDiskCounts_FromTaggedState :: Tagged_State -> (Int, Int)
actual_BlackAndWhiteUnusedDiskCounts_FromTaggedState taggedState =
    (UnusedDiskCount.countFrom $ Tagged_BlackUnusedDiskCount b, UnusedDiskCount.countFrom $ Tagged_WhiteUnusedDiskCount w)
        where (b, w) = blackAndWhiteUnusedDiskCounts_FromTaggedState taggedState