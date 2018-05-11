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
    , makeHistory
    , priorMoveColor
    , applyMoveOnHistory
    , gameSummary
    , winner
    , board_FromTaggedState 
    , coreState_FromTaggedState
    , nextMoveColor_FromTaggedState
    , nextMoveColor_FromMidState
    , mbPriorMove_FromTaggedState
    , actual_NextMoves_FromTaggedState
    , actual_UnusedDiskCounts_FromTaggedState_BlackWhite
    , actual_mbPriorMove_FromTaggedState
    , undoHistoryOnce
    , undoHistoryOnceForColor
    , isForfeitTurn -- todo only used in testing
    )   
    where

import Data.Maybe ( fromMaybe, mapMaybe )
import Data.Function ( (&) )
import Data.Tree.Game_tree.Game_tree
import Data.Array ( listArray, (!) )
import qualified Data.List.NonEmpty as NE ( NonEmpty, dropWhile, fromList, head, init, last, length, reverse, toList )

import Disk ( Color(..), toggleColor )
import Board ( Board, Move(..), Tagged_Square(..), applyBoardMove, initialBoard, squaresColoredCounts_BlackWhite, validMoves, moveColor, boardAt, filledSquares, toFilledSquare, isSquareColored, isEmptyAt, boardSquaresColored, toPos, cornerCounts_BlackWhite, filledSquaresAdjacentToEmptyCorners ) 
import UnusedDiskCount ( BlackUnusedDiskCount, WhiteUnusedDiskCount, Tagged_UnusedDiskCount(..), makeBlackUnusedDiskCount, makeWhiteUnusedDiskCount, isZeroCount, transferDiskTo, decreaseByOneFor, countFrom, applyToUnusedDiskCounts )
import SquareCount ( BlackSquareCount, WhiteSquareCount, Tagged_SquareCount(..), makeBlackSquareCount, makeWhiteSquareCount, countFrom )
import Position ( isValidCoords, makeValidPosition, posCoords )
import BlackWhite ( BlackWhite, makeBlackWhite, blacksWhites )


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
        round $ heuristic_score taggedState


    children :: Tagged_State -> [Tagged_State]
    children taggedState =
        actual_NextMoves_FromTaggedState taggedState
            & map (\ move -> applyMoveOnState move taggedState)

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


nextMovesFrom :: Color -> Board -> NextMoves
nextMovesFrom color board =
    NextMoves $ validMoves color board 


isZeroUnusedDiskCount :: Color -> CoreState -> Bool
isZeroUnusedDiskCount color (CoreState b w _) =
    case color of
        Black -> isZeroCount $ Tagged_BlackUnusedDiskCount b
        White -> isZeroCount $ Tagged_WhiteUnusedDiskCount w


applyMoveOnState :: Move -> Tagged_State -> Tagged_State
applyMoveOnState move taggedState =
    let
        makeMidState :: MidState
        makeMidState =
            let
                (CoreState b w board) = coreState_FromTaggedState taggedState
                color = moveColor move
                ( b', w' ) = applyToUnusedDiskCounts (decreaseByOneFor color) ( b, w )
                board' = applyBoardMove move board
                nexts = nextMovesFrom (toggleColor color) board'
            in
                MidState (PriorMove move) Normal nexts (CoreState b' w' board')
    in
        case taggedState of
            Tagged_StartState _ -> processMidState makeMidState
            Tagged_MidState   _ -> processMidState makeMidState
            Tagged_EndState   _ -> taggedState -- should never get here


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
            Tagged_MidState $ MidState priorMove TransferDisk_Rule9 nexts (CoreState b' w' board)
                where ( b', w' ) = applyToUnusedDiskCounts (transferDiskTo nextColor) ( b, w )

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
    GameSummary reason (makeBlackSquareCount b) (makeWhiteSquareCount w)
        where ( b, w ) = blacksWhites $ squaresColoredCounts_BlackWhite board


board_FromTaggedState :: Tagged_State -> Board
board_FromTaggedState taggedState =
    x where (CoreState _ _ x) = coreState_FromTaggedState taggedState


unusedDiskCounts_FromTaggedState :: Tagged_State -> (BlackUnusedDiskCount, WhiteUnusedDiskCount)
unusedDiskCounts_FromTaggedState taggedState =
    (b, w) 
        where (CoreState b w _) = coreState_FromTaggedState taggedState


coreState_FromTaggedState :: Tagged_State -> CoreState
coreState_FromTaggedState taggedState =
    case taggedState of
        Tagged_StartState (StartState _ _ x) -> x
        Tagged_MidState (MidState _ _ _ x)   -> x
        Tagged_EndState (EndState _ _ x)     -> x


colorResultingInTaggedState :: Tagged_State -> Color
colorResultingInTaggedState taggedState =
    case taggedState of
        Tagged_StartState (StartState color _ _)                      -> color
        Tagged_MidState (MidState (PriorMove (Move color _ _)) _ _ _) -> color
        Tagged_EndState (EndState (PriorMove (Move color _ _)) _ _)   -> color


nextMoveColor_FromTaggedState :: Tagged_State -> Maybe Color
nextMoveColor_FromTaggedState taggedState =  
    case taggedState of
        Tagged_StartState (StartState color _ _) -> Just color
        Tagged_MidState midState -> Just $ nextMoveColor_FromMidState midState
        Tagged_EndState _ -> Nothing
        

nextMoveColor_FromMidState :: MidState -> Color
nextMoveColor_FromMidState (MidState priorMove midStatus _ _) =  
    case midStatus of
        Normal -> toggleColor $ priorMoveColor priorMove
        ForfeitTurn_Rule2 -> priorMoveColor priorMove
        TransferDisk_Rule9 -> toggleColor $ priorMoveColor priorMove


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
        
    
actual_UnusedDiskCounts_FromTaggedState_BlackWhite :: Tagged_State -> BlackWhite Int
actual_UnusedDiskCounts_FromTaggedState_BlackWhite taggedState =
    makeBlackWhite (UnusedDiskCount.countFrom $ Tagged_BlackUnusedDiskCount b) (UnusedDiskCount.countFrom $ Tagged_WhiteUnusedDiskCount w)
        where (b, w) = unusedDiskCounts_FromTaggedState taggedState


makeHistory :: NE.NonEmpty Tagged_State
makeHistory =
    NE.fromList $ [Tagged_StartState makeStartState]


applyMoveOnHistory :: Move -> NE.NonEmpty Tagged_State -> NE.NonEmpty Tagged_State
applyMoveOnHistory move history = 
    let
        -- todo validate move
        taggedState = applyMoveOnState move $ NE.last history
    in
        NE.fromList $ (NE.toList history) ++ [taggedState]


undoHistoryOnce :: NE.NonEmpty Tagged_State -> Maybe (NE.NonEmpty Tagged_State)
undoHistoryOnce history = 
    undoHistoryOnceForColor color history
        where color = fromMaybe Black $ nextMoveColor_FromTaggedState $ NE.last history -- should never use default
        

undoHistoryOnceForColor :: Color -> NE.NonEmpty Tagged_State -> Maybe (NE.NonEmpty Tagged_State)
undoHistoryOnceForColor color history = 
    let
        lastState = NE.last history
    in
        if NE.length history == 1 then 
            Nothing
        else if NE.length history == 2 then
            let 
                headState = NE.head history
            in
                case headState of
                    Tagged_StartState (StartState color' _ _) -> -- always the case, by definition
                        if color == color' then
                            Just $ NE.fromList [headState]
                        else
                            Nothing
    
                    Tagged_MidState _ -> 
                        Nothing -- should never get here

                    Tagged_EndState _ -> 
                        Nothing -- should never get here
        else if isForfeitTurn lastState then
            Just $ NE.fromList $ NE.init history
        else
            case lastState of
                Tagged_StartState _ -> 
                    Nothing -- should never get here

                Tagged_MidState _ ->
                    let
                        toggledColor = toggleColor color
                        
                        f :: Tagged_State -> Bool
                        f = \ x -> colorResultingInTaggedState x == toggledColor
                    in
                        history
                            & NE.reverse
                            & NE.dropWhile f
                            & drop 1
                            & NE.fromList -- non empty by definition
                            & NE.reverse
                            & Just

                Tagged_EndState _ -> 
                    Nothing -- should never get here


isForfeitTurn :: Tagged_State -> Bool
isForfeitTurn taggedState = 
    case taggedState of
        Tagged_StartState _ ->
            False

        Tagged_MidState (MidState _ midStatus _ _) ->
            case midStatus of
                Normal -> False
                ForfeitTurn_Rule2 -> True
                TransferDisk_Rule9 -> False

        Tagged_EndState _ -> 
            False


---------------------------------------------------------------------------------------------
---------------------------------------------------------------------------------------------
-- Ideally should be in separate 'Heuristic' module, but that causes circular dependencies
---------------------------------------------------------------------------------------------

-- Assume: boardSize == 8

-- https://kartikkukreja.wordpress.com/2013/03/30/heuristic-function-for-reversiothello/

-- https://courses.cs.washington.edu/courses/cse573/04au/Project/mini1/RUSSIA/Final_Paper.pdf
    -- MaxPlayer to move next (section 4.1)

heuristic_pieceDifference :: Color -> Board -> Double 
heuristic_pieceDifference myColor board = 
    let
        ( b, w ) = blacksWhites $ squaresColoredCounts_BlackWhite board

        ( myCount, opCount ) = 
            if myColor == Black then
                ( b, w )
            else
                ( w, b )

        total = myCount + opCount
    in
        if myCount > opCount then
            100 * fromIntegral myCount / fromIntegral total
        else if myCount < opCount then
            -100 * fromIntegral opCount / fromIntegral total
        else
            0


heuristic_frontierDisks :: Color -> Board -> Double 
heuristic_frontierDisks myColor board = 
    let
        filledSquares' = filledSquares board
            & map (posCoords . toPos . Tagged_FilledSquare)
            & concatMap 
                ( \ (i,j) -> 
                    let
                        xs = map (i +) [-1, -1,  0,  1,  1,  1,  0, -1] 
                        ys = map (j +) [ 0,  1,  1,  1,  0, -1, -1, -1]
                    in
                        zip xs ys
                            & filter (\ (x,y) -> isValidCoords (x,y) && (isEmptyAt (makeValidPosition x y) board))
                            & mapMaybe (\ _ -> toFilledSquare $ boardAt board $ makeValidPosition i j)
                )

        count = \ color -> length $ filter (isSquareColored color) filledSquares'

        myCount = count myColor
        opCount = count $ toggleColor myColor

        total = myCount + opCount
    in
        if myCount > opCount then
            -100 * fromIntegral myCount / fromIntegral total
        else if myCount < opCount then
            100 * fromIntegral opCount / fromIntegral total
        else
            0


heuristic_diskSquares :: Color -> Board -> Double 
heuristic_diskSquares myColor board = 
    let
        v = listArray ((1,1), (8,8))
            [ 20, -3, 11,  8,  8, 11, -3, 20
            , -3, -7, -4,  1,  1, -4, -7, -3
            , 11, -4,  2,  2,  2,  2, -4, 11
            ,  8,  1,  2, -3, -3,  2,  1,  8
            ,  8,  1,  2, -3, -3,  2,  1,  8
            , 11, -4,  2,  2,  2,  2, -4, 11
            , -3, -7, -4,  1,  1, -4, -7, -3
            , 20, -3, 11,  8,  8, 11, -3, 20
            ]

        weight = \ color ->
            boardSquaresColored color board
                & map ((!) v . posCoords . toPos . Tagged_FilledSquare)
                & sum
                
        myWeight = weight myColor
        opWeight = weight $ toggleColor myColor
    in
        myWeight - opWeight


heuristic_cornerOccupancy :: Color -> Board -> Double 
heuristic_cornerOccupancy myColor board = 
    let       
        ( b, w ) = blacksWhites $ cornerCounts_BlackWhite board

        ( myCornerCount, oppCornerCount ) =
            if myColor == Black then ( b, w )
            else ( w, b )
    in
        25 * fromIntegral (myCornerCount - oppCornerCount)


heuristic_cornerCloseness :: Color -> Board -> Double 
heuristic_cornerCloseness myColor board = 
    let
        xs = filledSquaresAdjacentToEmptyCorners board
        count = \ color -> length $ filter (isSquareColored color) xs

        myCount = count myColor
        opCount = count $ toggleColor myColor
    in
        -12.5 * fromIntegral (myCount - opCount)


heuristic_mobility :: Color -> [Move] -> Board -> Double 
heuristic_mobility myColor nextMoves board = 
    let
        opColor = toggleColor myColor

        myMoveCount = length nextMoves
        oppMoveCount = length $ validMoves opColor board

        total = myMoveCount + oppMoveCount
    in
        if myMoveCount > oppMoveCount then
            100 * fromIntegral myMoveCount / fromIntegral total
        else if myMoveCount < oppMoveCount then
            -100 * fromIntegral oppMoveCount / fromIntegral total
        else
            0


heuristic_score :: Tagged_State -> Double 
heuristic_score taggedState = 
    case taggedState of
        Tagged_StartState _  -> 
            1 -- constant whatever

        Tagged_MidState midState -> 
            let
                nextMoveColor = nextMoveColor_FromMidState midState
                nextMoves = actual_NextMoves_FromTaggedState taggedState
                board = board_FromTaggedState taggedState
            in
                (10 * heuristic_pieceDifference nextMoveColor board) + 
                    (801.724 * heuristic_cornerOccupancy nextMoveColor board) + 
                        (382.026 * heuristic_cornerCloseness nextMoveColor board) + 
                            (78.922 * heuristic_mobility nextMoveColor nextMoves board) + 
                                (74.396 * heuristic_frontierDisks nextMoveColor board) + 
                                    (10 * heuristic_diskSquares nextMoveColor board)                        

        Tagged_EndState (EndState (PriorMove (Move color _ _)) _ (CoreState _ _ board)) -> 
            heuristic_pieceDifference (toggleColor color) board

---------------------------------------------------------------------------------------------
---------------------------------------------------------------------------------------------