module GameState
    ( GameState(..)
    , PlayGameState(..)
    , EndGameState(..)
    , All_State(..)
    , EndReason(..)
    , makePlayGameState
    , nextToMove
    , possibleMoves
    , blackAndWhiteUnusedDiskCounts
    , applyMove
    , gameStateDisplay
    , gameState
    , gameSummary
    , winner
    )  
    where
 
import qualified Data.Map.Strict as Map ( (!) )

import Disk ( Color(..), toggleColor, iconChar )
import Board ( Board, Move(..), applyBoardMove, initialBoard, numSquaresColored, validMoves, boardDisplay, boardWithValidMovesDisplay ) 
import UnusedDiskCount ( BlackUnusedDiskCount, WhiteUnusedDiskCount, All_UnusedDiskCount(..), initialBlackUnusedDiskCount, initialWhiteUnusedDiskCount, decreaseByOne, isZeroCount, transferDiskTo, decreaseByOneFor, countFrom )
import SquareCount ( BlackSquareCount, WhiteSquareCount, All_SquareCount(..), makeBlackSquareCount, makeWhiteSquareCount, countFrom )
import Position ( Position )

data GameState = GameState NextToMove PossibleMoves BlackUnusedDiskCount WhiteUnusedDiskCount Board deriving (Eq, Show)

newtype NextToMove = NextToMove Color deriving (Eq, Show)

newtype PossibleMoves = PossibleMoves [Move] deriving (Eq, Show)

newtype PlayGameState = PlayGameState GameState deriving (Eq, Show)

data EndGameState = EndGameState EndReason GameState deriving (Eq, Show)

data All_State
    = PlayState PlayGameState
    | EndState  EndGameState
        deriving (Eq, Show)

data GameSummary = GameSummary EndReason BlackSquareCount WhiteSquareCount deriving (Eq, Show)
        
data EndReason
    = NoUnusedDisksForBoth
    | NoValidMoves
        deriving (Eq, Show)

data Winner
    = WinnerColor Color
    | Tie
        deriving (Eq, Show)


blackAndWhiteUnusedDiskCounts :: All_State -> (Int, Int)
blackAndWhiteUnusedDiskCounts tagged =
    let
        (b, w) = 
            case tagged of
                PlayState (PlayGameState (GameState _ _ b w _)) -> (b, w)
                EndState (EndGameState _ (GameState _ _ b w _)) -> (b, w)
    in
        ( UnusedDiskCount.countFrom $ BlackUnused b
        , UnusedDiskCount.countFrom $ WhiteUnused w
        )


nextToMove :: All_State -> Color
nextToMove tagged =
    case tagged of
        PlayState (PlayGameState (GameState (NextToMove c) _ _ _ _)) -> c
        EndState (EndGameState _ (GameState (NextToMove c) _ _ _ _)) -> c -- in theory


possibleMoves :: All_State -> [Move]
possibleMoves tagged =
    case tagged of
        PlayState (PlayGameState (GameState _ (PossibleMoves m) _ _ _)) -> m
        EndState (EndGameState _ (GameState _ (PossibleMoves m) _ _ _)) -> m


gameState :: All_State -> GameState
gameState tagged =
    case tagged of
        PlayState (PlayGameState g) -> g
        EndState (EndGameState _ g) -> g


initialNextToMove :: NextToMove
initialNextToMove =
    NextToMove Black


makePlayGameState :: PlayGameState
makePlayGameState =
    let
        next = initialNextToMove
        (NextToMove color) = next
        board = initialBoard
        moves = PossibleMoves $ validMoves color board
    in
        PlayGameState $ 
            GameState 
                next
                moves
                initialBlackUnusedDiskCount 
                initialWhiteUnusedDiskCount 
                board
  

isZeroUnusedDiskCount :: Color -> GameState -> Bool
isZeroUnusedDiskCount color (GameState _ _ b w _) =
    case color of
        Black -> isZeroCount $ BlackUnused b
        White -> isZeroCount $ WhiteUnused w


isZeroUnusedDiskCount_Tagged :: Color -> All_State -> Bool
isZeroUnusedDiskCount_Tagged color tagged =
    let
        f :: GameState -> Bool
        f = \ x -> isZeroUnusedDiskCount color x
    in
        case tagged of
            PlayState (PlayGameState x) -> f x
            EndState (EndGameState _ x) -> f x


applyMove :: Move -> PlayGameState -> All_State
applyMove move p@(PlayGameState (GameState n@(NextToMove color) m b w board)) =
    let
        board' = applyBoardMove move board

        mahp = decreaseByOneFor color b w -- for only one of them, whichever it is
        (BlackUnused b') = mahp Map.! Black
        (WhiteUnused w') = mahp Map.! White

        oppColor = toggleColor color
        n' = NextToMove oppColor
        m' = PossibleMoves $ validMoves oppColor board'

        p' = PlayGameState $ GameState n' m' b' w' board'
    in
        analyzeState p'


analyzeState :: PlayGameState -> All_State
analyzeState p@(PlayGameState g@(GameState n@(NextToMove color) v@(PossibleMoves moves) bCount wCount board)) =
    -- Rule 9: If a player runs out of disks, but still has the opportunity to outflank an opposing disk on their turn, the opponent must give the player a disk to use. This can happen as many times as the player needs and can use a disk.
    -- Rule 10: When it is no longer possible for either player to move, the game is over.
    let
        oppColor = toggleColor color

        isZeroUnused = isZeroUnusedDiskCount_Tagged color $ PlayState p
        isZeroUnusedOpp = isZeroUnusedDiskCount_Tagged oppColor $ PlayState p

        end_NoUnusedDisksForBoth = EndState $ EndGameState NoUnusedDisksForBoth g
        end_NoValidMoves = EndState $ EndGameState NoValidMoves g
        playWithTransferredDisk = \ b w -> PlayState $ PlayGameState $ GameState n v b w board 
        play = PlayState p
    in
        if null moves then 
            end_NoValidMoves
        else if isZeroUnused then
            if isZeroUnusedOpp then 
                end_NoUnusedDisksForBoth
            else 
                let
                    mahp = transferDiskTo color bCount wCount

                    (BlackUnused bCount') = mahp Map.! Black
                    (WhiteUnused wCount') = mahp Map.! White
                in
                    playWithTransferredDisk bCount' wCount'
        else
            play


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


gameSummary :: EndGameState -> GameSummary
gameSummary (EndGameState reason (GameState _ _ _ _ board)) =
    let
        m = numSquaresColored board

        b = makeBlackSquareCount $ m Map.! Black
        w = makeWhiteSquareCount $ m Map.! White
    in
        GameSummary reason b  w
     

gameStateDisplay :: Maybe [(Int, Position)] -> All_State -> String
gameStateDisplay mbShowMoves tagged =
    let
        (b, w, board) = 
            case tagged of
                PlayState (PlayGameState (GameState _ _ b w board)) -> (b, w, board)
                EndState (EndGameState _ (GameState _ _ b w board)) -> (b, w, board)

        blackUnusedCount = UnusedDiskCount.countFrom $ BlackUnused b
        whiteUnusedCount = UnusedDiskCount.countFrom $ WhiteUnused w

        blackUnused = "Black: " ++ (replicate blackUnusedCount $ iconChar Black)
        whiteUnused = "White: " ++ (replicate whiteUnusedCount $ iconChar White)

        footer = 
            "Available Disks" ++ "\n" ++
            blackUnused ++ "\n" ++
            whiteUnused

        boardString = 
            case mbShowMoves of
                Just xs -> boardWithValidMovesDisplay xs board
                Nothing -> boardDisplay board
    in
        boardString ++ "\n" ++ footer     