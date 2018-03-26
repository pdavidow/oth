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
    , GameState.boardWithFlipCountDisplay
    , gameState
    , gameSummary
    , winner
    )   
    where
 
import qualified Data.Map.Strict as Map ( (!) )
import Data.List ( intersperse )

import Disk ( Color(..), toggleColor )
import Board ( Board, Move(..), applyBoardMove, initialBoard, squaresColoredCount, validMoves ) 
import UnusedDiskCount ( BlackUnusedDiskCount, WhiteUnusedDiskCount, All_UnusedDiskCount(..), initialBlackUnusedDiskCount, initialWhiteUnusedDiskCount, isZeroCount, transferDiskTo, decreaseByOneFor, countFrom )
import SquareCount ( BlackSquareCount, WhiteSquareCount, All_SquareCount(..), makeBlackSquareCount, makeWhiteSquareCount, countFrom )
import Position ( Position )
import Display ( boardDisplay, boardWithValidMovesDisplay, boardWithFlipCountDisplay, diskIconChar )


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


nextToMove :: All_State -> Color
nextToMove tagged =
    x where ((x, _, _, _, _)) = gameStateElems tagged


possibleMoves :: All_State -> [Move]
possibleMoves tagged =
    x where ((_, x, _, _, _)) = gameStateElems tagged


blackAndWhiteUnusedDiskCounts :: All_State -> ( Int, Int )
blackAndWhiteUnusedDiskCounts tagged =
    ( b, w ) where ((_, _, b, w, _)) = gameStateElems tagged


board :: All_State -> Board
board tagged =
    x where ((_, _, _, _, x)) = gameStateElems tagged


gameStateElems :: All_State -> ( Color, [Move], Int, Int, Board )
gameStateElems tagged =
    let
        (GameState (NextToMove color) (PossibleMoves moves) b w bd) = gameState tagged
        blackUnusedDiskCount = UnusedDiskCount.countFrom $ BlackUnused b
        whiteUnusedDiskCount = UnusedDiskCount.countFrom $ WhiteUnused w
    in
        ( color, moves, blackUnusedDiskCount, whiteUnusedDiskCount, bd )


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
        bd = initialBoard
        moves = PossibleMoves $ validMoves color bd
    in
        PlayGameState $ 
            GameState 
                next
                moves
                initialBlackUnusedDiskCount 
                initialWhiteUnusedDiskCount 
                bd
  

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
applyMove move p@(PlayGameState (GameState n@(NextToMove color) m b w bd)) =
    let
        bd' = applyBoardMove move bd

        mp = decreaseByOneFor color b w -- for only one of them, whichever it is
        (BlackUnused b') = mp Map.! Black
        (WhiteUnused w') = mp Map.! White

        oppColor = toggleColor color
        n' = NextToMove oppColor
        m' = PossibleMoves $ validMoves oppColor bd'

        p' = PlayGameState $ GameState n' m' b' w' bd'
    in
        analyzeState p'


analyzeState :: PlayGameState -> All_State
analyzeState p@(PlayGameState g@(GameState n@(NextToMove color) v@(PossibleMoves moves) bCount wCount bd)) =
    -- Rule 9: If a player runs out of disks, but still has the opportunity to outflank an opposing disk on their turn, the opponent must give the player a disk to use. This can happen as many times as the player needs and can use a disk.
    -- Rule 10: When it is no longer possible for either player to move, the game is over.
    let
        oppColor = toggleColor color

        isZeroUnused = isZeroUnusedDiskCount_Tagged color $ PlayState p
        isZeroUnusedOpp = isZeroUnusedDiskCount_Tagged oppColor $ PlayState p

        end_NoUnusedDisksForBoth = EndState $ EndGameState NoUnusedDisksForBoth g
        end_NoValidMoves = EndState $ EndGameState NoValidMoves g
        playWithTransferredDisk = \ b w -> PlayState $ PlayGameState $ GameState n v b w bd 
        play = PlayState p
    in
        if null moves then 
            end_NoValidMoves
        else if isZeroUnused then
            if isZeroUnusedOpp then 
                end_NoUnusedDisksForBoth
            else 
                let
                    mp = transferDiskTo color bCount wCount

                    (BlackUnused bCount') = mp Map.! Black
                    (WhiteUnused wCount') = mp Map.! White
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
gameSummary (EndGameState reason (GameState _ _ _ _ bd)) =
    let
        m = squaresColoredCount bd

        b = makeBlackSquareCount $ m Map.! Black
        w = makeWhiteSquareCount $ m Map.! White
    in
        GameSummary reason b  w
     

-- todo move to Display ..?
gameStateDisplay :: Maybe [(Int, Position)] -> All_State -> String
gameStateDisplay mbShowMoves tagged =
    let
        (_, _, b, w, bd) = gameStateElems tagged
        f = \ n char -> intersperse ' '  (replicate n $ char)
        blackUnused = "Black " ++ show b ++ ": " ++ (f b $ diskIconChar Black)        
        whiteUnused = "White " ++ show w ++ ": " ++ (f w $ diskIconChar White)

        footer = 
            "Available Disks" ++ "\n" ++
            blackUnused ++ "\n" ++
            whiteUnused

        boardString = 
            case mbShowMoves of
                Just xs -> boardWithValidMovesDisplay xs bd
                Nothing -> boardDisplay bd
    in
        boardString ++ "\n\n" ++ footer     


-- todo move to Display ..?
boardWithFlipCountDisplay :: All_State -> String
boardWithFlipCountDisplay tagged =
    Display.boardWithFlipCountDisplay $ board tagged
