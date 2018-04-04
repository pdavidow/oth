module Display
    ( gameStateDisplay
    , boardDisplay
    , boardWithFlipCountDisplay
    , movePosChoicesNomenclature
    , gameSummaryDisplay
    , colorAllCapsString
    , showMoveNumInEmptySquare
    )
    where

import Data.Function ( (&) )
import Data.List ( find, intersperse ) 
import Data.Maybe ( fromMaybe )

import Board ( Board, EmptySquare(..), FilledSquare, BoardSquare(..), diskFrom, toPos, boardRow, movePos )
import Disk ( Color(..), diskColor, _flipCount )
import Position ( Position )
import BoardSize ( boardSize )
import ColumnName ( columnLegend, posNomenclature )
import State ( EndState, Tagged_State, GameSummary(..), EndReason(..), Winner(..), board_FromTaggedState, actual_BlackAndWhiteUnusedDiskCounts_FromTaggedState, gameSummary, winner, actual_mbPriorMove_FromTaggedState )
import SquareCount ( Tagged_SquareCount(..), countFrom )
import Lib ( vSlice ) 


colorAllCapsString :: Color -> String
colorAllCapsString c =
    case c of
        Black -> "BLACK"
        White -> "WHITE"


movePosChoicesNomenclature :: [(Int, Position)] -> String
movePosChoicesNomenclature xs =
    xs
        & concatMap (\ ((i, pos)) -> show i ++ ":" ++ posNomenclature pos ++ " ")


squareDisplay :: (EmptySquare -> String) -> (FilledSquare -> String) -> BoardSquare -> String
squareDisplay f g square =
    case square of
        Board_EmptySquare  x -> f x
        Board_FilledSquare x -> g x


boardWithSquareDisplay :: (EmptySquare -> String) -> (FilledSquare -> String) -> Board -> String
boardWithSquareDisplay f g board =
    let
        boardString = 
            [1 .. boardSize] 
                & map 
                    ( \ i -> (show i ++ " ") ++ 
                        ( (vSlice ((i - 1) * boardSize) boardSize $ boardRow board)
                            & concatMap (squareDisplay f g)
                        )
                    )
                & intersperse ("\n\n")
                & concat
    in
        "    " ++ columnLegend ++ "\n" ++ boardString 


padSquareContents :: String -> String
padSquareContents s = 
    -- Assume: Square width is 5 chars, and contents are either of length 1 or 2
    case length s of
        1 -> "  " ++ s ++ "  "
        2 ->  " " ++ s ++ "  "
        _ -> s


defaultEmptySquareChar :: Char
defaultEmptySquareChar = 
    '.'


largeDiskIconChar :: Color -> Char
largeDiskIconChar color =
    case color of
        Black -> 'X' 
        White -> 'O'


smallDiskIconChar :: Color -> Char
smallDiskIconChar color =
    case color of
        Black -> 'x'
        White -> 'o'


defaultDiskIconChar :: (Color -> Char)
defaultDiskIconChar =
    smallDiskIconChar


highlightDiskIconChar :: (Color -> Char)
highlightDiskIconChar =
    largeDiskIconChar


defaultEmptySquareContentsDisplay :: EmptySquare -> String
defaultEmptySquareContentsDisplay _ = 
    padSquareContents [defaultEmptySquareChar]


defaultFilledSquareContentsDisplay :: FilledSquare -> String
defaultFilledSquareContentsDisplay filledSquare = 
    padSquareContents [defaultDiskIconChar $ diskColor $ diskFrom filledSquare]


boardWithFlipCountDisplay :: Board -> String
boardWithFlipCountDisplay board =
    let
        filledF :: FilledSquare -> String
        filledF filledSquare = 
            padSquareContents $ show $ _flipCount $ diskFrom filledSquare
    in
        boardWithSquareDisplay defaultEmptySquareContentsDisplay filledF board
        

showMoveNumInEmptySquare :: [(Int, Position)] -> (EmptySquare -> String)
showMoveNumInEmptySquare showMoves emptySquare =
    padSquareContents $
        case find (\ ((_, pos')) -> (toPos $ Board_EmptySquare emptySquare) == pos') showMoves of
            Just (moveN, _) -> show moveN
            Nothing         -> [defaultEmptySquareChar]


highlightPriorMoveInFilledSquare :: Position -> (FilledSquare -> String)
highlightPriorMoveInFilledSquare pos filledSquare =
    let
        color = diskColor $ diskFrom filledSquare
        isPriorMove = (toPos $ Board_FilledSquare filledSquare) == pos

        f = case isPriorMove of 
            True  -> highlightDiskIconChar
            False -> defaultDiskIconChar
    in
        padSquareContents [f color]


boardDisplay :: Maybe (EmptySquare -> String) -> Maybe (FilledSquare -> String) -> Board -> String
boardDisplay mbF mbG board =
    boardWithSquareDisplay 
        (fromMaybe defaultEmptySquareContentsDisplay mbF)
        (fromMaybe defaultFilledSquareContentsDisplay mbG)
        board


gameStateDisplay :: Maybe [(Int, Position)] -> Tagged_State -> String
gameStateDisplay mbShowMoves taggedState =
    let
        mbEmptyF :: Maybe (EmptySquare -> String)
        mbEmptyF = 
            fmap showMoveNumInEmptySquare mbShowMoves

        mbFilledF :: Maybe (FilledSquare -> String)
        mbFilledF = 
            fmap highlightPriorMoveInFilledSquare mbPriorMovePos
                where mbPriorMovePos = fmap movePos (actual_mbPriorMove_FromTaggedState taggedState)

        body = boardDisplay mbEmptyF mbFilledF $ board_FromTaggedState taggedState
        footer = unusedDisksDisplay taggedState
    in
        body ++ "\n\n" ++ footer


unusedDisksDisplay :: Tagged_State -> String
unusedDisksDisplay taggedState =
    let
        (b, w) = actual_BlackAndWhiteUnusedDiskCounts_FromTaggedState taggedState
        f = \ n char -> intersperse ' '  (replicate n $ char)

        blackUnused = "Black " ++ show b ++ ": " ++ (f b $ defaultDiskIconChar Black)        
        whiteUnused = "White " ++ show w ++ ": " ++ (f w $ defaultDiskIconChar White)
    in
        "Available Disks" ++ "\n" ++
        blackUnused ++ "\n" ++
        whiteUnused       


gameSummaryDisplay :: EndState -> String
gameSummaryDisplay endState =
    let 
        g@(GameSummary reason b w) = gameSummary endState

        reasonString = case reason of
            NoUnusedDisksForBoth -> "No more available disks for either player"
            NoValidMoves         -> "No more valid moves"

        winnerString = case winner g of
            WinnerColor color -> "Winner is " ++ (colorAllCapsString color)
            Tie               -> "Tie game"
    in 
        "GAME OVER (" ++ reasonString ++ ") " ++ winnerString ++ ".  Disk-counts: Black " ++ show (SquareCount.countFrom $ Tagged_BlackSquareCount b) ++ "; White " ++ show (SquareCount.countFrom $ Tagged_WhiteSquareCount w)
