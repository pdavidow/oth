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

import Color( Color(..))
import Board ( Board, EmptySquare(..), FilledSquare, Tagged_Square(..), Move, diskFrom, toPos, boardRow, movePos, outflankPositions )
import Disk ( diskColor, flipCount )
import Position ( Position )
import BoardSize ( boardSize )
import ColumnName ( columnLegend, posNomenclature )
import State ( EndState, Tagged_State, PriorMove(..), GameSummary(..), EndStatus(..), Winner(..), board_FromTaggedState, actual_UnusedDiskCounts_FromTaggedState_BlackWhite, gameSummary, winner, mbPriorMove_FromTaggedState, actual_mbPriorMove_FromTaggedState, priorMoveColor )
import SquareCount ( Tagged_SquareCount(..), countFrom )
import BlackWhite ( BlackWhite(..) )
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


squareDisplay :: (EmptySquare -> String) -> (FilledSquare -> String) -> Tagged_Square -> String
squareDisplay f g square =
    case square of
        Tagged_EmptySquare  x -> f x
        Tagged_FilledSquare x -> g x


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
    -- Assume: Square width is 5 chars
    case length s of
        1 -> "  " ++ s ++ "  "
        2 ->  " " ++ s ++ "  "
        3 ->  " " ++ s ++ " "
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


priorMoveIndicatorChar :: Char
priorMoveIndicatorChar =
    '='


flipIndicatorChar :: Char
flipIndicatorChar =
    '-'


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
            padSquareContents $ show $ flipCount $ diskFrom filledSquare
    in
        boardWithSquareDisplay defaultEmptySquareContentsDisplay filledF board
        

showMoveNumInEmptySquare :: [(Int, Position)] -> (EmptySquare -> String)
showMoveNumInEmptySquare showMoves emptySquare =
    padSquareContents $
        case find (\ ((_, pos')) -> (toPos $ Tagged_EmptySquare emptySquare) == pos') showMoves of
            Just (moveN, _) -> show moveN
            Nothing         -> [defaultEmptySquareChar]


bracketIcon :: Char -> Char -> String
bracketIcon bracket icon =
    [bracket] ++ [icon] ++ [bracket]


boardDisplay :: Maybe (EmptySquare -> String) -> Maybe (FilledSquare -> String) -> Board -> String
boardDisplay mbF mbG board =
    boardWithSquareDisplay 
        (fromMaybe defaultEmptySquareContentsDisplay mbF)
        (fromMaybe defaultFilledSquareContentsDisplay mbG)
        board


mbFilledSquareDisplayer :: Tagged_State -> Maybe (FilledSquare -> String)
mbFilledSquareDisplayer taggedState =
    let
        f :: Move -> (FilledSquare -> String)
        f = \ priorMove filledSquare ->
            let
                color = diskColor $ diskFrom filledSquare

                -- mutually exclusive by definition
                isPriorMove = (toPos $ Tagged_FilledSquare filledSquare) == movePos priorMove
                isFlipped = elem (toPos $ Tagged_FilledSquare filledSquare) $ outflankPositions priorMove
            in
                padSquareContents $ 
                    if isPriorMove then
                        bracketIcon priorMoveIndicatorChar $ highlightDiskIconChar color
                    else if isFlipped then
                        bracketIcon flipIndicatorChar $ defaultDiskIconChar color
                    else
                        [defaultDiskIconChar color]
    in
        fmap f $ actual_mbPriorMove_FromTaggedState taggedState


gameStateDisplay :: Maybe [(Int, Position)] -> Tagged_State -> String
gameStateDisplay mbShowMoves taggedState =
    let
        mbEmptyF :: Maybe (EmptySquare -> String)
        mbEmptyF = fmap showMoveNumInEmptySquare mbShowMoves

        mbFilledF :: Maybe (FilledSquare -> String)
        mbFilledF = mbFilledSquareDisplayer taggedState

        body = boardDisplay mbEmptyF mbFilledF $ board_FromTaggedState taggedState
        footer = gameStateDisplayFooter taggedState
    in
        body ++ "\n\n" ++ footer


gameStateDisplayFooter :: Tagged_State -> String
gameStateDisplayFooter taggedState =
    let
        part1 = case mbPriorMove_FromTaggedState taggedState of
            Just priorMove@(PriorMove move) ->
                let
                    color = priorMoveColor priorMove
                    code = posNomenclature $ movePos move
                in
                    (colorAllCapsString color) ++ " moved to: " ++ code
            Nothing -> 
                "Awaiting your first move..."
        
        part2 = unusedDisksDisplay taggedState
    in
        part1 ++ "\n\n" ++ part2


unusedDisksDisplay :: Tagged_State -> String
unusedDisksDisplay taggedState =
    let
        ( BlackWhite b w ) = actual_UnusedDiskCounts_FromTaggedState_BlackWhite taggedState
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
            Tie               -> "TIE game"
    in 
        "GAME OVER (" ++ reasonString ++ ") " ++ winnerString ++ ".  Disk-counts: Black " ++ show (SquareCount.countFrom $ Tagged_BlackSquareCount b) ++ "; White " ++ show (SquareCount.countFrom $ Tagged_WhiteSquareCount w)
