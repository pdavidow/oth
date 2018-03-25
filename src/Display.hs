module Display
    ( boardDisplay
    , boardWithValidMovesDisplay
    , boardWithFlipCountDisplay
    , movePosChoicesNomenclature
    , diskIconChar
    )
    where

import Data.Function ( (&) )
import Data.List ( find, intersperse ) 

import Board ( Board, BoardSquare(..), diskFrom, toPos, boardRow )
import Disk ( Disk, Color(..), diskColor, _flipCount )
import Position ( Position )
import BoardSize ( boardSize )
import ColumnName ( columnLegend, posNomenclature )
import Lib ( vSlice ) 


movePosChoicesNomenclature :: [(Int, Position)] -> String
movePosChoicesNomenclature xs =
    xs
        & concatMap (\ ((i, pos)) -> show i ++ ":" ++ posNomenclature pos ++ " ")


squareDisplay :: (Position -> String) -> (Disk -> String) -> BoardSquare -> String
squareDisplay emptyF filledF square =
    case square of
        Board_EmptySquare _ -> emptyF $ toPos square
        Board_FilledSquare x -> filledF $ diskFrom x


boardWithSquareDisplay :: (Position -> String) -> (Disk -> String) -> Board -> String
boardWithSquareDisplay emptyF filledF board =
    let
        boardString = 
            [1 .. boardSize] 
                & map 
                    ( \ i -> (show i ++ " ") ++ 
                        ( (vSlice ((i - 1) * boardSize) boardSize $ boardRow board)
                            & concatMap (squareDisplay emptyF filledF)
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


diskIconChar :: Color -> Char
diskIconChar color =
    case color of
        Black -> 'X' -- 'x'
        White -> 'O' -- 'o'


emptySquareContentsDisplay :: Position -> String
emptySquareContentsDisplay _ = 
    padSquareContents [defaultEmptySquareChar]


filledSquareContentsDisplay :: Disk -> String
filledSquareContentsDisplay disk = 
    padSquareContents [diskIconChar $ diskColor disk]


boardDisplay :: Board -> String
boardDisplay b =
    boardWithSquareDisplay emptySquareContentsDisplay filledSquareContentsDisplay b


boardWithValidMovesDisplay :: [(Int, Position)] -> Board -> String
boardWithValidMovesDisplay xs b =
    let
        emptyF :: Position -> String
        emptyF = \ pos -> padSquareContents $
            case find (\ ((_, pos')) -> pos == pos') xs of
                Just (moveN, _) -> show moveN
                Nothing -> [defaultEmptySquareChar]
    in
        boardWithSquareDisplay emptyF filledSquareContentsDisplay b


boardWithFlipCountDisplay :: Board -> String
boardWithFlipCountDisplay b =
    let
        filledF :: Disk -> String
        filledF disk = 
            padSquareContents $ show $ _flipCount disk
    in
        boardWithSquareDisplay emptySquareContentsDisplay filledF b