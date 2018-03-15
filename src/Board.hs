module Board
    ( board_DisplayString

    , initialBoard
    )
    where

import Data.Array ( ( ! ), ( // ), Array, array, elems )
import Data.Function ( (&) )
import Lib ( slice )


data Disk = Disk {_initColor :: Color,  _flipCount :: Int} deriving (Eq, Show)

data Color = Black | White deriving (Eq, Show)

data EmptySquare = EmptySquare Position deriving (Eq, Show)

data FilledSquare = FilledSquare Disk Position deriving (Eq, Show)

data BoardSquare 
    = Board_EmptySquare EmptySquare
    | Board_FilledSquare FilledSquare 
        deriving (Eq, Show)

type Position = ( Int, Int )

type Board = Array Position BoardSquare


boardSize :: Int
boardSize = 
    8


emptyBoard :: Board
emptyBoard = 
    array ((1,1), (boardSize,boardSize)) 
        [((i, j), Board_EmptySquare $ EmptySquare $ (i, j)) | i <- [1..boardSize], j <- [1..boardSize]]


newDisk :: Color -> Disk
newDisk color = 
    Disk {_initColor = color,  _flipCount = 0}


newWhiteDisk :: Disk
newWhiteDisk = 
    newDisk White


newBlackDisk :: Disk
newBlackDisk = 
    newDisk Black


initialBoard :: Board
initialBoard =
    let
        board = emptyBoard
    in
        board
            & place newWhiteDisk (board ! (4,4)) 
            & place newBlackDisk (board ! (4,5))
            & place newBlackDisk (board ! (5,4))
            & place newWhiteDisk (board ! (5,5))


place :: Disk -> BoardSquare -> Board -> Board
place disk boardSquare board = 
    case boardSquare of
        Board_EmptySquare (EmptySquare pos) -> fillAt pos disk board
        Board_FilledSquare _ -> board


flipAt :: BoardSquare -> Board -> Board
flipAt boardSquare board =
    case boardSquare of
        Board_EmptySquare _ -> board
        Board_FilledSquare (FilledSquare disk pos) -> fillAt pos (flipDisk disk) board
           

fillAt :: Position -> Disk -> Board -> Board
fillAt pos disk board =
    board // [(pos, Board_FilledSquare $ FilledSquare disk pos)]


flipDisk :: Disk -> Disk
flipDisk (Disk color flipCount) =
    Disk (toggleColor color) $ flipCount + 1


toggleColor :: Color -> Color
toggleColor color = 
    case color of
        White -> Black
        Black -> White


boardSquare_DisplayString :: BoardSquare -> String
boardSquare_DisplayString boardSquare =
        let 
            string = 
                case boardSquare of
                    Board_EmptySquare _ -> "-"
                    Board_FilledSquare (FilledSquare (Disk color _) _)  ->
                        case color of
                            Black -> "x"
                            White -> "o"
        in
            " " ++ string ++ " "    


board_DisplayString :: Board -> String
board_DisplayString board =
    let
        colLegend = "   A  B  C  D  E  F  G  H\n" 
        xs = elems board
        f = \ i -> [show i ++ " "] ++ (map boardSquare_DisplayString $ slice ((i - 1) * boardSize) (i * boardSize) xs) ++ ["\n"]
        boardString = concat $ concat $ map f [1..boardSize] -- could use flatten
             
    in
        colLegend ++ boardString 
