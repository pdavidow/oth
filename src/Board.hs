module Board
    ( Color(..)
    , EmptySquare(..)
    , Move(..)
    , FilledRow(..)
    , BoardSquare(..)
    , initialBoard
    , validMoves
    , board_DisplayString

    , makeBoard
    , place
    , makeWhiteDisk
    , makeBlackDisk
    , boardAt
    , toPos
    )
    where

    -- module Board
    -- ( )
    -- where

import Data.Maybe ( mapMaybe  )
import Data.List ( nub )
import Data.Array ( ( ! ), ( // ), Array, array, elems )
import Data.Function ( (&) )

import BoardSize ( boardSize )
import Position ( Position, PosRow(..), adjacentPositions, radiatingPosRows )
import Lib ( vSlice ) 


data Disk = Disk {_initColor :: Color,  _flipCount :: Int} deriving (Eq, Show)

data Color = Black | White deriving (Eq, Show)

data EmptySquare = EmptySquare {_pos :: Position, _radiatingPosRows :: [PosRow]}

data FilledSquare = FilledSquare Disk EmptySquare deriving (Eq)

data BoardSquare 
    = Board_EmptySquare EmptySquare
    | Board_FilledSquare FilledSquare 
        deriving (Eq, Show)

newtype Board = Board (Array Position BoardSquare) deriving (Eq, Show)

newtype FilledRow = FilledRow [FilledSquare] deriving (Eq, Show) -- horiz, vert, diag

data Move = Move {_color :: Color, _square :: EmptySquare,  _outflanks :: [FilledRow]} deriving (Eq, Show)

------------------

instance Eq EmptySquare where
    (EmptySquare pos1 _) == (EmptySquare pos2 _) = pos1 == pos2 


instance Show EmptySquare where
    show (EmptySquare pos _) = "EmptySquare " ++ show pos    


instance Show FilledSquare where
    show (FilledSquare disk (EmptySquare pos _)) = "FilledSquare " ++ show pos ++ " " ++ show disk  

------------------

makeBoard :: Board
makeBoard = 
    let
        makeElem :: Position -> BoardSquare
        makeElem pos =
            Board_EmptySquare $ EmptySquare {_pos = pos, _radiatingPosRows = radiatingPosRows pos}
    in
        Board $ array ((1,1), (boardSize,boardSize)) 
            [ ((i,j), makeElem (i,j)) | i <- [1..boardSize], j <- [1..boardSize] ]


makeFilledSquare :: Disk -> EmptySquare -> FilledSquare
makeFilledSquare disk emptySquare =
    FilledSquare disk emptySquare


makeDisk :: Color -> Disk
makeDisk color = 
    Disk {_initColor = color,  _flipCount = 0}


makeWhiteDisk :: Disk
makeWhiteDisk = 
    makeDisk White


makeBlackDisk :: Disk
makeBlackDisk = 
    makeDisk Black


diskColor :: Disk -> Color
diskColor disk =
    if even $ _flipCount disk then
        _initColor disk
    else
        toggleColor $ _initColor disk


initialBoard :: Board
initialBoard =
    let
        board = makeBoard
    in
        board -- assume: boardSize = 8
            & place makeWhiteDisk (boardAt board (4,4)) 
            & place makeBlackDisk (boardAt board (4,5))
            & place makeBlackDisk (boardAt board (5,4))
            & place makeWhiteDisk (boardAt board (5,5))


boardAt :: Board -> Position -> BoardSquare
boardAt (Board board) pos =
    -- todo Position should be SmartPosition which is bounded in (1,1)(boardSize,boardSize)
    board ! pos


place :: Disk -> BoardSquare -> Board -> Board
place disk boardSquare board = 
    case boardSquare of
        Board_EmptySquare emptySquare -> fillAt emptySquare disk board
        Board_FilledSquare _ -> board
       

fillAt :: EmptySquare -> Disk -> Board -> Board
fillAt emptySquare@(EmptySquare pos _) disk (Board board) =
    Board $ board // [(pos, Board_FilledSquare $ makeFilledSquare disk emptySquare)]


flipAt :: BoardSquare -> Board -> Board
flipAt boardSquare board =
    case boardSquare of
        Board_EmptySquare _ -> board
        Board_FilledSquare (FilledSquare disk emptySquare) -> fillAt emptySquare (flipDisk disk) board


flipDisk :: Disk -> Disk
flipDisk (Disk color flipCount) =
    Disk (toggleColor color) $ flipCount + 1


toggleColor :: Color -> Color
toggleColor color = 
    case color of
        White -> Black
        Black -> White


toEmptySquare :: BoardSquare -> Maybe EmptySquare
toEmptySquare boardSquare =
    case boardSquare of 
        Board_EmptySquare x  -> Just x
        Board_FilledSquare _ -> Nothing


toFilledSquare :: BoardSquare -> Maybe FilledSquare
toFilledSquare boardSquare =
    case boardSquare of 
        Board_EmptySquare _  -> Nothing
        Board_FilledSquare x -> Just x
 
        
emptySquares :: Board -> [EmptySquare]
emptySquares (Board board) =  
    mapMaybe toEmptySquare $ elems board


filledSquares :: Board -> [FilledSquare]
filledSquares (Board board) =
    mapMaybe toFilledSquare $ elems board


isSquareColored :: Color -> FilledSquare -> Bool
isSquareColored color (FilledSquare (Disk color' _) _) =
    color == color'


isEmptySquare :: BoardSquare -> Bool
isEmptySquare boardSquare =
    case boardSquare of 
        Board_EmptySquare _ -> True
        Board_FilledSquare _ -> False


isFilledSquare :: BoardSquare -> Bool
isFilledSquare boardSquare =
    not $ isEmptySquare boardSquare


toPos :: BoardSquare -> Position
toPos boardSquare =
    case boardSquare of 
        Board_EmptySquare (EmptySquare pos _) -> pos
        Board_FilledSquare (FilledSquare _ (EmptySquare pos _)) -> pos


contiguousFilledRow :: PosRow -> Board -> FilledRow
contiguousFilledRow (PosRow ps) board =
    ps
        & map (\ p -> boardAt board p) -- todo wasteful, use fold ?
        & takeWhile isFilledSquare 
        & mapMaybe toFilledSquare
        & FilledRow


radiatingFilledRows :: EmptySquare -> Board -> [FilledRow]
radiatingFilledRows emptySquare board = 
    _radiatingPosRows emptySquare
        & map (\ posRow -> contiguousFilledRow posRow board)
        & filter (\ (FilledRow xs) -> not $ null xs)


outflanks :: Color -> EmptySquare -> Board -> [FilledRow]
outflanks color emptySquare board =
    radiatingFilledRows emptySquare board
        & filter (\ (FilledRow row) -> length row >= 2) 
        & filter (\ (FilledRow row) -> isSquareColored toggledColor $ head row)  
        & filter (\ (FilledRow row) -> any (\ x -> isSquareColored color x) $ tail row)
        & map (\ (FilledRow row) -> FilledRow $ takeWhile (\ x -> isSquareColored toggledColor x) row)
            where toggledColor = toggleColor color


adjacentEmptySquares :: BoardSquare -> Board -> [EmptySquare]
adjacentEmptySquares boardSquare board =
    toPos boardSquare
        & adjacentPositions
        & mapMaybe (\ pos -> toEmptySquare $ boardAt board pos)


squaresColored :: Color -> Board -> [FilledSquare]
squaresColored color board =
    filter (\ x -> isSquareColored color x) $ filledSquares board


validMove :: Color -> EmptySquare -> Board -> Maybe Move
validMove color emptySquare board = 
    let
        candidates = outflanks color emptySquare board
    in
        if null candidates then
            Nothing
        else
            Just $ Move 
                { _color = color
                , _square = emptySquare
                , _outflanks = candidates
                }


validMoves :: Color -> Board -> [Move]
validMoves color board =
      squaresColored (toggleColor color) board
        & concatMap (\ filledSquare -> adjacentEmptySquares (Board_FilledSquare filledSquare) board)
        & nub 
        & mapMaybe (\ emptySquare -> validMove color emptySquare board) 
           
             
boardSquare_DisplayString :: BoardSquare -> String
boardSquare_DisplayString boardSquare =
        let 
            string = 
                case boardSquare of
                    Board_EmptySquare _ -> "-"
                    Board_FilledSquare (FilledSquare disk _)  ->
                        case diskColor disk of
                            Black -> "x"
                            White -> "o"
        in
            " " ++ string ++ " "    


board_DisplayString :: Board -> String
board_DisplayString (Board board) =
    let
        colLegend = "   A  B  C  D  E  F  G  H\n" 

        f = \ i -> 
            [show i ++ " "] 
                ++ (map boardSquare_DisplayString $ vSlice ((i - 1) * boardSize) boardSize $ elems board) 
                    ++ ["\n"]

        boardString = concat $ concat $ map f [1..boardSize]             
    in
        colLegend ++ boardString 
