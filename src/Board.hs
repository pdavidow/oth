module Board
    ( Board -- hiding constructor
    , EmptySquare(..)
    , FilledSquare -- hiding constructor
    , Move(..)
    , Outflanks(..)
    , FilledRow(..)
    , Tagged_Square(..)
    , initialBoard
    , validMoves
    , movePosChoices
    , boardFromConfig
    , toPos
    , applyBoardMove
    , filledPositions
    , boardSquaresColored
    , squaresColoredCount
    , isSquareColored
    , isEmptyAt
    , isFilledAt
    , emptySquares
    , filledSquares
    , toFilledSquare
    , diskFrom
    , boardAt
    , boardRow
    , movePos
    , flipCount
    , dummyMove
    , colorCount
    , moveColor
    , outflankPositions
    , cornerCountsGroupedBlackWhite
    , filledSquaresAdjacentToEmptyCorners
    ------------------------------------------ ,flipAt -- Should NOT be exposed (but ok to temp expose for sake of commented-out test)
    )
    where

import Data.Maybe ( mapMaybe )
import Data.List ( foldl', nub )
import Data.Array ( ( ! ), ( // ), Array, array, elems )
import Data.Function ( (&) )
import qualified Data.Map.Strict as Map ( Map, empty, insert )
import Safe ( headMay, tailMay )

import Disk ( Disk, Color(..), diskColor, flipDisk, makeDisk, toggleColor )  
import BoardSize ( boardSize )
import Position ( Position, PosRow(..), makeSomePosition, adjacentPositions, posCoords, radiatingPosRows )
import Lib ( mapTakeWhile ) 
 

data EmptySquare = EmptySquare Position RadiatingPosRows

newtype RadiatingPosRows = RadiatingPosRows [PosRow] deriving (Eq, Show)

data FilledSquare = FilledSquare Disk EmptySquare deriving (Eq)

data Tagged_Square 
    = Tagged_EmptySquare EmptySquare
    | Tagged_FilledSquare FilledSquare 
        deriving (Eq, Show)

newtype Board = Board (Array (Int, Int) Tagged_Square) deriving (Eq, Show)

data Move = Move Color EmptySquare Outflanks deriving (Eq, Show)

newtype Outflanks = Outflanks [FilledRow] deriving (Eq, Show)

newtype FilledRow = FilledRow [FilledSquare] deriving (Eq, Show)

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
        makeElem :: (Int, Int) -> Tagged_Square
        makeElem (i,j) =
            Tagged_EmptySquare $ EmptySquare pos $ RadiatingPosRows $ radiatingPosRows pos
                where pos = makeSomePosition i j
    in
        Board $ array ((1,1), (boardSize,boardSize)) 
            [ ((i,j), makeElem (i,j)) | i <- [1..boardSize], j <- [1..boardSize] ]


makeFilledSquare :: Disk -> EmptySquare -> FilledSquare
makeFilledSquare disk emptySquare =
    FilledSquare disk emptySquare


boardFromConfig :: [(Color, Position)] -> Board
boardFromConfig config =
    config
        & foldl' (\ acc ((color, pos)) -> place (makeDisk color) (boardAt acc pos) acc) makeBoard


initialBoard :: Board
initialBoard = 
    boardFromConfig 
        [ (White, (makeSomePosition 4 4))
        , (White, (makeSomePosition 5 5))
        , (Black, (makeSomePosition 4 5))
        , (Black, (makeSomePosition 5 4))
        ]


boardAt :: Board -> Position -> Tagged_Square
boardAt (Board board) pos =
    board ! posCoords pos

            
boardRow :: Board -> [Tagged_Square]
boardRow (Board board) =
    elems board


place :: Disk -> Tagged_Square -> Board -> Board
place disk taggedSquare board = 
    case taggedSquare of
        Tagged_EmptySquare emptySquare -> fillAt emptySquare disk board
        Tagged_FilledSquare _ -> board
       

fillAt :: EmptySquare -> Disk -> Board -> Board
fillAt emptySquare@(EmptySquare pos _) disk (Board board) =
    Board $ board // [(posCoords pos, Tagged_FilledSquare $ makeFilledSquare disk emptySquare)]


flipAt :: Tagged_Square -> Board -> Board
flipAt taggedSquare board =
    case taggedSquare of
        Tagged_EmptySquare _ -> board
        Tagged_FilledSquare (FilledSquare disk emptySquare) -> fillAt emptySquare (flipDisk disk) board


flipCount :: Move -> Int
flipCount move =
    length $ outflankSquares move


outflankPositions :: Move -> [Position]    
outflankPositions move =
    outflankSquares move
        & map (toPos . Tagged_FilledSquare)


outflankSquares :: Move -> [FilledSquare]
outflankSquares (Move _ _ (Outflanks xs)) =
    xs
        & (\ filledRows -> concatMap (\ (FilledRow ys) -> ys) filledRows)


toEmptySquare :: Tagged_Square -> Maybe EmptySquare
toEmptySquare taggedSquare =
    case taggedSquare of 
        Tagged_EmptySquare x  -> Just x
        Tagged_FilledSquare _ -> Nothing


toFilledSquare :: Tagged_Square -> Maybe FilledSquare
toFilledSquare taggedSquare =
    case taggedSquare of 
        Tagged_EmptySquare _  -> Nothing
        Tagged_FilledSquare x -> Just x
 
        
emptySquares :: Board -> [EmptySquare]
emptySquares (Board board) =  
    mapMaybe toEmptySquare $ elems board


filledSquares :: Board -> [FilledSquare]
filledSquares (Board board) =
    mapMaybe toFilledSquare $ elems board


emptyCorners :: Board -> [EmptySquare]
emptyCorners board =
    mapMaybe toEmptySquare $ corners board


filledCorners :: Board -> [FilledSquare]
filledCorners board =
    mapMaybe toFilledSquare $ corners board


diskFrom :: FilledSquare -> Disk
diskFrom (FilledSquare disk _) =  
    disk


isSquareColored :: Color -> FilledSquare -> Bool
isSquareColored color (FilledSquare disk _) =
    color == diskColor disk


isEmptySquare :: Tagged_Square -> Bool
isEmptySquare taggedSquare =
    case taggedSquare of 
        Tagged_EmptySquare _ -> True
        Tagged_FilledSquare _ -> False


isFilledSquare :: Tagged_Square -> Bool
isFilledSquare taggedSquare =
    not $ isEmptySquare taggedSquare


isEmptyAt :: Position -> Board -> Bool
isEmptyAt pos board =
    isEmptySquare $ boardAt board pos


isFilledAt :: Position -> Board -> Bool
isFilledAt pos board =
    isFilledSquare $ boardAt board pos


toPos :: Tagged_Square -> Position
toPos taggedSquare =
    case taggedSquare of 
        Tagged_EmptySquare (EmptySquare pos _) -> pos
        Tagged_FilledSquare (FilledSquare _ (EmptySquare pos _)) -> pos


contiguousFilledRow :: PosRow -> Board -> FilledRow
contiguousFilledRow (PosRow ps) board =
    ps
        & mapTakeWhile (\ p -> boardAt board p) isFilledSquare
        & mapMaybe toFilledSquare
        & FilledRow


outflanks :: Color -> EmptySquare -> Board -> [FilledRow]
outflanks color (EmptySquare _ (RadiatingPosRows posRows)) board =
    posRows
        & map (\ posRow -> contiguousFilledRow posRow board)
        & filter (\ filledRow -> isHeadColored toggledColor filledRow && isTailHaveAnyColor color filledRow)
        & map (\ (FilledRow xs) -> FilledRow $ takeWhile (\ x -> isSquareColored toggledColor x) xs)
            where toggledColor = toggleColor color


isHeadColored :: Color -> FilledRow -> Bool
isHeadColored color (FilledRow row) =
    case headMay row of
        Just square -> isSquareColored color square
        Nothing -> False


isTailHaveAnyColor :: Color -> FilledRow -> Bool
isTailHaveAnyColor color (FilledRow row) =
    case tailMay row of
        Just squares -> any (isSquareColored color) squares
        Nothing -> False


adjacentEmptySquares :: Tagged_Square -> Board -> [EmptySquare]
adjacentEmptySquares taggedSquare board =
    toPos taggedSquare
        & adjacentPositions
        & mapMaybe (\ pos -> toEmptySquare $ boardAt board pos)


boardSquaresColored :: Color -> Board -> [FilledSquare]
boardSquaresColored color board =
    squaresColored color $ filledSquares board


squaresColored :: Color -> [FilledSquare] -> [FilledSquare]
squaresColored color xs =
    filter (\ x -> isSquareColored color x) xs


colorCount :: Color -> Board -> Int
colorCount color board =
    filledSquares board
        & squaresColored color
        & length


-- todo redo as tuple, then reuse in heuristic
squaresColoredCount :: Board -> Map.Map Color Int
squaresColoredCount board =
    let
        f :: Color -> Map.Map Color Int -> Map.Map Color Int
        f = \ color m -> Map.insert color (colorCount color board) m
    in
        Map.empty   
            & f Black
            & f White 
            

moveColor :: Move -> Color
moveColor (Move x _ _) =
    x


filledPositions :: Color -> Board -> [Position]
filledPositions color board = 
    boardSquaresColored color board
        & map (\ x -> toPos $ Tagged_FilledSquare x)


validMove :: Color -> EmptySquare -> Board -> Maybe Move
validMove color emptySquare board = 
    let
        candidates = outflanks color emptySquare board
    in
        if null candidates then
            Nothing
        else
            Just $ Move color emptySquare $ Outflanks candidates


validMoves :: Color -> Board -> [Move]
validMoves color board =
      boardSquaresColored (toggleColor color) board
        & concatMap (\ filledSquare -> adjacentEmptySquares (Tagged_FilledSquare filledSquare) board)
        & nub 
        & mapMaybe (\ emptySquare -> validMove color emptySquare board) 
         
        
movePos :: Move -> Position
movePos (Move _ (EmptySquare pos _) _) =
    pos


movePosChoices :: [Move] -> [(Int, Position)]
movePosChoices xs =
    -- 1 based
    zip [(1 :: Int)..] $ map movePos xs 


applyBoardMove :: Move -> Board -> Board
applyBoardMove (Move color emptySquare (Outflanks xs)) board =
    let
        disk = makeDisk color
        taggedSquare = Tagged_EmptySquare emptySquare

        flipOutflanks :: Board -> Board
        flipOutflanks board' =
            xs
                & concatMap (\ (FilledRow ys) -> ys)
                & foldl' (\ acc y -> flipAt (Tagged_FilledSquare y) acc) board'
    in
        board
            & place disk taggedSquare
            & flipOutflanks


dummyMove :: Move   
dummyMove =
    Move Black (EmptySquare (makeSomePosition 1 1) $ RadiatingPosRows []) (Outflanks [])


cornerCountsGroupedBlackWhite :: Board -> ( Int,  Int )
cornerCountsGroupedBlackWhite board =
    ( length blacks, length whites)
        where ( blacks, whites ) = cornersGroupedBlackWhite board


cornersGroupedBlackWhite :: Board -> ( [FilledSquare],  [FilledSquare] )
cornersGroupedBlackWhite board =
    let
        xs = filledCorners board
        f = \ color -> filter (isSquareColored color) xs
    in
        (f Black, f White)


filledSquaresAdjacentToEmptyCorners :: Board -> [FilledSquare]
filledSquaresAdjacentToEmptyCorners board =
    emptyCorners board
        & concatMap (adjacentPositions . toPos . Tagged_EmptySquare)
        & mapMaybe (toFilledSquare . boardAt board)


corners :: Board -> [Tagged_Square]
corners board =
    [(1,1), (1,boardSize), (boardSize,boardSize), (boardSize,1)]
        & map (\(i,j) -> boardAt board (makeSomePosition i j)) 