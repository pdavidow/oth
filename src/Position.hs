module Position
    ( Position -- hiding constructor
    , PosRow(..)
    , makeValidPosition
    , posCoords
    , adjacentPositions
    , radiatingPosRows
    , isValidCoords
    )
    where

-- Origin is at top left of board, where x is vertical axis, y horizontal 

import Data.Either ( rights )
import Data.Function ( (&) )

import BoardSize ( boardSize )

data Position = Position {x :: Int, y :: Int} deriving (Eq, Show) -- one-based

newtype PosRow = PosRow [Position] deriving (Eq, Show)

data Dir = Inc | Dec


isValidCoord :: Int -> Bool
isValidCoord x = 
    x >= 1 && x <= boardSize


isValidCoords :: (Int, Int) -> Bool
isValidCoords (i, j) =
    isValidCoord i && isValidCoord j


makePosition :: Int -> Int -> Either String Position
makePosition i j =
    let
        sizeString = show boardSize
    in
        if isValidCoords (i,j) then
            Right $ Position i j
        else
            Left $ "Out of Bounds: Position ranges from (1,1) to ("  ++ sizeString ++ ","  ++ sizeString ++ ") inclusive"


makeValidPosition :: Int -> Int -> Position
makeValidPosition i j =
    either error id $ makePosition i j


posCoords :: Position -> (Int, Int)
posCoords (Position i j) = 
    (i, j)


adjacentPositions :: Position -> [Position]
adjacentPositions (Position i j) =
    [ (i-1, j-1), (i, j-1), (i+1, j-1)
    , (i-1, j  ),           (i+1, j  )
    , (i-1, j+1), (i, j+1), (i+1, j+1)
    ]
    -- todo to conform to nomenclature, but technically unnecessary (tests will need updating)
    -- [ (i-1, j-1), (i-1, j), (i-1, j+1)
    -- , (i,   j-1),           (i  , j+1)
    -- , (i+1, j-1), (i+1, j), (i+1, j+1)
    -- ]   
        & map (\ (i', j') -> makePosition i' j')
        & rights


radiatingPosRows :: Position -> [PosRow] 
radiatingPosRows pos =
    -- does NOT include starting pos
    let 
        candidates =
            [rowVertUp pos] ++
            [rowVertDown pos] ++

            [rowHorizRight pos] ++
            [rowHorizLeft pos] ++

            [rowDiagUpRight pos] ++
            [rowDiagUpLeft pos] ++

            [rowDiagDownRight pos] ++
            [rowDiagDownLeft pos]         
    in
        candidates
            & filter (\ (PosRow row) -> not $ null row)


rowVertUp :: Position -> PosRow
rowVertUp (Position i j) =   
    PosRow $ [ Position i' j' | i' <- reverse [1..(i-1)], j' <- [j] ]


rowVertDown :: Position -> PosRow
rowVertDown (Position i j) =   
    PosRow $ [ Position i' j' | i' <- [(i+1)..boardSize], j' <- [j] ]


rowHorizRight :: Position -> PosRow
rowHorizRight (Position i j) =   
    PosRow $ [ Position i' j' | i' <- [i], j' <- [(j+1)..boardSize] ]


rowHorizLeft :: Position -> PosRow
rowHorizLeft (Position i j) =   
    PosRow $ [ Position i' j' | i' <- [i], j' <- reverse [1..(j-1)] ]
        

rowDiagUpRight :: Position -> PosRow
rowDiagUpRight (Position i j) =  
    rowDiag Dec Inc $ Position (i-1) (j+1)


rowDiagUpLeft :: Position -> PosRow
rowDiagUpLeft (Position i j) = 
    rowDiag Dec Dec $ Position (i-1) (j-1)  


rowDiagDownRight :: Position -> PosRow
rowDiagDownRight (Position i j) = 
    rowDiag Inc Inc $ Position (i+1) (j+1) 


rowDiagDownLeft :: Position -> PosRow
rowDiagDownLeft (Position i j) =
    rowDiag Inc Dec $ Position (i+1) (j-1) 


rowDiag :: Dir -> Dir -> Position -> PosRow
rowDiag horizDir vertDir (Position i j) =
    let
        isExceededLimits :: Int -> Int -> Bool
        isExceededLimits = \ x y -> 
            case (horizDir, vertDir) of
                (Inc, Inc) -> (x > boardSize) || (y > boardSize)
                (Inc, Dec) -> (x > boardSize) || (y < 1)
                (Dec, Inc) -> (x < 1)         || (y > boardSize)
                (Dec, Dec) -> (x < 1)         || (y < 1)

        f :: Dir -> (Int -> Int)
        f = \ dir -> 
            case dir of
                Inc -> ( 1 +) 
                Dec -> (-1 +) 
                
        go result x y = 
            if isExceededLimits x y then
                result
            else
                let 
                    x' = f horizDir x
                    y' = f vertDir y
                in 
                    go (result ++ [Position x y]) x' y'
    in
        PosRow $ go [] i j