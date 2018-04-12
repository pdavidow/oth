module Position
    ( Position -- hiding constructor
    , PosRow(..)
    , makePosition
    , makeSomePosition
    , posCoords
    , adjacentPositions
    , radiatingPosRows
    , isCornerPos
    , isCornerNeighborPos
    )
    where

import Data.Function ( (&) )
import Data.Either ( fromRight  )

import BoardSize ( boardSize )


data Position = Position Int Int deriving (Eq, Show) -- one-based

newtype PosRow = PosRow [Position] deriving (Eq, Show)

data Dir = Inc | Dec


makePosition :: Int -> Int -> Either String Position
makePosition i j =
    let
        isValidParam :: Int -> Bool
        isValidParam = \x -> x >= 1 && x <= boardSize

        sizeString = show boardSize
    in
        if isValidParam i && isValidParam j then
            Right $ Position i j
        else
            Left $ "Out of Bounds: Position ranges from (1,1) to ("  ++ sizeString ++ ","  ++ sizeString ++ ") inclusive"


makeSomePosition :: Int -> Int -> Position
makeSomePosition i j =
    fromRight (Position 1 1) $ makePosition i j


posCoords :: Position -> (Int, Int)
posCoords (Position i j) = 
    (i, j)


isCornerPos :: Position -> Bool
isCornerPos (Position i j) =
    f i && f j
        where f = \x -> x == 1 || x == boardSize


isCornerNeighborPos :: Position -> Bool
isCornerNeighborPos pos =
    isCornerNeighborPos_UpperLeft  pos ||
    isCornerNeighborPos_UpperRight pos ||
    isCornerNeighborPos_LowerRight pos ||
    isCornerNeighborPos_LowerLeft  pos 
    

isCornerNeighborPos_UpperLeft :: Position -> Bool
isCornerNeighborPos_UpperLeft (Position 1 2) = True
isCornerNeighborPos_UpperLeft (Position 2 2) = True
isCornerNeighborPos_UpperLeft (Position 2 1) = True
isCornerNeighborPos_UpperLeft (Position _ _) = False


isCornerNeighborPos_UpperRight :: Position -> Bool
isCornerNeighborPos_UpperRight (Position 1 j) = (j == boardSize - 1)
isCornerNeighborPos_UpperRight (Position 2 j) = (j == boardSize - 1) || (j == boardSize)
isCornerNeighborPos_UpperRight (Position _ _) = False


isCornerNeighborPos_LowerRight :: Position -> Bool
isCornerNeighborPos_LowerRight (Position i j) = 
    ((i == boardSize - 1) && (j == boardSize - 1)) ||
    ((i == boardSize - 1) && (j == boardSize))     ||
    ((i == boardSize)     && (j == boardSize - 1))


isCornerNeighborPos_LowerLeft :: Position -> Bool
isCornerNeighborPos_LowerLeft (Position 7 1) = True
isCornerNeighborPos_LowerLeft (Position 7 2) = True
isCornerNeighborPos_LowerLeft (Position 8 2) = True
isCornerNeighborPos_LowerLeft (Position _ _) = False


adjacentPositions :: Position -> [Position]
adjacentPositions (Position i j) =
    let
        candidates = 
            [ (i-1, j-1), (i, j-1), (i+1, j-1)
            , (i-1, j  ),           (i+1, j  )
            , (i-1, j+1), (i, j+1), (i+1, j+1)
            ]

        isInRange = \ x -> (x >= 1) && (x <= boardSize)
    in
        candidates
            & filter (\ (i', j') -> isInRange i' && isInRange j') 
            & map (\ (i', j') -> Position i' j')


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