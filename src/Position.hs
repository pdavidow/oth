module Position
    ( Position
    , PosRow(..)
    , adjacentPositions
    , radiatingPosRows
    )
    where

import Data.Function ( (&) )

import BoardSize ( boardSize )

type Position = ( Int, Int )

newtype PosRow = PosRow [Position]  deriving (Eq, Show) -- horiz, vert, diag

data Dir = Inc | Dec


adjacentPositions :: Position -> [Position]
adjacentPositions (i, j) =
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
rowVertUp (i, j) =   
    PosRow $ [ (i',j') | i' <- reverse [1..(i-1)], j' <- [j] ]


rowVertDown :: Position -> PosRow
rowVertDown (i, j) =   
    PosRow $ [ (i',j') | i' <- [(i+1)..boardSize], j' <- [j] ]


rowHorizRight :: Position -> PosRow
rowHorizRight (i, j) =   
    PosRow $ [ (i',j') | i' <- [i], j' <- [(j+1)..boardSize] ]


rowHorizLeft :: Position -> PosRow
rowHorizLeft (i, j) =   
    PosRow $ [ (i',j') | i' <- [i], j' <- reverse [1..(j-1)] ]
        

rowDiagUpRight :: Position -> PosRow
rowDiagUpRight (i, j) =  
    rowDiag Dec Inc (i-1, j+1)


rowDiagUpLeft :: Position -> PosRow
rowDiagUpLeft (i, j) = 
    rowDiag Dec Dec (i-1, j-1)


rowDiagDownRight :: Position -> PosRow
rowDiagDownRight (i, j) = 
    rowDiag Inc Inc (i+1, j+1)


rowDiagDownLeft :: Position -> PosRow
rowDiagDownLeft (i, j) =
    rowDiag Inc Dec (i+1, j-1)


rowDiag :: Dir -> Dir -> Position -> PosRow
rowDiag horizDir vertDir (i,j) =
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
                    go (result ++ [(x, y)]) x' y'
    in
        PosRow $ go [] i j