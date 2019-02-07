{-# LANGUAGE DataKinds #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE TemplateHaskell #-}

module Position
    ( Position(..)
    , PosRow(..)
    , BoardCoord
    , mkPosition
    , posCoords
    , adjacentPositions
    , radiatingPosRows
    , isValidCoords
    )
    where

-- Origin is at top left of board, where x is vertical axis, y horizontal 

import Data.Either ( rights )
import Data.Function ( (&) )
import GHC.TypeLits (Nat)
import Language.Haskell.TH
import Refined ( Refined(..), FromTo, refineTH, unrefine )

import BoardSize ( boardSize )

data Position = Position BoardCoord BoardCoord deriving (Eq, Show) -- one-based

newtype PosRow = PosRow [Position] deriving (Eq, Show)

data Dir = Inc | Dec

type BoardCoord = Refined FromTo (1 :: Nat) (8 :: Nat) ------------------------------------------(boardSize :: Nat)  


mkBoardCoord :: Int -> BoardCoord
mkBoardCoord n =
    $$(refineTH (n :: Nat)) :: BoardCoord


mkPosition :: Int -> Int -> Position
mkPosition i j =
    Position (mkBoardCoord i) (mkBoardCoord j)


posCoords :: Position -> (Int, Int)
posCoords (Position i j) = 
    (unrefine i, unrefine j)


adjacentPositions :: Position -> [Position]
adjacentPositions (Position i j) =
    [ (i-1, j-1), (i, j-1), (i+1, j-1)
    , (i-1, j  ),           (i+1, j  )
    , (i-1, j+1), (i, j+1), (i+1, j+1)
    ]
        & map (\ (i', j') -> mkPosition i' j')
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
    PosRow $ [ mkPosition i' j' | i' <- reverse [1..(unrefine i - 1)], j' <- [unrefine j] ]


rowVertDown :: Position -> PosRow
rowVertDown (Position i j) =   
    PosRow $ [ mkPosition i' j' | i' <- [(unrefine i + 1)..boardSize], j' <- [unrefine j] ]


rowHorizRight :: Position -> PosRow
rowHorizRight (Position i j) =   
    PosRow $ [ mkPosition i' j' | i' <- [unrefine i], j' <- [(unrefine j + 1)..boardSize] ]


rowHorizLeft :: Position -> PosRow
rowHorizLeft (Position i j) =   
    PosRow $ [ mkPosition i' j' | i' <- [unrefine i], j' <- reverse [1..(unrefine j - 1)] ]
        

rowDiagUpRight :: Position -> PosRow
rowDiagUpRight (Position i j) =  
    rowDiag Dec Inc $ mkPosition (unrefine i - 1) (unrefine j + 1)


rowDiagUpLeft :: Position -> PosRow
rowDiagUpLeft (Position i j) = 
    rowDiag Dec Dec $ mkPosition (unrefine i - 1) (unrefine j - 1)  


rowDiagDownRight :: Position -> PosRow
rowDiagDownRight (Position i j) = 
    rowDiag Inc Inc $ mkPosition (unrefine i + 1) (unrefine j + 1) 


rowDiagDownLeft :: Position -> PosRow
rowDiagDownLeft (Position i j) =
    rowDiag Inc Dec $ mkPosition (unrefine i + 1) (unrefine j - 1) 


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
                    go (result ++ [mkPosition x y]) x' y'
    in
        PosRow $ go [] (unrefine i) (unrefine j)