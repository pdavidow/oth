module UnusedDiskCount
    ( UnusedDiskCounts
    , makeUnusedDiskCounts
    , transferDiskTo
    , decreaseByOneFor
    )
    where

import Data.Function ( (&) )
import Color ( Color(..) )

import BoardSize ( boardSize )
import BlackWhite ( BlackWhite(..), makeBlackWhite ) 


type UnusedDiskCounts = BlackWhite Int


makeUnusedDiskCounts :: UnusedDiskCounts
makeUnusedDiskCounts =
    makeBlackWhite maxDiskCount maxDiskCount


maxDiskCount :: Int
maxDiskCount =
    div (boardSize * boardSize) 2 -- apparently

     
transferDiskTo :: Color -> UnusedDiskCounts -> UnusedDiskCounts
transferDiskTo color (BlackWhite b w) =
    case color of
        Black -> makeBlackWhite (b + 1) (subtractOneForPositive w)
        White -> makeBlackWhite (subtractOneForPositive b) (w + 1)
            

decreaseByOneFor :: Color -> UnusedDiskCounts -> UnusedDiskCounts
decreaseByOneFor color (BlackWhite b w) =
    case color of
        Black -> makeBlackWhite (subtractOneForPositive b) w
        White -> makeBlackWhite b (subtractOneForPositive w)  
        

subtractOneForPositive :: Int -> Int
subtractOneForPositive n =
    if n < 1 then n else n - 1  