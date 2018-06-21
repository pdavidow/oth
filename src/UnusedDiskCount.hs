module UnusedDiskCount
    ( BlackUnusedDiskCount -- hiding constructor
    , WhiteUnusedDiskCount -- hiding constructor
    , Tagged_UnusedDiskCount(..)
    , UnusedDiskCounts
    , makeUnusedDiskCounts
    , isZeroCount
    , transferDiskTo
    , decreaseByOneFor
    , countFrom
    )
    where

import Data.Function ( (&) )
import Disk ( Color(..) )

import BoardSize ( boardSize )
import BlackWhite ( BlackWhiteH(..) ) 


newtype BlackUnusedDiskCount = BlackUnusedDiskCount Int deriving (Eq, Show)

newtype WhiteUnusedDiskCount = WhiteUnusedDiskCount Int deriving (Eq, Show)

type UnusedDiskCounts = BlackWhiteH BlackUnusedDiskCount WhiteUnusedDiskCount 

data Tagged_UnusedDiskCount
    = Tagged_BlackUnusedDiskCount BlackUnusedDiskCount
    | Tagged_WhiteUnusedDiskCount WhiteUnusedDiskCount
        deriving (Eq, Show)


makeUnusedDiskCounts :: UnusedDiskCounts
makeUnusedDiskCounts =
    BlackWhiteH 
        (BlackUnusedDiskCount initUnusedDiskCount) 
        (WhiteUnusedDiskCount initUnusedDiskCount)


initUnusedDiskCount :: Int
initUnusedDiskCount =
    div (boardSize * boardSize) 2 -- apparently


isZeroCount :: Tagged_UnusedDiskCount -> Bool
isZeroCount tagged =
    let
        f :: Int -> Bool
        f = \ n -> n == 0
    in
        case tagged of
            Tagged_BlackUnusedDiskCount (BlackUnusedDiskCount x) -> f x
            Tagged_WhiteUnusedDiskCount (WhiteUnusedDiskCount x) -> f x

     
transferDiskTo :: Color -> UnusedDiskCounts -> UnusedDiskCounts
transferDiskTo color (BlackWhiteH (BlackUnusedDiskCount b) (WhiteUnusedDiskCount w)) =
    case color of
        Black -> BlackWhiteH (BlackUnusedDiskCount $ b + 1) (WhiteUnusedDiskCount $ subtractOneForPositive w)
        White -> BlackWhiteH (BlackUnusedDiskCount $ subtractOneForPositive b) (WhiteUnusedDiskCount $ w + 1)
            

decreaseByOneFor :: Color -> UnusedDiskCounts -> UnusedDiskCounts
decreaseByOneFor color (BlackWhiteH bc@(BlackUnusedDiskCount b) wc@(WhiteUnusedDiskCount w)) =
    case color of
        Black -> BlackWhiteH (BlackUnusedDiskCount $ subtractOneForPositive b) wc
        White -> BlackWhiteH bc (WhiteUnusedDiskCount $ subtractOneForPositive w)
        

subtractOneForPositive :: Int -> Int
subtractOneForPositive n =
    if n < 1 then n else n - 1


countFrom :: Tagged_UnusedDiskCount -> Int
countFrom tagged =
    case tagged of
        Tagged_BlackUnusedDiskCount (BlackUnusedDiskCount n) -> n
        Tagged_WhiteUnusedDiskCount (WhiteUnusedDiskCount n) -> n   