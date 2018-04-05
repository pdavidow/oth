module UnusedDiskCount
    ( BlackUnusedDiskCount -- hiding constructor
    , WhiteUnusedDiskCount -- hiding constructor
    , Tagged_UnusedDiskCount(..)
    , makeBlackUnusedDiskCount
    , makeWhiteUnusedDiskCount 
    , decreaseByOne
    , isZeroCount
    , transferDiskTo
    , decreaseByOneFor
    , countFrom
    )
    where

import Data.Function ( (&) )
import Disk ( Color(..) )
import qualified Data.Map.Strict as Map ( Map, empty, insert )

import BoardSize ( boardSize )


newtype UnusedDiskCount = UnusedDiskCount Int deriving (Eq, Show)

newtype BlackUnusedDiskCount = BlackUnusedDiskCount UnusedDiskCount deriving (Eq, Show)

newtype WhiteUnusedDiskCount = WhiteUnusedDiskCount UnusedDiskCount deriving (Eq, Show)

data Tagged_UnusedDiskCount
    = Tagged_BlackUnusedDiskCount BlackUnusedDiskCount
    | Tagged_WhiteUnusedDiskCount WhiteUnusedDiskCount
        deriving (Eq, Show)


makeBlackUnusedDiskCount :: BlackUnusedDiskCount 
makeBlackUnusedDiskCount =
    BlackUnusedDiskCount initUnusedDiskCount


makeWhiteUnusedDiskCount :: WhiteUnusedDiskCount 
makeWhiteUnusedDiskCount =
    WhiteUnusedDiskCount initUnusedDiskCount


initUnusedDiskCount :: UnusedDiskCount
initUnusedDiskCount =
    UnusedDiskCount $ div (boardSize * boardSize) 2 -- apparently


isZeroCount :: Tagged_UnusedDiskCount -> Bool
isZeroCount tagged =
    let
        f :: UnusedDiskCount -> Bool
        f = \ (UnusedDiskCount n) -> n == 0
    in
        case tagged of
            Tagged_BlackUnusedDiskCount (BlackUnusedDiskCount x) -> f x
            Tagged_WhiteUnusedDiskCount (WhiteUnusedDiskCount x) -> f x
            

applyToCount :: (UnusedDiskCount -> UnusedDiskCount) -> Tagged_UnusedDiskCount -> Tagged_UnusedDiskCount
applyToCount f tagged =
    case tagged of
        Tagged_BlackUnusedDiskCount (BlackUnusedDiskCount x) -> Tagged_BlackUnusedDiskCount $ BlackUnusedDiskCount $ f x
        Tagged_WhiteUnusedDiskCount (WhiteUnusedDiskCount x) -> Tagged_WhiteUnusedDiskCount $ WhiteUnusedDiskCount $ f x


increaseByOne :: Tagged_UnusedDiskCount -> Tagged_UnusedDiskCount
increaseByOne tagged =
    tagged
        & applyToCount (\ (UnusedDiskCount n) -> UnusedDiskCount $ n + 1)


decreaseByOne :: Tagged_UnusedDiskCount -> Tagged_UnusedDiskCount
decreaseByOne tagged =
    tagged
        & applyToCount (\ d@(UnusedDiskCount n) -> if n == 0 then d else UnusedDiskCount $ n - 1)

        
transferDiskTo :: Color -> BlackUnusedDiskCount -> WhiteUnusedDiskCount -> Map.Map Color Tagged_UnusedDiskCount
transferDiskTo color b w =
    let
        (taggedB', taggedW') = 
            case color of
                Black -> (increaseByOne $ Tagged_BlackUnusedDiskCount b, decreaseByOne $ Tagged_WhiteUnusedDiskCount w)
                White -> (decreaseByOne $ Tagged_BlackUnusedDiskCount b, increaseByOne $ Tagged_WhiteUnusedDiskCount w)
    in
        Map.empty
            & Map.insert Black taggedB'
            & Map.insert White taggedW'


decreaseByOneFor :: Color -> BlackUnusedDiskCount -> WhiteUnusedDiskCount -> Map.Map Color Tagged_UnusedDiskCount
decreaseByOneFor color b w =
    let
        (taggedB', taggedW') = 
            case color of
                Black -> (decreaseByOne $ Tagged_BlackUnusedDiskCount b, Tagged_WhiteUnusedDiskCount w)
                White -> (Tagged_BlackUnusedDiskCount b                , decreaseByOne $ Tagged_WhiteUnusedDiskCount w)
    in
        Map.empty
            & Map.insert Black taggedB'
            & Map.insert White taggedW'


countFrom :: Tagged_UnusedDiskCount -> Int
countFrom tagged =
    case tagged of
        Tagged_BlackUnusedDiskCount (BlackUnusedDiskCount (UnusedDiskCount n)) -> n
        Tagged_WhiteUnusedDiskCount (WhiteUnusedDiskCount (UnusedDiskCount n)) -> n   