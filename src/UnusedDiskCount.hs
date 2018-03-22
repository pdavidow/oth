module UnusedDiskCount
    ( BlackUnusedDiskCount -- hiding constructor
    , WhiteUnusedDiskCount -- hiding constructor
    , All_UnusedDiskCount(..)
    , initialBlackUnusedDiskCount
    , initialWhiteUnusedDiskCount 
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


newtype UnusedDiskCount = UnusedDiskCount Int deriving (Eq, Show)

newtype BlackUnusedDiskCount = BlackUnusedDiskCount UnusedDiskCount deriving (Eq, Show)

newtype WhiteUnusedDiskCount = WhiteUnusedDiskCount UnusedDiskCount deriving (Eq, Show)

data All_UnusedDiskCount
    = BlackUnused BlackUnusedDiskCount
    | WhiteUnused WhiteUnusedDiskCount
        deriving (Eq, Show)


initialBlackUnusedDiskCount :: BlackUnusedDiskCount 
initialBlackUnusedDiskCount =
    BlackUnusedDiskCount initialUnusedDiskCount


initialWhiteUnusedDiskCount :: WhiteUnusedDiskCount 
initialWhiteUnusedDiskCount =
    WhiteUnusedDiskCount initialUnusedDiskCount


initialUnusedDiskCount :: UnusedDiskCount
initialUnusedDiskCount =
    UnusedDiskCount 32 -- assume boardSize 8


isZeroCount :: All_UnusedDiskCount -> Bool
isZeroCount tagged =
    let
        f :: UnusedDiskCount -> Bool
        f = \ (UnusedDiskCount n) -> n == 0
    in
        case tagged of
            BlackUnused (BlackUnusedDiskCount x) -> f x
            WhiteUnused (WhiteUnusedDiskCount x) -> f x
            

applyToCount :: (UnusedDiskCount -> UnusedDiskCount) -> All_UnusedDiskCount -> All_UnusedDiskCount
applyToCount f tagged =
    case tagged of
        BlackUnused (BlackUnusedDiskCount x) -> BlackUnused $ BlackUnusedDiskCount $ f x
        WhiteUnused (WhiteUnusedDiskCount x) -> WhiteUnused $ WhiteUnusedDiskCount $ f x


increaseByOne :: All_UnusedDiskCount -> All_UnusedDiskCount
increaseByOne tagged =
    tagged
        & applyToCount (\ (UnusedDiskCount n) -> UnusedDiskCount $ n + 1)


decreaseByOne :: All_UnusedDiskCount -> All_UnusedDiskCount
decreaseByOne tagged =
    tagged
        & applyToCount (\ d@(UnusedDiskCount n) -> if n == 0 then d else UnusedDiskCount $ n - 1)

        
transferDiskTo :: Color -> BlackUnusedDiskCount -> WhiteUnusedDiskCount -> Map.Map Color All_UnusedDiskCount
transferDiskTo color b w =
    let
        (taggedB', taggedW') = 
            case color of
                Black -> (increaseByOne $ BlackUnused b, decreaseByOne $ WhiteUnused w)
                White -> (decreaseByOne $ BlackUnused b, increaseByOne $ WhiteUnused w)
    in
        Map.empty
            & Map.insert Black taggedB'
            & Map.insert White taggedW'


decreaseByOneFor :: Color -> BlackUnusedDiskCount -> WhiteUnusedDiskCount -> Map.Map Color All_UnusedDiskCount
decreaseByOneFor color b w =
    let
        (taggedB', taggedW') = 
            case color of
                Black -> (decreaseByOne $ BlackUnused b, WhiteUnused w)
                White -> (BlackUnused b                , decreaseByOne $ WhiteUnused w)
    in
        Map.empty
            & Map.insert Black taggedB'
            & Map.insert White taggedW'


countFrom :: All_UnusedDiskCount -> Int
countFrom tagged =
    case tagged of
        BlackUnused (BlackUnusedDiskCount (UnusedDiskCount n)) -> n
        WhiteUnused (WhiteUnusedDiskCount (UnusedDiskCount n)) -> n   