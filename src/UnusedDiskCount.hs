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
    , applyToUnusedDiskCounts
    )
    where

import Data.Function ( (&) )
import Disk ( Color(..) )

import BoardSize ( boardSize )
import BlackWhite ( BlackWhite(..), makeBlackWhite, blacksWhites )


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


applyToUnusedDiskCounts 
    :: (BlackWhite Tagged_UnusedDiskCount -> BlackWhite Tagged_UnusedDiskCount) 
    -> ( BlackUnusedDiskCount, WhiteUnusedDiskCount )
    -> ( BlackUnusedDiskCount, WhiteUnusedDiskCount )
applyToUnusedDiskCounts f ( b, w ) =
    let
        ( (Tagged_BlackUnusedDiskCount b'), (Tagged_WhiteUnusedDiskCount w') ) = makeBlackWhite (Tagged_BlackUnusedDiskCount b) (Tagged_WhiteUnusedDiskCount w)
            & f
            & blacksWhites
    in
        ( b',  w' )


increaseByOne :: Tagged_UnusedDiskCount -> Tagged_UnusedDiskCount
increaseByOne tagged =
    tagged
        & applyToCount (\ (UnusedDiskCount n) -> UnusedDiskCount $ n + 1)


decreaseByOne :: Tagged_UnusedDiskCount -> Tagged_UnusedDiskCount
decreaseByOne tagged =
    tagged
        & applyToCount (\ d@(UnusedDiskCount n) -> if n == 0 then d else UnusedDiskCount $ n - 1)

        
transferDiskTo :: Color -> BlackWhite Tagged_UnusedDiskCount -> BlackWhite Tagged_UnusedDiskCount
transferDiskTo color bw =
    let
        ( b, w ) = blacksWhites bw

        ( b', w' ) = 
            case color of
                Black -> ( increaseByOne b, decreaseByOne w )
                White -> ( decreaseByOne b, increaseByOne w )
    in
        makeBlackWhite b' w'


decreaseByOneFor :: Color -> BlackWhite Tagged_UnusedDiskCount -> BlackWhite Tagged_UnusedDiskCount
decreaseByOneFor color bw =
    let
        ( b, w ) = blacksWhites bw

        ( b', w' ) = 
            case color of
                Black -> ( decreaseByOne b, w )
                White -> ( b, decreaseByOne w )
    in
        makeBlackWhite b' w'


countFrom :: Tagged_UnusedDiskCount -> Int
countFrom tagged =
    case tagged of
        Tagged_BlackUnusedDiskCount (BlackUnusedDiskCount (UnusedDiskCount n)) -> n
        Tagged_WhiteUnusedDiskCount (WhiteUnusedDiskCount (UnusedDiskCount n)) -> n   