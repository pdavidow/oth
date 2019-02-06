module Disk
    ( Disk -- hiding constructor
    , diskColor
    , flipCount
    , flipDisk
    , makeDisk
    , toggleColor
    )
    where

import Color ( Color(..) )


data Disk = Disk InitColor FlipCount deriving (Eq, Show)

newtype InitColor = InitColor Color deriving (Eq, Show)

newtype FlipCount = FlipCount Int deriving (Eq, Show)


makeDisk :: Color -> Disk
makeDisk color = 
    Disk (InitColor color) (FlipCount 0)


diskColor :: Disk -> Color
diskColor (Disk (InitColor color) (FlipCount count)) =
    if even count then 
        color
    else 
        toggleColor color


toggleColor :: Color -> Color
toggleColor color = 
    case color of
        White -> Black
        Black -> White        
     

flipDisk :: Disk -> Disk
flipDisk (Disk initColor (FlipCount count)) =
    Disk initColor $ FlipCount $ count + 1       


flipCount :: Disk -> Int
flipCount (Disk _ (FlipCount x)) =
    x