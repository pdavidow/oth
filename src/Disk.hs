module Disk
    ( Disk -- hiding constructor
    , Color(..)
    , diskColor
    , _flipCount
    , flipDisk
    , makeDisk
    , toggleColor
    , iconChar
    )
    where

data Disk = Disk {_initColor :: Color,  _flipCount :: Int} deriving (Eq, Show)

data Color = Black | White deriving (Eq, Ord, Show)


makeDisk :: Color -> Disk
makeDisk color = 
    Disk {_initColor = color,  _flipCount = 0}


diskColor :: Disk -> Color
diskColor disk =
    if even $ _flipCount disk then
        _initColor disk
    else
        toggleColor $ _initColor disk


toggleColor :: Color -> Color
toggleColor color = 
    case color of
        White -> Black
        Black -> White        
     

flipDisk :: Disk -> Disk
flipDisk (Disk initColor_dontTouch flipCount) =
    Disk initColor_dontTouch $ flipCount + 1       


iconChar :: Color -> Char
iconChar color =
    case color of
        Black -> 'X' -- 'x'
        White -> 'O' -- 'o'