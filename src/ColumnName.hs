module ColumnName
    ( columnLegend
    , posNomenclature
    )
    where

import Data.Ix as Ix ( Ix, index, range )
import Position ( Position )

-- as per http://www.boardgamecapital.com/game_rules/othello.pdf
data ColumnName = A | B | C | D | E | F | G | H  deriving (Eq, Ix, Ord, Show)


firstColumnIndex :: ColumnName
firstColumnIndex =
    A


lastColumnIndex :: ColumnName
lastColumnIndex =
    H


columnLegend :: String
columnLegend =
    concatMap (\x -> show x ++ "  ") columnIndexRange


columnIndexRange :: [ColumnName]
columnIndexRange = 
    Ix.range (firstColumnIndex, lastColumnIndex)


posNomenclature :: Position -> String
posNomenclature (i, j) =
    show (columnIndexRange !! (j - 1)) ++ (show i)
