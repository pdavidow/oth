module ColumnName
    ( columnLegend
    , posNomenclature
    )
    where

import Data.Ix as Ix ( Ix, range )
import Safe ( atDef )

import Position ( Position, posCoords )


-- can be auto-generated with Template-Haskell
data ColumnName = A | B | C | D | E | F | G | H  deriving (Eq, Ix, Ord, Show)


firstColumnIndex :: ColumnName
firstColumnIndex =
    A


lastColumnIndex :: ColumnName
lastColumnIndex =
    H


columnLegend :: String
columnLegend =
    concatMap (\x -> show x ++ "    ") columnIndexRange


columnIndexRange :: [ColumnName]
columnIndexRange = 
    Ix.range (firstColumnIndex, lastColumnIndex)


posNomenclature :: Position -> String
posNomenclature pos = 
    show (atDef firstColumnIndex columnIndexRange (j - 1)) ++ (show i)
        where (i,j) = posCoords pos