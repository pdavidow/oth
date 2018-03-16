module Lib
    ( vSlice
    )
    where

import Data.Function ( (&) )
import Data.Vector ( fromList, slice, toList )


vSlice :: Int -> Int -> [a] -> [a]
vSlice start len xs =
    xs
        & fromList 
        & slice start len
        & toList 