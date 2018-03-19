module Lib
    ( mapTakeWhile
    , vSlice
    )
    where

import Data.Function ( (&) )
import Data.Vector ( fromList, slice, toList )


mapTakeWhile :: (a -> b) -> (b -> Bool) -> [a] -> [b]
mapTakeWhile _ _ [] =  []
mapTakeWhile f p (x:xs) =
    let
        result = f x
    in
        if p result then
            result : mapTakeWhile f p xs
        else
            []        


vSlice :: Int -> Int -> [a] -> [a]
vSlice start len xs =
    xs
        & fromList 
        & slice start len
        & toList             