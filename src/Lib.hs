module Lib
    ( slice
    )
    where

import Data.List

-- https://stackoverflow.com/questions/4597820/does-haskell-have-list-slices-i-e-python
slice :: Int -> Int -> [a] -> [a]
slice start stop xs = fst $ splitAt (stop - start) (snd $ splitAt start xs)