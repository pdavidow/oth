module Lib
    ( mapTakeWhile
    , vSlice
    , getValidChoice
    )
    where

import Text.Read

import Data.Function ( (&) )
import Data.Vector ( fromList, slice, toList )

-- todo test
mapTakeWhile :: (a -> b) -> (b -> Bool) -> [a] -> [b]
mapTakeWhile _ _ [] = []
mapTakeWhile f p (x:xs) = let y = f x in if p y then y : mapTakeWhile f p xs else []        


vSlice :: Int -> Int -> [a] -> [a]
vSlice start len xs =
    xs
        & fromList 
        & slice start len
        & toList             


getValidChoice :: String -> [Int] -> IO Int
getValidChoice prompt options = do
    let same = getValidChoice prompt options

    let
        again :: IO Int -> IO Int
        again f = do
            putStrLn "Invalid input, try again...  "
            f

    putStrLn ""
    putStrLn prompt
    line <- getLine

    case (readMaybe line :: Maybe Int) of
        Just n ->
            if elem n options then do
                return n
            else do
                again same
        Nothing -> do
            again same