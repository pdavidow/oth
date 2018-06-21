{-# LANGUAGE DuplicateRecordFields #-}

module BlackWhite
    ( BlackWhite(..)
    , BlackWhiteH(..)
    )
    where


data BlackWhite a = BlackWhite {black :: a, white :: a} deriving (Eq, Show)
data BlackWhiteH a b = BlackWhiteH {black :: a, white :: b} deriving (Eq, Show)