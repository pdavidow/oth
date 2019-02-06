module BlackWhite
    ( BlackWhite(..)
    , makeBlackWhite
    )
    where


data BlackWhite a = BlackWhite a a deriving (Eq, Show)


makeBlackWhite :: a -> a -> BlackWhite a
makeBlackWhite b w =
    BlackWhite b w