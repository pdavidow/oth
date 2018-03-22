module SquareCount
    ( BlackSquareCount -- hiding constructor
    , WhiteSquareCount -- hiding constructor
    , All_SquareCount(..)
    , makeBlackSquareCount
    , makeWhiteSquareCount
    , countFrom
    )
    where


newtype SquareCount = SquareCount Int deriving (Eq, Show)

newtype BlackSquareCount = BlackSquareCount SquareCount deriving (Eq, Show)

newtype WhiteSquareCount = WhiteSquareCount SquareCount deriving (Eq, Show)

data All_SquareCount
    = Tagged_BlackSquareCount BlackSquareCount
    | Tagged_WhiteSquareCount WhiteSquareCount
        deriving (Eq, Show)


makeBlackSquareCount :: Int -> BlackSquareCount
makeBlackSquareCount n =
    BlackSquareCount $ SquareCount n


makeWhiteSquareCount :: Int -> WhiteSquareCount
makeWhiteSquareCount n =
    WhiteSquareCount $ SquareCount n


countFrom :: All_SquareCount -> Int
countFrom tagged =
    case tagged of
        Tagged_BlackSquareCount (BlackSquareCount (SquareCount n)) -> n
        Tagged_WhiteSquareCount (WhiteSquareCount (SquareCount n)) -> n