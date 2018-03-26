module Player
    ( PlayerBlack -- hiding constructor
    , PlayerWhite -- hiding constructor
    , All_Player(..)
    , makePlayerBlack
    , makePlayerWhite
    , playerTypeFrom
    , playerColor
    )
    where

import Disk ( Color(..) )
import PlayerType ( PlayerType(..) )


data PlayerBlack = PlayerBlack PlayerType deriving (Eq, Show)

data PlayerWhite = PlayerWhite PlayerType deriving (Eq, Show)

data All_Player
    = BlackPlayerTag PlayerBlack
    | WhitePlayerTag PlayerWhite
        deriving (Eq, Show)


makePlayerBlack :: PlayerType -> PlayerBlack
makePlayerBlack x =
    PlayerBlack x


makePlayerWhite :: PlayerType -> PlayerWhite
makePlayerWhite x =
    PlayerWhite x


playerTypeFrom :: All_Player -> PlayerType
playerTypeFrom tagged =
    case tagged of
        BlackPlayerTag (PlayerBlack x) -> x
        WhitePlayerTag (PlayerWhite x) -> x


playerColor :: All_Player -> Color
playerColor tagged =
    case tagged of
        BlackPlayerTag _ -> Black
        WhitePlayerTag _ -> White