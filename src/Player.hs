module Player
    ( PlayerBlack -- hiding constructor
    , PlayerWhite -- hiding constructor
    , Tagged_Player(..)
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

-- todo tagged nomen
data Tagged_Player
    = BlackPlayerTag PlayerBlack
    | WhitePlayerTag PlayerWhite
        deriving (Eq, Show)


makePlayerBlack :: PlayerType -> PlayerBlack
makePlayerBlack x =
    PlayerBlack x


makePlayerWhite :: PlayerType -> PlayerWhite
makePlayerWhite x =
    PlayerWhite x


playerTypeFrom :: Tagged_Player -> PlayerType
playerTypeFrom tagged =
    case tagged of
        BlackPlayerTag (PlayerBlack x) -> x
        WhitePlayerTag (PlayerWhite x) -> x


playerColor :: Tagged_Player -> Color
playerColor tagged =
    case tagged of
        BlackPlayerTag _ -> Black
        WhitePlayerTag _ -> White