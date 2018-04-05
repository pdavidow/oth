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

data Tagged_Player
    = Tagged_PlayerBlack PlayerBlack
    | Tagged_PlayerWhite PlayerWhite
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
        Tagged_PlayerBlack (PlayerBlack x) -> x
        Tagged_PlayerWhite (PlayerWhite x) -> x


playerColor :: Tagged_Player -> Color
playerColor tagged =
    case tagged of
        Tagged_PlayerBlack _ -> Black
        Tagged_PlayerWhite _ -> White