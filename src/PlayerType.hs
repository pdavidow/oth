module PlayerType
    ( PlayerType(..) )
    where

import Engine ( Strategy )

 
data PlayerType
    = Person
    | Computer Strategy
        deriving (Eq, Show)