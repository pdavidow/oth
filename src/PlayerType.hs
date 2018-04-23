module PlayerType
    ( PlayerType(..) )
    where

import Engine ( Strategy, SuggestionSearchDepth ) 

 
data PlayerType
    = Person SuggestionSearchDepth
    | Computer Strategy
        deriving (Eq, Show)