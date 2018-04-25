module Engine
    ( Strategy(..)
    , SearchDepth(..)
    , SuggestionSearchDepth(..)
    , computerChoose
    , strategyDisplay
    , bestNextMove
    , searchPhrase
    )
    where

import System.Random  
import Data.Tree.Game_tree.Negascout ( alpha_beta_search ) 
import Safe ( atDef, headMay, tailMay )
import Language.English.Plural ( tryPlural ) 

import State ( MidState(..), EndState(..), Tagged_State(..), PriorMove(..), actual_NextMoves_FromTaggedState )
import Board ( Move, dummyMove )

data Strategy 
    = RandomPick
    | SearchDepth SearchDepth
        deriving (Eq, Show)

newtype SuggestionSearchDepth = SuggestionSearchDepth SearchDepth deriving (Eq, Show)

data SearchDepth 
    = SearchDepth_1
    | SearchDepth_2
    | SearchDepth_3
    | SearchDepth_4
    | SearchDepth_5
    | SearchDepth_6
    | SearchDepth_7
    | SearchDepth_8
    | SearchDepth_9
    | SearchDepth_10
        deriving (Eq, Show)


depthLevel :: SearchDepth -> Int
depthLevel searchDepth =
    case searchDepth of
        SearchDepth_1  ->  1
        SearchDepth_2  ->  2
        SearchDepth_3  ->  3
        SearchDepth_4  ->  4
        SearchDepth_5  ->  5
        SearchDepth_6  ->  6
        SearchDepth_7  ->  7
        SearchDepth_8  ->  8
        SearchDepth_9  ->  9
        SearchDepth_10 -> 10


strategyDisplay :: Strategy -> String
strategyDisplay strategy =
    case strategy of
        RandomPick -> "random choice"
        SearchDepth searchDepth -> searchPhrase searchDepth


searchPhrase :: SearchDepth -> String
searchPhrase searchDepth =
    "searching " ++ show depth ++ " " ++ (tryPlural depth "level") ++ " deep"
        where depth = depthLevel searchDepth


computerChoose :: Strategy -> Tagged_State -> IO Move 
computerChoose strat taggedState = do
    case taggedState of
        Tagged_EndState _ -> do 
            return dummyMove -- should never get here

        _ -> do
            case strat of
                RandomPick -> do 
                    gen <- getStdGen  
                    let moves = actual_NextMoves_FromTaggedState taggedState
                    let (randN, _) = randomR (0, length moves - 1) gen :: (Int, StdGen)
                    return $ atDef dummyMove moves randN

                SearchDepth searchDepth ->
                    return $ bestNextMove searchDepth taggedState


bestNextMove :: SearchDepth -> Tagged_State -> Move
bestNextMove searchDepth taggedState =
    case taggedState of
        Tagged_EndState _ ->  
            dummyMove -- should never get here

        _ -> 
            let
                bestNextState :: Maybe Tagged_State 
                bestNextState = 
                    (pure $ fst $ alpha_beta_search taggedState $ depthLevel searchDepth) 
                        >>= tailMay >>= headMay
            in
                case bestNextState of
                    Nothing                                                  -> dummyMove
                    Just (Tagged_StartState _)                               -> dummyMove -- should never get here
                    Just (Tagged_MidState (MidState (PriorMove move) _ _ _)) -> move
                    Just (Tagged_EndState (EndState (PriorMove move) _ _))   -> move