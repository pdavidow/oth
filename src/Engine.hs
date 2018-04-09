{-# LANGUAGE InstanceSigs #-}

module Engine
    ( Strategy(..)
    , computerChoose
    , stratDisplay
    )
    where

import System.Random  
import Data.Tree.Game_tree.Negascout ( alpha_beta_search ) 
import Safe ( headMay, tailMay )

import State ( MidState(..), EndState(..), Tagged_State(..), PriorMove(..), actual_NextMoves_FromTaggedState, isTaggedEndState )
import Board ( Move, dummyMove )


data Strategy 
    = RandomPick
    | SearchDepth_1
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


stratDisplay :: Strategy -> String
stratDisplay strat =
    case strat of
        RandomPick     -> "random choice"
        SearchDepth_1  -> "searching 1 level deep"
        SearchDepth_2  -> "searching 2 levels deep"
        SearchDepth_3  -> "searching 3 levels deep"
        SearchDepth_4  -> "searching 4 levels deep"
        SearchDepth_5  -> "searching 5 levels deep"
        SearchDepth_6  -> "searching 6 levels deep"
        SearchDepth_7  -> "searching 7 levels deep"
        SearchDepth_8  -> "searching 8 levels deep"
        SearchDepth_9  -> "searching 9 levels deep"
        SearchDepth_10 -> "searching 10 levels deep"

computerChoose :: Strategy -> Tagged_State -> IO Move 
computerChoose strat taggedState = do
    if isTaggedEndState taggedState then do -- should never get here
        return dummyMove
    else do
        case strat of
            RandomPick -> do 
                gen <- getStdGen  
                let moves = actual_NextMoves_FromTaggedState taggedState
                let (randN, _) = randomR (0, length moves - 1) gen :: (Int, StdGen)
                return $ moves !! randN

            SearchDepth_1  -> return $ bestNextMove taggedState 1
            SearchDepth_2  -> return $ bestNextMove taggedState 2
            SearchDepth_3  -> return $ bestNextMove taggedState 3
            SearchDepth_4  -> return $ bestNextMove taggedState 4
            SearchDepth_5  -> return $ bestNextMove taggedState 5
            SearchDepth_6  -> return $ bestNextMove taggedState 6
            SearchDepth_7  -> return $ bestNextMove taggedState 7
            SearchDepth_8  -> return $ bestNextMove taggedState 8
            SearchDepth_9  -> return $ bestNextMove taggedState 9
            SearchDepth_10 -> return $ bestNextMove taggedState 10

            
bestNextMove :: Tagged_State -> Int -> Move
bestNextMove taggedState searchDepth =
    if isTaggedEndState taggedState then -- should never get here
        dummyMove
    else
        let
            bestNextState :: Maybe Tagged_State 
            bestNextState = 
                (return $ fst $ alpha_beta_search taggedState searchDepth) 
                    >>= tailMay >>= headMay
        in
            case bestNextState of
                Nothing                                                -> dummyMove
                Just (Tagged_StartState _)                             -> dummyMove -- should never get here
                Just (Tagged_MidState (MidState (PriorMove move) _ _)) -> move
                Just (Tagged_EndState (EndState (PriorMove move) _ _)) -> move