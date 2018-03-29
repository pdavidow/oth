module Engine
    ( Strategy(..)
    , computerChoose
    )
    where

import System.Random  
import Data.Tree.Game_tree.Negascout ( alpha_beta_search, negamax ) 

import State ( MidState(..), EndState(..), Tagged_State(..), PriorMove(..), actual_NextMoves_FromTaggedState, isTaggedEndState )
import Board ( Move, dummyMove )


data Strategy 
    = RandomPick
    | SearchDepth_0
    | SearchDepth_1
    | SearchDepth_2
    | SearchDepth_3
    | SearchDepth_4
        deriving (Eq, Show)


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

            SearchDepth_0 -> return $ bestNextMove taggedState 0
            SearchDepth_1 -> return $ bestNextMove taggedState 1
            SearchDepth_2 -> return $ bestNextMove taggedState 2
            SearchDepth_3 -> return $ bestNextMove taggedState 3
            SearchDepth_4 -> return $ bestNextMove taggedState 4


bestNextMove :: Tagged_State -> Int -> Move
bestNextMove taggedState searchDepth =
    if isTaggedEndState taggedState then -- should never get here
        dummyMove
    else
        let
            bestNextState :: Maybe Tagged_State -- play it safe 
            bestNextState = 
                let
                    list = fst $ search taggedState searchDepth
                in
                    if null list then
                        Nothing
                    else
                        Just $ last list -- todo or is it head?
        in
            case bestNextState of
                Nothing                                                -> dummyMove
                Just (Tagged_StartState _)                             -> dummyMove -- should never get here
                Just (Tagged_MidState (MidState (PriorMove move) _ _)) -> move
                Just (Tagged_EndState (EndState (PriorMove move) _ _)) -> move


search :: Tagged_State -> Int -> ([Tagged_State], Int)
search taggedState searchDepth =
    alpha_beta_search taggedState searchDepth