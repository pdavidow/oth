module Sequencer
    ( moveSequence
    ) 
    where 
 
import Safe ( atDef )
import Data.Maybe ( fromMaybe )
import Data.List ( find )

import Player ( PlayerBlack, PlayerWhite, Tagged_Player(..), playerTypeFrom, playerColor ) 
import PlayerType ( PlayerType(..) )
import State ( Tagged_State(..), applyMove, board_FromTaggedState, nextMoveColor_FromTaggedState, actual_NextMoves_FromTaggedState )
import Board ( Move, dummyMove, movePosChoices, movePos )
import Disk ( Color(..) )
import Position ( Position, makeValidPosition )
import Display ( movePosChoicesNomenclature, boardWithFlipCountDisplay, gameStateDisplay, gameSummaryDisplay, colorAllCapsString )
import Engine ( SuggestionSearchDepth(..), computerChoose, strategyDisplay, bestNextMove, searchPhrase )
import Lib ( getValidChoice ) 
import ColumnName ( posNomenclature )


moveSequence :: (PlayerBlack, PlayerWhite) -> Tagged_State -> IO ()
moveSequence players taggedState = do       
    let taggedPlayer = nextPlayer players taggedState
    putStrLn ""
    putStrLn $ gameStateDisplay Nothing taggedState

    move <- case playerTypeFrom taggedPlayer of
        Person suggestionSearchDepth -> do
            moveIndex <- personChoose (playerColor taggedPlayer) suggestionSearchDepth taggedState
            return $ atDef dummyMove (actual_NextMoves_FromTaggedState taggedState) moveIndex

        Computer strategy -> do 
            putStrLn $ "\nComputer (White) is working (" ++ (strategyDisplay strategy) ++ ") ...\n"
            computerChoose strategy taggedState

    advance players move taggedState
 

advance :: (PlayerBlack, PlayerWhite) -> Move -> Tagged_State -> IO ()
advance players move taggedState = do    
    let nextTaggedState = applyMove move taggedState

    case nextTaggedState of 
        Tagged_StartState _ -> moveSequence players nextTaggedState -- should never get here

        Tagged_MidState _   -> moveSequence players nextTaggedState

        Tagged_EndState x -> do
            putStrLn $ gameStateDisplay Nothing nextTaggedState
            putStrLn ""
            putStrLn $ gameSummaryDisplay x
            putStrLn ""
            putStrLn "########################################"
            putStrLn "FYI, here are the flip-counts:\n"
            putStrLn $ boardWithFlipCountDisplay $ board_FromTaggedState nextTaggedState
            putStrLn "########################################"
            return ()


nextPlayer :: (PlayerBlack, PlayerWhite) -> Tagged_State -> Tagged_Player
nextPlayer (pb, pw) taggedState =
    case fromMaybe Black $ nextMoveColor_FromTaggedState taggedState of
        Black -> Tagged_PlayerBlack pb
        White -> Tagged_PlayerWhite pw


personChoose :: Color -> SuggestionSearchDepth -> Tagged_State -> IO Int
personChoose color suggestionSearchDepth taggedState = do
    let moves = actual_NextMoves_FromTaggedState taggedState
    let numberedMovesWithPos = movePosChoices moves
    n <- handlePersonChoose color numberedMovesWithPos moves suggestionSearchDepth taggedState
    return n


handlePersonChoose :: Color -> [(Int, Position)] -> [Move] -> SuggestionSearchDepth -> Tagged_State -> IO Int
handlePersonChoose color numberedMovesWithPos moves s@(SuggestionSearchDepth searchDepth) taggedState  = do
    n <- getMoveChoice color numberedMovesWithPos

    if n == choiceNumberFor_DisplayChoicesOnBoard then do
        putStrLn ""
        putStrLn $ gameStateDisplay (Just numberedMovesWithPos) taggedState
        handlePersonChoose color numberedMovesWithPos moves s taggedState 
    else if n == choiceNumberFor_Suggest then do
        let pos = movePos $ bestNextMove searchDepth taggedState
        let index = fst $ fromMaybe (0, makeValidPosition 1 1) $ find (\(_, pos') -> pos == pos') numberedMovesWithPos
        putStrLn $ "\nComputer suggests (after " ++ searchPhrase searchDepth ++ "): " ++ movePosChoicesNomenclature [(index, pos)]
        handlePersonChoose color numberedMovesWithPos moves s taggedState 
    else do 
        return $ n - 1 -- index is zero-based


getMoveChoice :: Color -> [(Int, Position)] -> IO Int
getMoveChoice color numberedMovesWithPos = do
    let posTags = Prelude.map fst numberedMovesWithPos
    let options = choiceNumberFor_DisplayChoicesOnBoard : choiceNumberFor_Suggest : posTags
    let nomenclature = movePosChoicesNomenclature numberedMovesWithPos
    let prompt = (colorAllCapsString color) ++ " Options: (" ++ show choiceNumberFor_DisplayChoicesOnBoard ++ ":show, " ++ show choiceNumberFor_Suggest ++ ":suggest) " ++ nomenclature ++ "\nEnter choice"
    getValidChoice prompt options


choiceNumberFor_DisplayChoicesOnBoard :: Int
choiceNumberFor_DisplayChoicesOnBoard = 
    99


choiceNumberFor_Suggest :: Int
choiceNumberFor_Suggest = 
    100