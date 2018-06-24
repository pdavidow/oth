module Sequencer
    ( moveSequence
    ) 
    where 
 
import Safe ( atDef )
import Data.Maybe ( fromMaybe, isJust )
import Data.List ( find )
import Data.Either ( fromLeft, fromRight, isRight )
import qualified Data.List.NonEmpty as NE ( NonEmpty, last, fromList, toList )

import Player ( PlayerBlack, PlayerWhite, Tagged_Player(..), playerTypeFrom, playerColor ) 
import PlayerType ( PlayerType(..) )
import State ( Tagged_State(..), EndState, MoveValidationError(..), History, applyMoveOnHistory, board_FromTaggedState, nextMoveColor_FromTaggedState, actual_NextMoves_FromTaggedState, undoHistoryOnceForColor )
import Board ( Move, dummyMove, movePosChoices, movePos )
import Disk ( Color(..) )
import Position ( Position, makeValidPosition )
import Display ( movePosChoicesNomenclature, boardWithFlipCountDisplay, gameStateDisplay, gameSummaryDisplay, colorAllCapsString )
import Engine ( SuggestionSearchDepth(..), computerChoose, strategyDisplay, bestNextMove, searchPhrase )
import Lib ( getValidChoice ) 

 
moveSequence :: (PlayerBlack, PlayerWhite) -> History -> IO ()
moveSequence players history = do 
    let taggedState = NE.last history     
    let taggedPlayer = nextPlayer players taggedState
    putStrLn ""
    putStrLn $ gameStateDisplay Nothing taggedState

    (move, history') <- case playerTypeFrom taggedPlayer of
        Person suggestionSearchDepth -> do
            (moveIndex, history') <- personChoose (playerColor taggedPlayer) suggestionSearchDepth history
            let move = atDef dummyMove (actual_NextMoves_FromTaggedState $ NE.last history') moveIndex
            return $ (move, history')

        Computer strategy -> do 
            putStrLn $ "\nComputer (White) is working (" ++ (strategyDisplay strategy) ++ ") ...\n"
            move <- computerChoose strategy taggedState
            return $ (move, history)

    advance players move history'
 

advance :: (PlayerBlack, PlayerWhite) -> Move -> History -> IO ()
advance players move history = do  
    let eiHistory' = applyMoveOnHistory move history

    if isRight eiHistory' then do
        let history' = fromRight history eiHistory' -- should never use default value
        let taggedState = NE.last history'
        
        case taggedState of 
            Tagged_StartState _ -> moveSequence players history' -- should never get here
            Tagged_MidState _   -> moveSequence players history'
            Tagged_EndState x   -> displayEndSummary x
    else -- should never get here (in theory), due to constrained interface
        reportMoveErrors $ fromLeft (NE.fromList [DefaultDummy]) eiHistory' -- should never use default value


nextPlayer :: (PlayerBlack, PlayerWhite) -> Tagged_State -> Tagged_Player
nextPlayer (pb, pw) taggedState =
    case fromMaybe Black $ nextMoveColor_FromTaggedState taggedState of
        Black -> Tagged_PlayerBlack pb
        White -> Tagged_PlayerWhite pw


personChoose :: Color -> SuggestionSearchDepth -> History -> IO (Int, History)
personChoose color suggestionSearchDepth history = do
    let moves = actual_NextMoves_FromTaggedState $ NE.last history   
    let numberedMovesWithPos = movePosChoices moves
    (n, history') <- handlePersonChoose color numberedMovesWithPos moves suggestionSearchDepth history
    return (n, history')


handlePersonChoose :: Color -> [(Int, Position)] -> [Move] -> SuggestionSearchDepth -> History -> IO (Int, History)
handlePersonChoose color numberedMovesWithPos moves s@(SuggestionSearchDepth searchDepth) history = do
    let taggedState = NE.last history     
    let mbHistory' = undoHistoryOnceForColor color history 
    let isUndoable = isJust mbHistory'
    n <- getMoveChoice color isUndoable numberedMovesWithPos

    if n == choiceNumberFor_DisplayChoicesOnBoard then do
        putStrLn ""
        putStrLn $ gameStateDisplay (Just numberedMovesWithPos) taggedState
        handlePersonChoose color numberedMovesWithPos moves s history
    else if n == choiceNumberFor_Suggest then do
        let pos = movePos $ bestNextMove searchDepth taggedState
        let index = fst $ fromMaybe (0, makeValidPosition 1 1) $ find (\(_, pos') -> pos == pos') numberedMovesWithPos
        putStrLn $ "\nComputer suggests (after " ++ searchPhrase searchDepth ++ "): " ++ movePosChoicesNomenclature [(index, pos)]
        handlePersonChoose color numberedMovesWithPos moves s history
    else if n == choiceNumberFor_Undo then do
        let history' = fromMaybe history mbHistory' -- should never use default
        putStrLn ""
        putStrLn $ gameStateDisplay Nothing $ NE.last history'
        personChoose color s history' -- color remains the same for undo
    else do 
        return $ (n - 1, history) -- index is zero-based


getMoveChoice :: Color -> Bool -> [(Int, Position)] -> IO Int
getMoveChoice color isUndoable numberedMovesWithPos = do
    let options = optionsForMoveChoice isUndoable numberedMovesWithPos
    let prompt = promptForMoveChoice color isUndoable numberedMovesWithPos
    getValidChoice prompt options


optionsForMoveChoice :: Bool -> [(Int, Position)] -> [Int]
optionsForMoveChoice isUndoable numberedMovesWithPos = 
    let 
        posTags = Prelude.map fst numberedMovesWithPos
    in
        if isUndoable then
            choiceNumberFor_DisplayChoicesOnBoard : choiceNumberFor_Suggest : choiceNumberFor_Undo : posTags
        else
            choiceNumberFor_DisplayChoicesOnBoard : choiceNumberFor_Suggest : posTags


promptForMoveChoice :: Color -> Bool -> [(Int, Position)] -> String
promptForMoveChoice color isUndoable numberedMovesWithPos = 
    let 
        nomenclature = movePosChoicesNomenclature numberedMovesWithPos
    in
        if isUndoable then 
            (colorAllCapsString color) ++ " Options: (" ++ show choiceNumberFor_DisplayChoicesOnBoard ++ ":show, " ++ show choiceNumberFor_Suggest ++ ":suggest, " ++ show choiceNumberFor_Undo ++ ":undo) " ++ nomenclature ++ "\nEnter choice"
        else 
            (colorAllCapsString color) ++ " Options: (" ++ show choiceNumberFor_DisplayChoicesOnBoard ++ ":show, " ++ show choiceNumberFor_Suggest ++ ":suggest) " ++ nomenclature ++ "\nEnter choice"


displayEndSummary :: EndState -> IO ()
displayEndSummary x = do
    let taggedState = Tagged_EndState x

    putStrLn $ gameStateDisplay Nothing taggedState
    putStrLn ""
    putStrLn $ gameSummaryDisplay x
    putStrLn ""
    putStrLn "########################################"
    putStrLn "FYI, here are the flip-counts:\n"
    putStrLn $ boardWithFlipCountDisplay $ board_FromTaggedState taggedState
    putStrLn "########################################"
    
    return ()


reportMoveErrors :: NE.NonEmpty MoveValidationError -> IO ()
reportMoveErrors errors = do
    let uhohs = concatMap (\x -> "\n   " ++ show x) $ NE.toList errors

    putStrLn "\n================="
    putStrLn "INVALID MOVE"
    putStrLn uhohs
    putStrLn "\n================="

    return ()


choiceNumberFor_DisplayChoicesOnBoard :: Int
choiceNumberFor_DisplayChoicesOnBoard = 
    99


choiceNumberFor_Suggest :: Int
choiceNumberFor_Suggest = 
    100


choiceNumberFor_Undo :: Int
choiceNumberFor_Undo = 
    101  