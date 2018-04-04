module Sequencer
    ( moveSequence
    ) 
    where 
 
import Player ( PlayerBlack, PlayerWhite, Tagged_Player(..), playerTypeFrom, playerColor ) 
import PlayerType ( PlayerType(..) )
import State ( Tagged_State(..), applyMove, board_FromTaggedState, nextMoveColor_FromTaggedState, actual_NextMoves_FromTaggedState )
import Board ( Move, movePosChoices )
import Disk ( Color(..) )
import Position ( Position )
import Display ( movePosChoicesNomenclature, boardWithFlipCountDisplay, gameStateDisplay, gameSummaryDisplay, colorAllCapsString )
import Engine ( computerChoose, stratDisplay )
import Lib ( getValidChoice )


moveSequence :: (PlayerBlack, PlayerWhite) -> Tagged_State -> IO ()
moveSequence players taggedState = do       
    let taggedPlayer = nextPlayer players taggedState
    putStrLn ""
    putStrLn $ gameStateDisplay Nothing taggedState

    move <- case playerTypeFrom taggedPlayer of
        Person -> do
            moveIndex <- personChoose (playerColor taggedPlayer) taggedState
            return $ (actual_NextMoves_FromTaggedState taggedState) !! moveIndex

        Computer strategy -> do 
            putStrLn $ "\nComputer (White) is working (" ++ (stratDisplay strategy) ++ ") ...\n"
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
    case nextMoveColor_FromTaggedState taggedState of
        Black -> BlackPlayerTag pb
        White -> WhitePlayerTag pw


personChoose :: Color -> Tagged_State -> IO Int
personChoose color taggedState = do
    let moves = actual_NextMoves_FromTaggedState taggedState
    let numberedMovesWithPos = movePosChoices moves
    n <- handlePersonChoose color numberedMovesWithPos moves taggedState
    return n


handlePersonChoose :: Color -> [(Int, Position)] -> [Move] -> Tagged_State -> IO Int
handlePersonChoose color numberedMovesWithPos moves taggedState  = do
    n <- getMoveChoice color numberedMovesWithPos

    if n == choiceNumberFor_DisplayChoicesOnBoard then do
        putStrLn ""
        putStrLn $ gameStateDisplay (Just numberedMovesWithPos) taggedState
        handlePersonChoose color numberedMovesWithPos moves taggedState 
    else do 
        return $ n - 1 -- index is zero-based


getMoveChoice :: Color -> [(Int, Position)] -> IO Int
getMoveChoice color numberedMovesWithPos = do
    let posTags = Prelude.map fst numberedMovesWithPos
    let options = choiceNumberFor_DisplayChoicesOnBoard : posTags
    let nomenclature = movePosChoicesNomenclature numberedMovesWithPos
    let prompt = (colorAllCapsString color) ++ " Options: (" ++ show choiceNumberFor_DisplayChoicesOnBoard ++ ":show) " ++ nomenclature ++ "\nEnter choice"
    getValidChoice prompt options


choiceNumberFor_DisplayChoicesOnBoard :: Int
choiceNumberFor_DisplayChoicesOnBoard = 
    99