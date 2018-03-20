module GameState
    ( GameState -- hiding constructor, don't want to swap diskCounts
    , playMove
    )
    where
 
import Disk ( Color(..) )
import Board ( Board, Move(..), applyMove, initialBoard )
import DiskCount ( TaggedDiskCount, makeTagged_BlackDiskCount, makeTagged_WhiteDiskCount, decreaseByOne)

data GameState = GameState 
    { _blackDiskCount   :: TaggedDiskCount
    , _whiteDiskCount   :: TaggedDiskCount
    , _board            :: Board
    }
        deriving (Eq, Show)
 

makeGameState :: GameState
makeGameState =
    GameState 
        { _blackDiskCount = makeTagged_BlackDiskCount
        , _whiteDiskCount = makeTagged_WhiteDiskCount
        , _board = initialBoard
        }


blackMoved :: Board -> GameState -> GameState
blackMoved board (GameState cB cW _) =
    GameState (decreaseByOne cB) cW board


whiteMoved :: Board -> GameState -> GameState
whiteMoved board (GameState cB cW _) =
    GameState cB (decreaseByOne cW) board


playMove :: Move -> GameState -> GameState
playMove move gameState =
    let
        board = applyMove move $ _board gameState
    in
        case _color move of 
            Black -> blackMoved board gameState 
            White -> whiteMoved board gameState
         