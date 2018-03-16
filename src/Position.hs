module Position
    ( Position
    , adjacentPositions
    , raysFrom
    )
    where

import BoardSize ( boardSize )

type Position = ( Int, Int )

data Dir = Inc | Dec


adjacentPositions :: Position -> [Position]
adjacentPositions (i, j) =
    let
        candidates = 
            [ (i-1, j-1), (i, j-1), (i+1, j-1)
            , (i-1, j  ),           (i+1, j  )
            , (i-1, j+1), (i, j+1), (i+1, j+1)
            ]

        isInRange = \ x -> x >= 1 && x <= boardSize
        f = \ (i',j') -> isInRange i' && isInRange j'
    in
        filter f candidates


raysFrom :: Position -> [[Position]]
raysFrom pos =
    [rayFrom_VertUp pos] ++
    [rayFrom_VertDown pos] ++

    [rayFrom_HorizRight pos] ++
    [rayFrom_HorizLeft pos] ++

    [rayFrom_DiagUpRight pos] ++
    [rayFrom_DiagUpLeft pos] ++

    [rayFrom_DiagDownRight pos] ++
    [rayFrom_DiagDownLeft pos]         


rayFrom_VertUp :: Position -> [Position]
rayFrom_VertUp (i,j) =   
    [(i',j') | i' <- reverse [1..i], j' <- [j]]


rayFrom_VertDown :: Position -> [Position]
rayFrom_VertDown (i,j) =   
    [(i',j') | i' <- [i..boardSize], j' <- [j]]


rayFrom_HorizRight :: Position -> [Position]
rayFrom_HorizRight (i,j) =   
    [(i',j') | i' <- [i], j' <- [j..boardSize]]


rayFrom_HorizLeft :: Position -> [Position]
rayFrom_HorizLeft (i,j) =   
    [(i',j') | i' <- [i], j' <- reverse [1..j]]
        

rayFrom_DiagUpRight :: Position -> [Position]
rayFrom_DiagUpRight pos =  
    rayFromDiag Dec Inc pos


rayFrom_DiagUpLeft :: Position -> [Position]
rayFrom_DiagUpLeft pos =
    rayFromDiag Dec Dec pos


rayFrom_DiagDownRight :: Position -> [Position]
rayFrom_DiagDownRight pos =  
    rayFromDiag Inc Inc pos


rayFrom_DiagDownLeft :: Position -> [Position]
rayFrom_DiagDownLeft pos =
    rayFromDiag Inc Dec pos


rayFromDiag :: Dir -> Dir -> Position -> [Position]
rayFromDiag rowDir colDir (i,j) =
    let
        limits = case (rowDir, colDir) of
            (Inc, Inc) -> (boardSize, boardSize)
            (Inc, Dec) -> (boardSize, 1)
            (Dec, Inc) -> (1, boardSize)
            (Dec, Dec) -> (1, 1)

        op dir = 
            case dir of
                Inc -> ( 1 +) 
                Dec -> (-1 +) 
                
        go result x y = 
            if x == fst limits || y == snd limits then
                result
            else
                let 
                    x' = op rowDir x
                    y' = op colDir y
                in 
                    go (result ++ [(x', y')]) x' y'
    in
        go [(i,j)] i j