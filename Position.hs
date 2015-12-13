module Position where

import Direction

type Position = (Int, Int)

goToDir :: Position -> Direction -> Position
goToDir (x, y) d = case d of
    L -> (x, y - 1)
    R -> (x, y + 1)
    D -> (x + 1, y)
    _ -> (x - 1, y)

dirFromPositions :: Position -> Position -> Direction
dirFromPositions origin destiny    
    | goToDir origin R == destiny = R
    | goToDir origin L == destiny = L
    | goToDir origin U == destiny = U
    | goToDir origin D == destiny = D