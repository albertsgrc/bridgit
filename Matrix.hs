module Matrix where

import Helpers

-- Very inneficient implementation of matrices. Only chance as we cannot import external libs

data Matrix t = Matrix Int Int [[t]]

matrix :: (Int -> Int -> t) -> Int -> Int -> Matrix t
matrix initF rows cols = Matrix rows cols $ (map . map) (uncurry initF) ijMatrix
    where ijMatrix  = map genCols [0..rows - 1]
          genCols x = [ (x,y) | y <- [0..cols - 1] ] 

inside :: Matrix t -> Int -> Int -> Bool
inside (Matrix rows cols _) r c = r `between` (0, rows - 1) && c `between` (0, cols - 1)

getRow :: Matrix t -> Int -> [t]
getRow (Matrix _ _ l) r = l !! r

getCol :: Matrix t -> Int -> [t]
getCol (Matrix _ _ l) c = map (!! c) l

get :: Matrix t -> Int -> Int -> t
get (Matrix _ _ l) r c = l !! r !! c

set :: Matrix t -> (t -> t) -> Int -> Int -> Matrix t
set (Matrix rows cols l) f r c = Matrix rows cols $ prevRows ++ [changedRow] ++ nextRows
    where (prevRows, row:nextRows) = splitAt r l
          changedRow               = prevElems ++ [f p] ++ nextElems
          (prevElems, p:nextElems) = splitAt c row
