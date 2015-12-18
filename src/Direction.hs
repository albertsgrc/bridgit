module Direction where

data Direction = U | R | D | L deriving(Eq)

instance Show Direction where
    show d = case d of
        U -> "superior"
        R -> "dret"
        D -> "inferior"
        L -> "esquerre"

opposite :: Direction -> Direction
opposite L = R; opposite R = L; opposite U = D; opposite D = U;
