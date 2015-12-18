module Board where

import Matrix
import Queue
import Color
import Position
import Direction

type Line = (Position, Direction)

lineFromPositions :: Position -> Position -> Line
lineFromPositions origin destiny = (origin, dirFromPositions origin destiny)

data Dot = Dot Position [Direction]

data Board = Board (Matrix Dot) Color

board :: Int -> Int -> Color -> Board
board rows cols Blue = Board (matrix (\x y -> Dot (x, y) []) rows (cols - 1)) Blue
board rows cols _    = Board (matrix (\x y -> Dot (x, y) []) (rows - 1) cols) Red

insideBoard :: Board -> Position -> Bool
insideBoard (Board m _) = uncurry $ inside m

getDot :: Board -> Position -> Dot
getDot (Board m _) = uncurry $ get m 

isEndPosition :: Board -> Position -> Bool
isEndPosition (Board (Matrix rows cols _) color) (r,c) = case color of
    Blue -> r == rows - 1
    _    -> c == cols - 1

neighbors :: Board -> Dot -> [Dot]
neighbors b (Dot pos conns) = [ getDot b $ goToDir pos d | d <- conns ] 

adjacent :: Position -> [Position]
adjacent p = [ goToDir p d | d <- [L,R,D,U] ]

sidesConnd :: Board -> (Bool, [Position])
sidesConnd b@(Board m color) = case color of
    Blue -> myBfs (buildQueue (zip (repeat (-1,-1)) (getRow m 0)))
    _    -> myBfs (buildQueue (zip (repeat (-1,-1)) (getCol m 0)))     
    where myBfs = bfs b (isEndPosition b) (neighbors b)

bfs :: Board -> (Position -> Bool) -> (Dot -> [Dot]) -> Queue (Position, Dot) -> (Bool, [Position])
bfs (Board (Matrix r c _) _) =  reconstructPath .: (bfs' visitedMatrix antecessorMatrix) 
    where (.:) = (.).(.).(.)
          visitedMatrix                   = matrix (\_ _ -> False)   r c
          antecessorMatrix                = matrix (\_ _ -> (-1,-1)) r c
          reconstructPath (False, _, _)   = (False, [])
          reconstructPath (_, predM, p)   = (True, reverse $ reconstructPath' predM p)
            where reconstructPath' _ (-1,-1) = []
                  reconstructPath' predM' p' = p' : reconstructPath' predM' ((uncurry $ get predM') p')

bfs' :: Matrix Bool -> Matrix Position -> (Position -> Bool) -> (Dot -> [Dot]) 
        -> Queue (Position, Dot) -> (Bool, Matrix Position, Position)
bfs' seen predec isTarget getNeighbors q
    | empty q     = (False, Matrix 0 0 [], (-1,-1))
    | alreadySeen = bfs' seen predec isTarget getNeighbors (pop q)
    | isTarget p  = (True, predec', p)
    | otherwise   = bfs' seen' predec' isTarget getNeighbors q'
    where (prev, d@(Dot p@(r,c) _)) = top q
          alreadySeen               = get seen r c
          seen'                     = set seen (\_ -> True) r c
          predec'                   = set predec (\_ -> prev) r c
          q'                        = foldl push (pop q) (zip (repeat p) (getNeighbors d))

hasLine :: Board -> Line -> Bool
hasLine b (p,d) = let (Dot _ dirs) = getDot b p in d `elem` dirs 

validLine :: Board -> Line -> Bool
validLine b (p,d) = insideBoard b $ goToDir p d

connect :: Board -> Line -> Board
connect b (p,d) = foldl addConnection b [(p, d), (goToDir p d, opposite d)]
    where addConnection (Board m c) (p', d') = Board ((uncurry $ set m addDir) p') c
           where addDir (Dot p'' l) = Dot p'' $ d':l