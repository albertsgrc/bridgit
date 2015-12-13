module Strategies where

import System.Random
import Matrix
import Board
import Bridgit
import Helpers
import Direction
import Color
import Position

type Strategy = (Bridgit -> Color -> IO Line) 

data StrategyInfo = StrategyInfo String Strategy 

strategies :: [StrategyInfo]
strategies = [ 
    StrategyInfo "Estrategia Aleatoria" strategyRandom,
    StrategyInfo "Estrategia Linia"     strategyLine,
    StrategyInfo "Estrategia BFS"       strategyBFS,  
    StrategyInfo "Estrategia Bloqueig"  strategyBlock
    ]

strategyRandom :: Strategy
strategyRandom b c = do
    rand <- randomRIO (0, (length validMoves) - 1)
    return $ validMoves !! rand
    where (Board (Matrix rows cols _) _) = getBoard b c
          validMoves = [ ((x, y), d) | x <- [0..rows-1], y <- [0..cols-1], d <- [L,R,U,D], isValidMove b c ((x,y),d) ]    

-- Sembla complicada però és una tonteria
strategyLine :: Strategy
strategyLine b c = do
    if l > 0 then do  
        randomInLine <- randomRIO (0, l-1)
        return $ bestLine !! randomInLine
    else strategyRandom b c

    where l = length bestLine
          bestLine = filter (isValidMove b c) $ fst $ argvalmax valueLine $ (map $ getMovesInLine c parald paral) [0..perp-1]
          (perp, paral, parald) = (if c == Blue then (cols, rows, D) else (rows, cols, R))
          boar@(Board (Matrix rows cols _) _) = getBoard b c
          getMovesInLine color dir size line = case color of
            Blue -> moves
            _    -> map flipAxis moves
            where moves =  [ ((x, line), dir) | x <- [0..size-2] ]
          flipAxis ((x,y), d) = ((y,x), d)
          valueLine :: [Line] -> Int
          valueLine line
            | feasible  = numberConnections
            | otherwise = -1
            where feasible = not $ (any $ crossesLine b c) line
                  numberConnections = foldl connCounter 0 line
                  connCounter x mov | hasLine boar mov = x + 1 | otherwise = x 

-- Estratègia inútil per si mateixa però útil per fer la de bloqueig
strategyBFS :: Strategy
strategyBFS b c = return $ lineFromPositions f s
    where boar@(Board m _) = getBoard b c 
          (_,path) = wsp boar (isEndPosition boar) feasibleNeighbors initialQueue
          initialQueue = case c of Blue -> zip (zip (repeat (-1,-1)) (getRow m 0)) (repeat 0)
                                   _    -> zip (zip (repeat (-1,-1)) (getCol m 0)) (repeat 0)
          feasibleNeighbors :: Dot -> [Dot]   
          feasibleNeighbors (Dot pos _) = map (\p -> getDot boar p) $ filter isFeasible $ adjacent pos
            where isFeasible neigh = let line = lineFromPositions pos neigh in 
                    not (crossesLine b c line) && (validLine boar line)
          (f,s) = firstNotConnectedToNext path
          firstNotConnectedToNext (x:y:xs)
            | hasLine boar $ lineFromPositions x y = firstNotConnectedToNext (y:xs)
            | otherwise                            = (x, y) 


wsp :: Board -> (Position -> Bool) -> (Dot -> [Dot]) -> [((Position, Dot), Int)] -> (Bool, [Position])
wsp b@(Board (Matrix r c _) _) =  reconstructPath .: (wsp' b visitedMatrix antecessorMatrix connsUsedMatrix) 
    where (.:) = (.).(.).(.)
          visitedMatrix                   = matrix (\_ _ -> False)   r c
          antecessorMatrix                = matrix (\_ _ -> (-1,-1)) r c
          connsUsedMatrix                 = matrix (\_ _ -> 2147483647)       r c
          reconstructPath (False, _, _)   = (False, [])
          reconstructPath (_, predM, p)   = (True, reverse $ reconstructPath' predM p)
            where reconstructPath' _ (-1,-1) = []
                  reconstructPath' predM' p' = p' : reconstructPath' predM' ((uncurry $ get predM') p')

wsp' :: Board -> Matrix Bool -> Matrix Position -> Matrix Int -> (Position -> Bool) -> (Dot -> [Dot]) 
        -> [((Position, Dot), Int)] -> (Bool, Matrix Position, Position)
wsp' b seen predec connsUsed isTarget getNeighbors q
    | null q      = (False, Matrix 0 0 [], (-1,-1))
    | alreadySeen = wsp' b seen predec connsUsed isTarget getNeighbors (popPrio q)
    | isTarget p  = (True, predec', p)
    | otherwise   = wsp' b seen' predec' connsUsed' isTarget getNeighbors q'
    where ((prev, d@(Dot p@(r,c) _)), connsUsedPrev) = topPrio q
          alreadySeen               = get seen r c
          seen'                     = set seen (\_ -> True) r c
          predec'                   = set predec (\_ -> prev) r c
          (q', connsUsed')          = foldl pushPrioIfMoreConnsUsed ((popPrio q), connsUsed) (getNeighbors d)
          topPrio (x:_)   = x
          pushPrio q'' e = insertSort hasLessConns q'' e
          popPrio (_:xs)  = xs
          hasLessConns (_,a) (_,b') = a < b'
          pushPrioIfMoreConnsUsed (q'', connsUsed'') d''@(Dot o@(x,y) _)
            | get connsUsed'' x y > connsNew = (pushPrio q'' ((p,d''), connsNew), set connsUsed'' (\_ -> connsNew) x y)
            | otherwise                      = (q'', connsUsed'') 
            where connsIUseNow | hasLine b (lineFromPositions p o) = 0 | otherwise = 1
                  connsNew = connsUsedPrev + connsIUseNow

insertSort :: (a -> a -> Bool) -> [a] -> a -> [a]
insertSort _ [] x     = [x]
insertSort menor (y:ys) x 
    | x `menor` y = x : y : ys
    | otherwise = y : (insertSort menor ys x)

-- Obtenir el camí que va d'un cantó a l'altre del taulell que impliqui afegir menys connexions
-- Trencar el camí per qualsevol punt
strategyBlock :: Strategy
strategyBlock b c = do
    othersMove <- strategyBFS b (otherColor c)
    return $ block othersMove
    where 
      block ((x, y), dir) = case dir of
        U -> case c of Blue -> ((x, y-1),  R) 
                       Red  -> ((x-1, y),  R)
        D -> case c of Blue -> ((x+1,y-1), R)
                       Red  -> ((x,y),     R)
        L -> case c of Blue -> ((x,y-1),   D)
                       Red  -> ((x-1,y),   D)
        R -> case c of Blue -> ((x,y),     D)
                       Red  -> ((x-1,y+1), D)
