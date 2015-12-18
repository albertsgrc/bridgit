module Bridgit where

import Matrix
import Board
import Helpers
import Error
import Color
import Position
import Direction

data Bridgit = Bridgit { blueBoard :: Board, redBoard :: Board }

instance Show Bridgit where
    show (Bridgit bb@(Board (Matrix _ colsB b) _) rb@(Board (Matrix _ _ r) _)) = 
        (concat $ intercalate (map showBlueRow b) (map (showRow Red) r)) 
        ++ "\x1b[0m"
      where showBlueRow row@((Dot (x, _) _):_)
                | x > 0 && hasLine rb ((x - 1, 0), D) = (colorChar Red) ++ "| " ++ showRow Blue row
                | otherwise = "  " ++ showRow Blue row 
            showRow col row = concatMap showDot row ++ "\n"
              where showDot (Dot p@(x, y) d)
                      | R `elem` d = (colorChar col) ++ "*---"
                      | (col == Blue && x > 0     && hasLine rb ((x-1,y+1), D)) || 
                        (col == Red  && y < colsB && hasLine bb (p, D)) = 
                        (colorChar col) ++ "* " ++ (colorChar (otherColor col))  ++ "| "
                      | otherwise = (colorChar col) ++ "*   "

bridgit :: Int -> Int -> Bridgit
bridgit rows cols = let myBoard = board rows cols in Bridgit (myBoard Blue) (myBoard Red)

wins :: Bridgit -> Color -> Maybe [Position]
wins (Bridgit blue red) color
    | color == Blue && blueConnected = Just bluePath
    | color == Red  && redConnected  = Just redPath
    | otherwise = Nothing
    where (blueConnected, bluePath) = sidesConnd blue
          (redConnected, redPath)   = sidesConnd red

getBoard :: Bridgit -> Color -> Board
getBoard (Bridgit blue _) Blue = blue
getBoard (Bridgit _  red) Red  = red

crossesLine :: Bridgit -> Color -> Line -> Bool
crossesLine (Bridgit blue@(Board (Matrix _ colsB _) _) red@(Board (Matrix rowsR _ _) _)) c ((x,y), d) = 
    case d of U -> case c of Blue -> x > 0     && hasLine red  ((x-1, y),   R)
                             _    -> y > 0     && hasLine blue ((x,   y-1), R)
              D -> case c of Blue -> x < rowsR && hasLine red  ((x,   y),   R)
                             _    -> y > 0     && hasLine blue ((x+1, y-1), R)
              L -> case c of Blue -> x > 0     && hasLine red  ((x-1, y),   D)
                             _    -> y > 0     && hasLine blue ((x,   y-1), D)
              _ -> case c of Blue -> x > 0     && hasLine red  ((x-1, y+1), D)
                             _    -> y < colsB && hasLine blue ((x,   y),   D)

isValidMove :: Bridgit -> Color -> Line -> Bool
isValidMove brid c l = (validLine boar l) && (not $ hasLine boar l) && (not $ crossesLine brid c l) 
    where boar = getBoard brid c

move :: Bridgit -> Color -> Line -> (Bridgit, Maybe Error)
move brid@(Bridgit blue red) c l
    | not $ validLine boar l = (brid, Just INVALID_CONN) 
    | hasLine boar l         = (brid, Just REPEATED_CONN)
    | crossesLine brid c l   = (brid, Just CROSSES_CONN) 
    | c == Blue              = (Bridgit (connect boar l) red, Nothing)
    | otherwise              = (Bridgit blue (connect boar l), Nothing)
    where boar = getBoard brid c

validSize :: Int -> Int -> Bool
validSize rows cols = rows > 1 && cols > 1

-- No ho he fet com instance de show perquè implicava definir line com a data o newtype,
-- I embrutava el codi amb una constructora innecesària
showMove :: Line -> String
showMove ((x, y), dir) = 
    "Connecta el punt " ++ (show (x+1,y+1)) ++ " amb el punt " ++ (show dir)

showPath :: [Position] -> String
showPath (x:xs) = showPos x ++ concatMap ((" -> " ++) . showPos) xs
    where showPos (r,c) = show (r+1,c+1)