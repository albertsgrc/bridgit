module Helpers where

import System.IO
import Error

between :: Ord a => a -> (a, a) -> Bool
between x (l, r) = l <= x && x <= r

intercalate :: [a] -> [a] -> [a]
intercalate (x:xs)  (y:ys) = x : y : (intercalate xs ys)
intercalate []       l     = l
intercalate l        l'    = l' ++ l 

isJust :: Maybe a -> Bool
isJust Nothing = False
isJust _       = True

fromJust :: Maybe a -> a
fromJust (Just x) = x

maybeRead :: Read a => String -> Maybe a
maybeRead s = case reads s of
    [(x, "")] -> Just x
    _         -> Nothing

prompt :: (String -> (Maybe Error, Maybe a)) -> String -> IO a
prompt parser s = do 
    putStr s
    hFlush stdout

    line <- getLine
    let (err, result) = parser line
    
    if isJust err then do
        putStrLn $ "Has introduit alguna dada malament: " ++ (show $ fromJust err)
        prompt parser s
    else return $ fromJust result

argvalcomp :: (b -> b -> Bool) -> (a -> b) -> [a] -> (a, b)
argvalcomp comp f (x:xs) = foldl (maxDecider f) (x, f x) xs
    where maxDecider f' m@(_, vmax) anew
            | vmax `comp` vnew = m | otherwise = (anew, vnew) where vnew = f' anew

argvalmax :: Ord b => (a -> b) -> [a] -> (a, b)
argvalmax = argvalcomp (>)

argvalmin :: Ord b => (a -> b) -> [a] -> (a, b)
argvalmin = argvalcomp (<)