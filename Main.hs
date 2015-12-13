module Main where

import Helpers
import Bridgit
import Strategies
import Error
import Color
import Direction

play :: Bridgit -> Maybe Color -> Strategy -> Color -> IO (Bridgit, Bool, Bool)
play b userColor strategy color = do
    putStrLn $ "Jugada del jugador " ++ (show color)
    
    m <- strategy b color
    putStrLn $ showMove m

    let (b', err) = move b color m
    if isJust err then do

        if isJust userColor && color == fromJust userColor then do 
            putStrLn $ "La jugada que has introduït no és correcte: " ++ show (fromJust err)
            return (b', False, True)
        else do
            putStrLn $ "El jugador " ++ (show color) ++ " ha comès un error en la jugada: " ++ show (fromJust err)
            putStrLn "S'avorta el joc, la màquina ha comès un error. Espero que no em suspenguis :("
            return (b', True, False)
    else do
        let path = wins b' color

        if isJust path then do
            putStrLn $ show b'
            putStrLn $ "Guanya el jugador " ++ (show color) ++ "!" ++ "\n"
            putStrLn $ "Camí que uneix els extrems: " ++ showPath (fromJust path) ++ "\n"
            putStrLn "#################"
            putStrLn "Fi de la partida"
            putStrLn "#################"
            return (b', True, False)
        else return (b', False, False)

gameFrom :: Bridgit -> Maybe Color -> Bool -> Strategy -> Strategy -> IO ()
gameFrom b userColor previousErr strategyBlue strategyRed = do
    if not previousErr then putStrLn $ show b else return ()

    (b', hasWon, err) <- (if not previousErr || fromJust userColor == Blue then 
                          play b userColor strategyBlue Blue else return (b, False, False))

    if hasWon then return () else
        if err then gameFrom b' userColor err strategyBlue strategyRed
        else do
            if not previousErr || fromJust userColor == Blue then putStrLn $ show b' else return ()
                
            (b'', hasWon', err') <- play b' userColor strategyRed  Red

            if hasWon' then return () else
                gameFrom b'' userColor err' strategyBlue strategyRed

showIntro :: String -> Int -> Int -> IO ()
showIntro t rows cols = do
    putStrLn $ "Inici de la " ++ t ++ ": " ++ (show rows) ++ " files, " ++ (show cols) ++ " columnes."
    putStrLn "Els punts es mostren de tal forma que l'extrem superior esquerre és l'(1,1)"
    putStrLn "i la notació és (fila,columna)"
    putStrLn $ "Comença el jugador " ++ (show Blue) ++ ".\n"
    putStrLn "Situació inicial:"

simulate :: Int -> Int -> IO ()
simulate rows cols = do
    putStrLn $ "\nQuina estratègia vols que segueixi el jugador " ++ (show Blue) ++ "?\n"
    
    strategyBlue <- promptStrategy

    putStrLn $ "Quina estratègia vols que segueixi el jugador " ++ (show Red) ++ "?\n"

    strategyRed <- promptStrategy

    showIntro "simulació" rows cols

    gameFrom (bridgit rows cols) Nothing False strategyBlue strategyRed

promptStrategy :: IO Strategy
promptStrategy = do
    mapM_ printStrategy $ zip [1..] strategies

    num <- prompt numParser "\nEntra un número: " 
    let (StrategyInfo s strategy) = strategies !! num

    putStrLn $ "\nHas escollit l'estratègia " ++ (show $ num + 1) ++ " (" ++ s ++ ")\n"

    return strategy
    where printStrategy (num, (StrategyInfo s _)) = putStrLn $ show num ++ ". " ++ s
          numParser s
            | isJust int && jint >= 1 && jint <= (length strategies) = (Nothing,  Just $ jint - 1)
            | isJust int = (Just INVALID_CHOICE_NUMBER, Nothing)
            | otherwise  = (Just PARSE_ERROR_INT, Nothing)
            where int = maybeRead s :: Maybe Int
                  jint = fromJust int

userStrategy :: Strategy
userStrategy _ _ = prompt moveParser "Entra la jugada (notació explicada dalt): "
    where readDir s | s `elem` ["L", "Esquerra"] = Just L
                    | s `elem` ["R", "Dreta"]    = Just R
                    | s `elem` ["U", "Amunt"]    = Just U
                    | s `elem` ["D", "Abaix"]    = Just D
                    | otherwise                  = Nothing
          moveParser s = moveParser' $ words s
          moveParser' l@[_,_,dir] = moveParser'' (map maybeRead (take 2 l) :: [Maybe Int]) (readDir dir)
          moveParser' _           = (Just NOT_ENOUGH_NUMBERS, Nothing)
          moveParser'' [Just row, Just col] (Just dir) = (Nothing, Just ((row-1,col-1), dir))
          moveParser'' [Just _, Just _]     _          = (Just INVALID_DIR, Nothing)
          moveParser'' _                    _          = (Just PARSE_ERROR_INT, Nothing)

userCPU :: Int -> Int -> IO ()
userCPU rows cols = do
    putStrLn "\nQuina estratègia vols que segueixi el programa?\n"

    cpuStrategy <- promptStrategy

    putStrLn $ "Per a la següent pregunta, tingues en compte que sempre comença el jugador " ++ show Blue ++ "\n"
    userColor <- prompt userColorParser $ "Quin jugador vols ser, el " ++ show Blue ++ " (1) o el " ++ show Red ++ " (2)? "

    putStrLn $ "\nHas triat ser el jugador " ++ (show userColor)
 
    putStrLn "\x1b[37;1m\nLa notació per indicar un moviment és \"fila columna DIRECCIO\" (sense \")"
    putStrLn "On fila columna són >= 1 i DIRECCIO és un de Amunt, Esquerra, Dreta, Abaix o U, L, R, D\x1b[0m\n"

    showIntro "partida" rows cols 

    if userColor == Blue then gameFrom (bridgit rows cols) (Just Blue) False userStrategy cpuStrategy
    else gameFrom (bridgit rows cols) (Just Red) False cpuStrategy userStrategy

    where userColorParser s
            | s `elem` ["1", "Blau", "Blue", "el Blau", "blau", "el blau", "primer"] = (Nothing, Just Blue)
            | otherwise = (Nothing, Just Red)
main = do
    putStrLn "----------------------------"
    putStrLn ":##########################:"
    putStrLn ":       B R I D G I T      :"
    putStrLn ":##########################:"
    putStrLn "----------------------------"

    putStrLn "\nBenvingut al joc!\n"
   
    isSimulation <- prompt gameTypeParser "Vols simular una partida (1) o jugar contra l'ordinador (2)? "

    putStrLn $ "\nHas triat " ++ (if isSimulation then "simular una partida" else "jugar contra l'ordinador") ++ "\n"

    [rows, cols] <- prompt sizesParser "Indica el nombre de files i columnes del tauler, separats per un espai: "

    if isSimulation then simulate rows cols
    else do userCPU rows cols

    where gameTypeParser s
            | s `elem` ["1", "sim", "simular", "simulacio", "primer", "primera", "vull simular"] = (Nothing, Just True)
            | otherwise = (Nothing, Just False)
          sizesParser s = sizesParser' $ words s
            where sizesParser' l@[_, _] = let l' = map maybeRead l :: [Maybe Int] in sizesParser'' l' 
                  sizesParser' _ = (Just NOT_ENOUGH_NUMBERS, Nothing)
                  sizesParser'' l = case l of
                   [Just x, Just y] -> sizesParser''' [x,y] 
                   [_, _] -> (Just PARSE_ERROR_INT, Nothing)
                  sizesParser''' l@[x, y]
                    | validSize x y = (Nothing, Just l)
                    | otherwise     = (Just INVALID_GAME_SIZE, Nothing)
