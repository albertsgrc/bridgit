module Color where

data Color = Blue | Red deriving(Eq)

instance Show Color where
    show Blue = colorChar Blue ++ "Blau" ++ "\x1b[0m"
    show Red  = colorChar Red ++ "Vermell" ++ "\x1b[0m"

colorChar :: Color -> String
colorChar Blue = "\x1b[34;1m"
colorChar Red  = "\x1b[31;1m"

otherColor :: Color -> Color
otherColor Blue = Red
otherColor _    = Blue