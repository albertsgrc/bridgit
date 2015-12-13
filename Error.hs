module Error where

data Error = INVALID_CONN | REPEATED_CONN | CROSSES_CONN | INVALID_GAME_SIZE | 
             NOT_ENOUGH_NUMBERS | PARSE_ERROR_INT | INVALID_CHOICE_NUMBER | INVALID_DIR

instance Show Error where 
    show e = case e of
        INVALID_CONN           -> "Connexió de punts no vàlida"
        REPEATED_CONN          -> "La connexió ja existeix"
        CROSSES_CONN           -> "La connexió creua una línia ja existent"
        INVALID_GAME_SIZE      -> "Les mides del joc no són prou grans"
        NOT_ENOUGH_NUMBERS     -> "No has entrat un nombre correcte de números"
        PARSE_ERROR_INT        -> "S'esperava un enter"
        INVALID_CHOICE_NUMBER  -> "El número que has escollit no està a la llista"
        INVALID_DIR            -> "La direcció que has entrat no és vàlida"