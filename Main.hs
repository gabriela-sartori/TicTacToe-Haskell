module Main where

import Global
import Game

{- Começar um jogo novo ou sair -}
main :: IO()
main = do
    putStrLn "Menu:\n  1 - Human vs Human\n  2 - Human vs Machine\n  3 - Machine vs Machine\n  _ - Exit"
    str_start <- getLine
    let doStart = if isValidDigit str_start then head str_start else '_'
    case doStart of
        '1' -> mainGame Primeiro Primeiro Human Human     cleanMap >> main
        '2' -> mainGame Primeiro Primeiro Human Machine   cleanMap >> main
        '3' -> mainGame Primeiro Primeiro Machine Machine cleanMap >> main
        _   -> putStrLn "Program ended."
