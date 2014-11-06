module Game where

import Control.Monad()
import Data.Char(toLower)
import Data.List
import Data.List.Split
import Data.Sequence()
import System.Random
import Global
import Control.Applicative()
import Data.Functor()
import Data.Maybe

{- Retorna a string de cada tipo: X, 0 ou vazio -}
getSlotStr :: Maybe Slot -> String
getSlotStr (Just Xis)  = "X"
getSlotStr (Just Bola) = "O"
getSlotStr Nothing     = " "

{- Retorna uma String representando o campo e as peças do Jogo Da Velha -}
drawMap :: Map -> String
drawMap xs = unlines $ intersperse "---------" $ map (intercalate " | ") $ chunksOf 3 $ map getSlotStr xs

{- Retorna se todos os campos foram preenchidos -}
isGameOver :: Map -> Bool
isGameOver xs = Nothing `notElem` xs

{- Retorna a peça do jogador -}
getPlayerPiece :: Jogador -> Maybe Slot
getPlayerPiece Primeiro = Just Xis
getPlayerPiece Segundo  = Just Bola

{- Retorna o jogador inimigo -}
getOponent :: Jogador -> Jogador
getOponent Primeiro = Segundo
getOponent Segundo  = Primeiro

{- Retorna a peça do mapa em função do seu indice -}
getSlotByIndex :: Int -> Map -> Maybe Slot
getSlotByIndex index xs = xs !! index

{- Verifica se determinado jogador ganhou -}
isPlayerWinner :: Jogador -> Map -> Bool
isPlayerWinner player xs = elem True $ map (all matches) winCombinations where
    matches slot = getSlotByIndex (slot - 1) xs == getPlayerPiece player

{- Verifica as combinações e retorna se algum jogador ganhou o jogo. Caso contrário retorna Nothing -}
getWinner :: Map -> Maybe Jogador
getWinner xs
    | isPlayerWinner Primeiro xs = Just Primeiro
    | isPlayerWinner Segundo  xs = Just Segundo
    | otherwise = Nothing

{- Gera um mapa vazio -}
cleanMap :: Map -- Gera um mapa novo
cleanMap = replicate 9 Nothing

{- Substitui um valor em um indice de uma lista  -}
replace :: a -> Int -> [a] -> [a]
replace value index xs = map (\(i, val) -> if i == index then value else val) (zip [0..] xs)

{- Faz a jogada, substituindo vazio pela peça do jogador -}
modifyMap :: Map -> Jogador -> Int -> Map
modifyMap xs player jogada = replace (getPlayerPiece player) (jogada - 1) xs

isValidDigit :: String -> Bool
isValidDigit []  = False
isValidDigit str = head str `elem` ['0'..'9']

{- Retorna as jogadas disponíveis -}
getAvaiableJogadas :: Map -> [(Int, Maybe Slot)]
getAvaiableJogadas _map = filter ((== Nothing) . snd) (zip [1..] _map)

getJogadaToWin :: Jogador -> Map -> Maybe Int
getJogadaToWin _player _map = find (const True) $ map snd $ filter match aiWinCombinations where
        match (combination, win_pos) = all (\slot -> getSlotByIndex (slot - 1) _map == getPlayerPiece _player) combination
                                       && isNothing (getSlotByIndex (win_pos - 1) _map)

{-
    A.I. atual:
        1. Verificar se tem uma jogada que o faria ganhar
        2. Verificar se tem uma jogada que o impediria perder
        3. Escolher uma casa aleatória dentre as disponíveis

    Flawless A.I.:
        http://programmers.stackexchange.com/a/213570
-}
{- Cada jogada de um jogador do tipo Machine -}
jogadaMachine :: Jogador -> Map -> IO Int
jogadaMachine _jogador _map = do
    let win = getJogadaToWin _jogador _map
    case win of
        Just jogada -> putStrLn ("::Win " ++ show jogada) >> return jogada
        Nothing -> do
            let lose = getJogadaToWin (getOponent _jogador) _map
            case lose of
                Just jogada -> putStrLn ("::Block " ++ show jogada) >> return jogada
                Nothing -> do
                    let jogadas = getAvaiableJogadas _map
                    _random <- randomRIO (0, length jogadas - 1)
                    putStrLn $ "::Random " ++ (show . fst $ jogadas !! _random)
                    return . fst $ jogadas !! _random

{- Cada jogada de um jogador do tipo Human -}
jogadaHuman :: Map -> IO Int
jogadaHuman _map = do
    raw_jogada <- getLine
    if isValidDigit raw_jogada then do
        let casa_jogada = read raw_jogada
        if casa_jogada >= 1 && casa_jogada <= 9 then
            if isNothing (getSlotByIndex (casa_jogada - 1) _map) then
                return casa_jogada
            else putStrLn "Slot já ocupado."
                 >> jogadaHuman _map
        else putStrLn "Digito precisa estar na margem [1..9]"
             >> jogadaHuman _map
    else putStrLn "Digito inválido."
         >> jogadaHuman _map

{- Retorna a jogada de humano ou maquina dependendo de quem é o round e do tipo do jogador 1 e 2 -}
getJogada :: Jogador -> (PlayerType, PlayerType) -> Map -> IO Int
getJogada Primeiro (Human,   _) _map = jogadaHuman            _map
getJogada Primeiro (Machine, _) _map = jogadaMachine Primeiro _map
getJogada Segundo  (_,   Human) _map = jogadaHuman            _map
getJogada Segundo  (_, Machine) _map = jogadaMachine Segundo  _map

{- Confirmação para repetir o mesmo tipo de jogo -}
newRound :: Jogador -> Jogador -> PlayerType -> PlayerType -> Map -> IO ()
newRound _startedRound _round _player1 _player2 _map = putStrLn "Começar uma nova partida ? [Y/N]"
    >> getLine
    >>= (\confirmation ->
    if map toLower confirmation == "y" then do
        let oponent = getOponent _startedRound
        mainGame oponent oponent _player1 _player2 cleanMap
    else
        putStrLn "Voltando ao menu principal."
    )

{- Cada jogo -}
mainGame :: Jogador -> Jogador -> PlayerType -> PlayerType -> Map -> IO ()
mainGame _startedRound _round _player1 _player2 _map = do
    putStrLn $ drawMap _map
    let ganhador = getWinner _map
    case ganhador of
        Just x  ->  do
                    putStrLn $ "O ganhador foi: " ++ show x
                    newRound _startedRound _round _player1 _player2 _map
        Nothing ->  if isGameOver _map then do
                        putStrLn "Empate!"
                        newRound _startedRound _round _player1 _player2 _map
                    else do
                        putStrLn $ "Jogador " ++ show _round ++ " faça sua jogada:"
                        casa_jogada <- getJogada _round (_player1, _player2) _map
                        mainGame _startedRound (getOponent _round) _player1 _player2 (modifyMap _map _round casa_jogada)
