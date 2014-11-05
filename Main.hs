import Control.Monad()
import Data.Char()
import Data.List
import Data.List.Split
import Data.Sequence()
import System.Random
import Global
import Control.Applicative()
import Data.Functor()

{- Retorna a string de cada tipo: X, 0 ou vazio -}
getSlotStr :: Slot -> String
getSlotStr Xis   = "X"
getSlotStr Bola  = "O"
getSlotStr Empty = " "

{- Retorna uma String representando o campo e as peças do Jogo Da Velha -}
drawMap :: Map -> String
drawMap xs = unlines $ intersperse "---------" $ map (intercalate " | ") $ chunksOf 3 $ map getSlotStr xs

{- Retorna se todos os campos foram preenchidos -}
isGameOver :: Map -> Bool
isGameOver xs = Empty `notElem` xs

{- Retorna a peça do jogador -}
getPlayerPiece :: Jogador -> Slot
getPlayerPiece Primeiro = Xis
getPlayerPiece Segundo  = Bola

{- Retorna o jogador inimigo -}
getOponent :: Jogador -> Jogador
getOponent Primeiro = Segundo
getOponent Segundo  = Primeiro

{- Retorna a peça do mapa em função do seu indice -}
getSlotByIndex :: Int -> Map -> Slot
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

{- Substitui um valor em um indice de uma lista  -}
replace :: a -> Int -> [a] -> [a]
replace value index xs = map (\(i, val) -> if i == index then value else val) (zip [0..] xs)

{- Faz a jogada, substituindo vazio pela peça do jogador -}
modifyMap :: Map -> Jogador -> Int -> Map
modifyMap xs player jogada = replace (getPlayerPiece player) (jogada - 1) xs

isValidDigit :: String -> Bool
isValidDigit []  = False
isValidDigit str = head str `elem` ['0'..'9']

{- Cada jogada -}
jogadaHuman :: Map -> IO Int
jogadaHuman _map = do
    raw_jogada <- getLine
    if isValidDigit raw_jogada then do
        let casa_jogada = read raw_jogada
        if casa_jogada >= 1 && casa_jogada <= 9 then
            if getSlotByIndex (casa_jogada - 1) _map == Empty then
                return casa_jogada
            else putStrLn "Slot já ocupado."
                 >> jogadaHuman _map
        else putStrLn "Digito precisa estar na margem [1..9]"
             >> jogadaHuman _map
    else putStrLn "Digito inválido."
         >> jogadaHuman _map

{- Retorna as jogadas disponíveis -}
getAvaiableJogadas :: Map -> [(Int, Slot)]
getAvaiableJogadas _map = filter ((== Empty) . snd) (zip [1..] _map)

getJogadaToWin :: Jogador -> Map -> Maybe Int
getJogadaToWin _player _map = find (const True) $ map snd $ filter match aiWinCombinations where
        match (combination, win_pos) = all (\slot -> getSlotByIndex (slot - 1) _map == getPlayerPiece _player) combination
                                       && getSlotByIndex (win_pos - 1) _map == Empty

{-
    A.I. atual:
        1. Verificar se tem uma jogada que o faria ganhar
        2. Verificar se tem uma jogada que o impediria perder
        3. Escolher uma casa aleatória dentre as disponíveis

    Flawless A.I.:
        http://programmers.stackexchange.com/a/213570
-}
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

{- Retorna a jogada de humano ou maquina dependendo de quem é o round e do tipo do jogador 1 e 2 -}
getJogada :: Jogador -> (PlayerType, PlayerType) -> Map -> IO Int
getJogada Primeiro (Human,   _) _map = jogadaHuman            _map
getJogada Primeiro (Machine, _) _map = jogadaMachine Primeiro _map
getJogada Segundo  (_,   Human) _map = jogadaHuman            _map
getJogada Segundo  (_, Machine) _map = jogadaMachine Segundo  _map

{- Cada jogo -}
mainGame :: Jogador -> PlayerType -> PlayerType -> Map -> IO ()
mainGame _round _player1 _player2 _map = do
    putStrLn $ drawMap _map
    let ganhador = getWinner _map
    case ganhador of
        Just x  ->  putStrLn $ "O ganhador foi: " ++ show x
        Nothing ->  if isGameOver _map then
                        putStrLn "Empate!"
                    else do
                        putStrLn $ "Jogador " ++ show _round ++ " faça sua jogada:"
                        casa_jogada <- getJogada _round (_player1, _player2) _map
                        mainGame (if _round == Primeiro then Segundo else Primeiro) _player1 _player2 (modifyMap _map _round casa_jogada)

{- Começar um jogo novo ou sair -}
main :: IO()
main = do
    putStrLn "Menu:\n  1 - Human vs Human\n  2 - Human vs Machine\n  3 - Machine vs Machine\n  _ - Exit"
    str_start <- getLine
    let doStart = if isValidDigit str_start then head str_start else '_'
    let cleanMap :: Map -- Gera um mapa novo
        cleanMap = replicate 9 Empty
    case doStart of
        '1' -> mainGame Primeiro Human Human     cleanMap >> main
        '2' -> mainGame Primeiro Human Machine   cleanMap >> main
        '3' -> mainGame Primeiro Machine Machine cleanMap >> main
        _   -> putStrLn "Program ended."
