import Control.Monad()
import Data.Char
import Data.List
import Data.List.Split
import Data.Sequence()
import System.Random

{- Definir tipos novos -}
data PlayerType = Human | Machine
data Jogador = Primeiro | Segundo deriving (Ord, Eq, Show, Read)
data Slot = Xis | Bola | Empty deriving (Ord, Eq, Show, Read)
type Map  = [Slot]

winCombinations :: [[Int]]
winCombinations = [ [1, 2, 3]
                  , [4, 5, 6]
                  , [7, 8, 9]
                  , [1, 4, 7]
                  , [2, 5, 8]
                  , [3, 6, 9]
                  , [1, 5, 9]
                  , [7, 5, 3]
                  ]

{- Gera um mapa novo -}
cleanMap :: Map
cleanMap = replicate 9 Empty

{- Retorna a string de cada tipo: X, 0 ou vazio -}
drawSlot :: Slot -> String
drawSlot Xis   = "X"
drawSlot Bola  = "O"
drawSlot Empty = " "

{- Retorna uma String representando o campo e as peças do Jogo Da Velha -}
drawMap :: Map -> String
drawMap xs = unlines $ intersperse "---------" $ map (intercalate " | ") $ chunksOf 3 $ map drawSlot xs

{- Retorna se todos os campos foram preenchidos -}
isGameOver :: Map -> Bool
isGameOver xs = Empty `notElem` xs

{- Retorna a peça do jogador -}
getSlotByPlayer :: Jogador -> Slot
getSlotByPlayer Primeiro = Xis
getSlotByPlayer Segundo  = Bola

{- Retorna a peça do mapa em função do seu indice -}
getSlotByIndex :: Int -> Map -> Slot
getSlotByIndex index xs = xs !! index

{- Verifica se determinado jogador ganhou -}
isPlayerWinner :: Jogador -> Map -> Bool
isPlayerWinner player xs = elem True $ map (all matches) winCombinations where
    matches slot = getSlotByIndex (slot - 1) xs == getSlotByPlayer player

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
modifyMap xs player jogada = replace (getSlotByPlayer player) (jogada - 1) xs

{- Cada jogada -}
jogadaHuman :: Map -> IO Int
jogadaHuman _map = do
    raw_jogada <- getLine
    let jogada = read raw_jogada
    if jogada >= 1 && jogada <= 9 then
        if _map !! (jogada - 1) == Empty then
                return jogada
        else putStrLn "Slot já ocupado."
          >> jogadaHuman _map
    else putStrLn "Jogada Invalida"
      >> jogadaHuman _map

{- Retorna as jogadas disponíveis -}
getAvaiableJogadas :: Map -> [(Int, Slot)]
getAvaiableJogadas _map = filter ((== Empty) . snd) (zip [1..] _map)

{- A AI da Machine é escolher uma casa aleatória dentre as disponiveis XD -}
jogadaMachine :: Map -> IO Int
jogadaMachine _map = do
    let jogadas = getAvaiableJogadas _map
    _random <- randomRIO (0, length jogadas - 1)
    return . fst $ jogadas !! _random

{- Retorna a jogada de humano ou maquina dependendo de quem é o round e do tipo do jogador 1 e 2 -}
getJogada :: Jogador -> (PlayerType, PlayerType) -> Map -> IO Int
getJogada Primeiro (Human,   _) _map = jogadaHuman   _map
getJogada Primeiro (Machine, _) _map = jogadaMachine _map
getJogada Segundo  (_,   Human) _map = jogadaHuman   _map
getJogada Segundo  (_, Machine) _map = jogadaMachine _map

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
    putStrLn "Menu:\n  1 - Human vs Human\n  2 - Human vs Machine\n  3 - Machine vs Machine\n _ - Exit"
    start <- getLine
    let doStart = toLower $ head start
    case doStart of
        '1' -> mainGame Primeiro Human Human     cleanMap >> main
        '2' -> mainGame Primeiro Human Machine   cleanMap >> main
        '3' -> mainGame Primeiro Machine Machine cleanMap >> main
        _   -> putStrLn "Program ended."
