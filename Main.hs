import Control.Monad()
import Data.Char
import Data.List
import Data.List.Split
import Data.Sequence()

{- Definir tipos novos -}
data Jogador = Primeiro | Segundo deriving (Ord, Eq, Show, Read)
data Slot = Xis | Bola | Empty deriving (Ord, Eq, Show, Read)
type Map  = [Slot]

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

{- [Implementar] Verifica as combinações e retorna se algum jogador ganhou o jogo. Caso contrário retorna Nothing -}
getGanhador :: Map -> Maybe Jogador
getGanhador xs = Nothing

{- Substitui um valor em um indice de uma lista  -}
replace :: a -> Int -> [a] -> [a]
replace value index xs = map (\(i, val) -> if i == index then value else val) (zip [0..] xs)

{- Faz a jogada, substituindo vazio pela peça do jogador -}
modifyMap :: Map -> Jogador -> Int -> Map
modifyMap xs player jogada = replace (getSlotByPlayer player) (jogada - 1) xs

{- Cada jogada -}
mainJogada :: Map -> IO Int
mainJogada xs = do
    raw_jogada <- getLine
    let jogada = read raw_jogada
    if jogada >= 1 && jogada <= 9 then
        if xs !! (jogada - 1) == Empty then
                return jogada
        else putStrLn "Slot já ocupado."
          >> mainJogada xs
    else putStrLn "Jogada Invalida"
      >> mainJogada xs

{- Cada jogo -}
mainGame :: Jogador -> Map -> IO()
mainGame player xs = do
    putStrLn $ drawMap xs
    let ganhador = getGanhador xs
    case ganhador of
        Just x -> putStrLn $ "O ganhador foi: " ++ show x
        Nothing ->  if isGameOver xs then
                        putStrLn "Finished."
                    else do
                        putStrLn $ "Jogador " ++ show player ++ " faça sua jogada:"
                        jogada <- mainJogada xs
                        mainGame (if player == Primeiro then Segundo else Primeiro) (modifyMap xs player jogada)

{- Começar um jogo novo ou sair -}
main :: IO()
main = do
    putStrLn "Start new game ? [Y/N]"
    start <- getLine
    let doStart = toLower $ head start
    if doStart == 'y' then
           mainGame Primeiro cleanMap
        >> main
    else   putStrLn "Program ended."
