module Global where

{- Definir tipos novos -}
data PlayerType = Human | Machine
data Jogador = Primeiro | Segundo deriving (Ord, Eq, Show, Read)
data Slot = Xis | Bola | Empty deriving (Ord, Eq, Show, Read)
type Map  = [Slot]

{- Combinações possíveis para a vitória -}
winCombinations :: [[Int]]
winCombinations =
    [ [1, 2, 3] -- Horizontais
    , [4, 5, 6]
    , [7, 8, 9]
    , [1, 4, 7] -- Verticais
    , [2, 5, 8]
    , [3, 6, 9]
    , [1, 5, 9] -- Diagonais
    , [7, 5, 3]
    ]

{-
    Fazer programaticamente a AI, talvez no máximo usar as tabelas `aiWinCombinations` e `aiAvoidLoseCombinations` só (que são iguais kkk enfim)
    Se funfar, depois disso, continua as proximas etapas (win, block lose, fork, block fork, center if empty, opposite enemy corner, empty corner, empty side)
-}

{- Combinações que faltam 1 jogada para a vitória -}
aiWinCombinations :: [([Int], Int)] -- Lista de tupla: (Combinacao, posParaVitoria)
aiWinCombinations =
    [ ([1, 2], 3), ([1, 3], 2), ([2, 3], 1) -- Horizontais
    , ([4, 5], 6), ([4, 6], 5), ([5, 6], 4)
    , ([7, 8], 9), ([7, 9], 8), ([8, 9], 7)
    , ([1, 4], 7), ([1, 7], 4), ([4, 7], 1) -- Verticais
    , ([2, 5], 8), ([2, 8], 5), ([5, 8], 2)
    , ([3, 6], 9), ([3, 9], 6), ([6, 9], 3)
    , ([1, 5], 9), ([1, 9], 5), ([5, 9], 1) -- Diagonais
    , ([7, 5], 3), ([7, 3], 5), ([5, 3], 7)
    ]

{- Combinações que fazem uma fork, ou seja, poder ganhar de 2 formas -}
aiForkCombinations :: [([Int], [Int], Int)] -- Lista de tupla (Combinacao, EspacosQuePrecisamEstarLivres, posParaFork)
aiForkCombinations =
    [ ([4, 2], [7, 3, 1], 1) -- lado, lado nao oposto -> quina entre elas
    , ([4, 8], [1, 9, 7], 7)
    , ([2, 6], [1, 9, 3], 3)
    , ([8, 6], [7, 3, 9], 9)
    , ([7, 3], [4, 2, 1], 1) -- quina, quina oposta -> quina
    , ([1, 9], [4, 8, 7], 7)
    , ([1, 9], [2, 6, 3], 3)
    , ([7, 3], [8, 6, 9], 9)
    , ([1, 5], [7, 9, 2, 3], 3) -- quina, centro -> quina
    --, ([3, 5], [7, 9], )
        -- desisti, acho que é melhor fazer programaticamente
        -- desisti, acho que é melhor fazer programaticamente
        -- desisti, acho que é melhor fazer programaticamente
    ]
