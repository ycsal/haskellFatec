module Aula3 where

-- O tipo eh de enumeração, assim como uma enum em Java

data Dia = Domingo 
        | Segunda 
        | Terca 
        | Quarta 
        | Quinta 
        | Sexta 
        | Sabado
        deriving (Show, Eq, Ord, Enum, Read)

-- Pattern Matching: Forma de desconstruir o tipo de entrada
agenda :: Dia -> String
agenda Segunda  = "Aula de IA"
agenda Quarta   = "Aula de Haskellao"
agenda  Quinta  = "Aulas chatas"
agenda  Sexta   = "Bar"
agenda  Sabado  = "Curso"
agenda _        = "Ninguem liga"

-- Mesma ideia porem é um switch dentro da função
agenda1 :: Dia -> String
agenda1 x = 
    case x of
        Segunda -> "Aula de IA"
        Quarta -> "Aula de Haskellao"
        Quinta -> "Aulas Chatas"
        Sexta -> "Bar"
        Sabado -> "Curso"
        _ -> "Ninguem liga"

-- numDia eh uma "funçao PARCIAL", existem entradas nao tratadas 
-- Uma funcao total trata todas as entradas
numDia :: Int -> Maybe Dia
numDia 0 = Just Domingo
numDia 1 = Just Segunda
numDia 2 = Just Terca
numDia 3 = Just Quarta
numDia 4 = Just Quinta
numDia 5 = Just Sexta
numDia 6 = Just Sabado
numDia _ = Nothing

-- numDia possui uma entrada inteira e sua saída é uma dúvida,
-- ou sai uma string de erro ou um dia valido
-- Either Erro Acerto 
numDia' :: Int -> Either String Dia
numDia' 0 = Right Domingo
numDia' 1 = Right Segunda
numDia' 2 = Right Terca
numDia' 3 = Right Quarta
numDia' 4 = Right Quinta
numDia' 5 = Right Sexta
numDia' 6 = Right Sabado
numDia' _ = Left "Dia Inválido"


-- O pattern Matching destruiu o tipo Maybe dia,
-- revelando duas possibilidades, Just Dia, onde dia é uma 
-- variavel do tipo dia (Domingo .. Sabado) ou o Nothing.
mostrar :: Maybe Dia -> String
mostrar :: (Just dia) = "O dia eh: " ++ show dia
mostrar Nothing = "O usuario digitou algo errado"

data Correncia = BRL | USD | EUR deriving Show

-- Tipo de dado algebrico do tipo registro
-- Record syntax: serve para representar dados, o nome dos campos
-- valor e correncia sao funcoes de projeto
data Moeda = Moeda{
    valor :: Double, 
    correncia :: Correncia
} deriving show

-- finalizar codigo com erro