module Aula4 where
-- no powershell: 'char' "string" 
-- data [Int] = [] || Int: [Int]
-- data [Char] = [] || Char: [Char] -- escreve 'char' : 'char' : []

-- Tipo Recursivo
-- Quantos valores há no tipo lista?
-- dois! Nil e Elem
-- O Nil tem 0 campos
-- O Elem tem 2 campos: um Int e uma Lista
data Lista = Nil | Elem Int Lista
    deriving Show

comprimento :: Lista -> Int
comprimento Nil = 0 -- caso base da recursao
-- x :: Int, resto :: Lista
comprimento (Elem x resto) = 1 + comprimento resto

--String = [Char]
-- data [Char] = [] || Char: [Char]
-- Guards: sao condicoes booleanas e imitam o if

-- tira vogal da string
-- filter (\x -> not (elem x "AEIOUaeiou")) "FATEC"
tiraVogal :: String -> String
tiraVogal [] = [] -- caso base
tiraVogal (letra : resto)
    | elem letra "AEIOUaeiou" = tiraVogal resto
    | otherwise = letra : tiraVogal resto

-- tira pares da lista
-- filter (\x -> odd x) [0,1,2,3,4,5,6,7,8,9,10]
-- filter odd [0,1,2,3,4,5,6,7,8,9,10]
tiraPar :: [Int] -> [Int]
tiraPar [] = [] -- caso base
tiraPar (x : xs)
    | even x = tiraPar xs
    | otherwise = x : tiraPar xs

-- contar qtd de pares
contaPar :: [Int] -> Int
contaPar [] = 0
contaPar (x : xs)
    | even x = 1 + contaPar xs
    | otherwise = contaPar xs

-- tirar palavras com mais de 5 letras
tiraCincoP :: [String] -> [String]
tiraCincoP [] = []
tiraCincoP (x : xs)
    | length x > 5 = tiraCincoP xs
    | otherwise = x : tiraCincoP xs

-- tirar 5 letras de palavras acima de 5 letras
tiraCincoL :: [String] -> [String]
tiraCincoL [] = []
tiraCincoL (x : xs)
    | length x > 5 = drop 5 x : tiraCincoL xs
    | otherwise = x : tiraCincoL xs

-- LAMBDA

-- \x -> 3*x) x
triplo :: Double -> Double
triplo x = 3*x

-- map (+1) [1,2,3,4,5]
-- fazer todos os exercicios do capitulo 1 usando so map e filter