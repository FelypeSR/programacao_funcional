
import Data.List (find)

-- Função para calcular o n-ésimo número de Catalan
numeroCatalan :: Integer -> Integer
numeroCatalan n = fatorial (2 * n) `div` (fatorial (n + 1) * fatorial n)
  where
    fatorial :: Integer -> Integer
    fatorial 0 = 1
    fatorial k = k * fatorial (k - 1)

-- Função para verificar se um número pertence à sequência de números de Catalan
pertenceCatalan :: Integer -> Bool
pertenceCatalan x = any (== x) [numeroCatalan n | n <- [1..x]]

-- Função para contar quantos números da sequência de Catalan existem abaixo de um determinado número
quantidadeCatalan :: Integer -> Int
quantidadeCatalan n = length $ takeWhile (<= n) [numeroCatalan i | i <- [1..]]

-- Função para calcular a soma de todos os números de Catalan dentro de um intervalo [a, b]
somaCatalan :: Integer -> Integer -> Integer
somaCatalan a b = sum [numeroCatalan i | i <- [1..], let c = numeroCatalan i, c >= a && c <= b]

main :: IO ()
main = do
  putStrLn "Digite um número para realizar as operações relacionadas aos números de Catalan:"
  input <- getLine
  let numero = read input :: Integer
  putStrLn $ "Número de Catalan para " ++ show numero ++ ": " ++ show (numeroCatalan numero)
  putStrLn $ "Pertence à sequência de Catalan? " ++ show (pertenceCatalan numero)
  putStrLn $ "Quantidade de números de Catalan abaixo de " ++ show numero ++ ": " ++ show (quantidadeCatalan numero)
  putStrLn $ "Soma dos números de Catalan entre 1 e " ++ show numero ++ ": " ++ show (somaCatalan 1 numero)
 -- a demora da entrega é devido a uma confusão(de minha parte) no horário, onde achei que seria até a meia noite de hoje, e, também, alguns bugs de função no qual demorei para corrigir, mas o código está funcionando corretamente.
 -- porém precisa da biblioteca Data.List(find) para funcionar corretamente.
