import Numeric (showHex)
import Data.Char (digitToInt)

-- 1. Utilizando Expressões-ZF
-- a. Ímpares entre 1 e 100
impares1a100 :: [Int]
impares1a100 = [x | x <- [1..100], odd x]

-- b. Pares entre 10 e 100
pares10a100 :: [Int]
pares10a100 = [x | x <- [10..100], even x]

-- c. Ímpares entre 1 e N
impares1aN :: Int -> [Int]
impares1aN n = [x | x <- [1..n], odd x]

-- d. Números entre 1 e N que são múltiplos de 3 e 5 ao mesmo tempo
multiplosDe3e5 :: Int -> [Int]
multiplosDe3e5 n = [x | x <- [1..n], x `mod` 3 == 0, x `mod` 5 == 0]

-- e. Tuplas entre 1 e N, contendo o número e seu respectivo quadrado
numerosEQuadrados :: Int -> [(Int, Int)]
numerosEQuadrados n = [(x, x^2) | x <- [1..n]]

-- f. Tuplas com os índices de uma matriz 3x4
indices3x4 :: [(Int, Int)]
indices3x4 = [(i, j) | i <- [1..3], j <- [1..4]]

-- g. Tuplas com os índices de uma matriz NxM
indicesNxM :: Int -> Int -> [(Int, Int)]
indicesNxM n m = [(i, j) | i <- [1..n], j <- [1..m]]

-- 2. Função que retorna os n primeiros números da sequência de Fibonacci
listaFibonacci :: Int -> [Int]
listaFibonacci n = take n fibonacci
  where
    fibonacci = 0 : 1 : zipWith (+) fibonacci (tail fibonacci)

-- 3. Função que converte uma String binária para uma String hexadecimal
-- Converte uma string binária para um número decimal
binToDec :: String -> Int
binToDec = foldl (\acc x -> acc * 2 + digitToInt x) 0

-- Converte um número decimal para uma string hexadecimal
decToHex :: Int -> String
decToHex n = showHex n ""

-- Divide a string binária em blocos de 4 bits, pois cada bloco corresponde a um dígito hexadecimal
splitInChunksOf :: Int -> String -> [String]
splitInChunksOf _ [] = []
splitInChunksOf n xs = take n xs : splitInChunksOf n (drop n xs)

-- Completa a string binária para que seu tamanho seja múltiplo de 4
padLeft :: Int -> a -> [a] -> [a]
padLeft n x xs = replicate (n - length xs) x ++ xs

-- Converte uma string binária para hexadecimal
binToHex :: String -> String
binToHex bin = concatMap (decToHex . binToDec) chunks
  where
    paddedBin = padLeft ((length bin + 3) `div` 4 * 4) '0' bin
    chunks = splitInChunksOf 4 paddedBin

-- Função principal para converter binário para hexadecimal
binaryToHexadecimal :: String -> String
binaryToHexadecimal bin = map toUpper (binToHex bin)
  where toUpper c = if c >= 'a' && c <= 'f' then toEnum (fromEnum c - 32) else c

-- Função main para testes
main :: IO ()
main = do
  -- Testes das expressões-ZF
  print impares1a100
  print pares10a100
  print $ impares1aN 50
  print $ multiplosDe3e5 50
  print $ numerosEQuadrados 10
  print indices3x4
  print $ indicesNxM 3 4
  
  -- Teste da sequência de Fibonacci
  print $ listaFibonacci 10
  
  -- Teste da conversão de binário para hexadecimal
  let binaryString = "11011110101011011011111011101111"
  print $ binaryToHexadecimal binaryString
