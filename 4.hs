import System.IO (hFlush, stdout)

-- Define a função que resolve o problema da Torre de Hanói
torreDeHanoi :: Int -> String -> String -> String -> [String]
torreDeHanoi 0 _ _ _ = []
torreDeHanoi n origem destino auxiliar = 
  torreDeHanoi (n - 1) origem auxiliar destino ++
  ["Mover disco " ++ show n ++ " de " ++ origem ++ " para " ++ destino] ++
  torreDeHanoi (n - 1) auxiliar destino origem

-- Função para solicitar e ler um número inteiro do usuário
readInt :: IO Int
readInt = do
  input <- getLine
  return (read input :: Int)

-- Função principal para interação com o usuário
main :: IO ()
main = do
  -- Solicita o número de discos ao usuário
  putStr "Digite o número de discos: "
  hFlush stdout
  n <- readInt
  
  -- Verifica se o número de discos é válido
  if n <= 0 then
    putStrLn "O número de discos deve ser maior que zero."
  else do
    -- Pinos: "A" é a origem, "C" é o destino, "B" é o auxiliar
    let movimentos = torreDeHanoi n "A" "C" "B"
    
    -- Imprime os movimentos necessários
    putStrLn "Os movimentos necessários são:"
    mapM_ putStrLn movimentos
