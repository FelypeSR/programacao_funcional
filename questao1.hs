--questao 1
maiorPosicao :: (Ord a, Num a) => [a] -> (a, Int)
maiorPosicao xs = maiorPosicaoAux xs 1 (head xs) 0
  where
   
    maiorPosicaoAux :: (Ord a, Num a) => [a] -> Int -> a -> Int -> (a, Int)
    maiorPosicaoAux [] _ maxVal maxPos = (maxVal, maxPos)
    maiorPosicaoAux (y:ys) currentIndex maxVal maxPos
      | y > maxVal = maiorPosicaoAux ys (currentIndex + 1) y currentIndex
      | otherwise  = maiorPosicaoAux ys (currentIndex + 1) maxVal maxPos


main :: IO ()
main = do
  let lista = [11, 51, 2, 3, 4]
  let resultado = maiorPosicao lista
  print resultado  -- Output (51, 1)
