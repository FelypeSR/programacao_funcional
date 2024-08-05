-- Define the dictionary
dic_10 :: [(Int, String)]
dic_10 = [(0, "zero"), (1, "um"), (2, "dois"), (3, "três"), (4, "quatro"), (5, "cinco"), (6, "seis"), (7, "sete"), (8, "oito"), (9, "nove")]

-- Function to convert an integer to its string representation using the dictionary
conv_int_str :: [Int] -> [String]
conv_int_str xs = map (\x -> lookupInt x dic_10) xs
  where
    lookupInt x dic = case lookup x dic of
                        Just str -> str
                        Nothing -> error "Número fora do intervalo"

-- Example usage
main :: IO ()
main = print $ conv_int_str [2, 5, 0]
