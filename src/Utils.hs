module Utils where

split :: Char -> String -> [String]
split c s = case rest of
                []     -> [chunk]
                _:rest -> chunk : split c rest
  where (chunk, rest) = break (==c) s

sliding :: Int -> [a] -> [[a]]
sliding k xs
  | length xs == k = [xs]
  | otherwise      = take k xs : sliding k (tail xs)

groupByCount :: Int -> [a] -> [[a]]
groupByCount k xs 
  | length xs == k = [xs]
  | otherwise      = take k xs : groupByCount k (drop k xs)
