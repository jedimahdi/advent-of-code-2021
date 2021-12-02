module Day1 where

part1 :: [Int] -> Int
part1 xs = length $ filter id $ zipWith (<) xs (tail xs)

part2 :: [Int] -> Int
part2 xs = part1 $ zipWith3 (\a b c -> a + b + c) xs (tail xs) (tail (tail xs))

main :: IO ()
main = do
  input <- readFile "inputs/input1"
  let result = part2 . map read . lines $ input
  print result
