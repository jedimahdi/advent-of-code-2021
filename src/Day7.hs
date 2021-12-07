module Day7 where

import           Data.List       ( sort )
import           Data.List.Split ( splitOn )

median :: [Int] -> Int
median xs | even listLength = sumOfTwoMidElems `div` 2
          | otherwise       = midElem
  where sorted = sort xs
        listLength = length sorted
        midElem = sorted !! ((listLength + 1) `div` 2)
        sumOfTwoMidElems = midElem + sorted !! ((listLength + 1) `div` 2 - 1)

mean :: [Int] -> Int
mean xs = truncate $ fromIntegral (sum xs) / fromIntegral (length xs)

part1 :: [Int] -> Int
part1 xs = sum
         . map (abs . (median xs -))
         $ xs

part2 :: [Int] -> Int
part2 xs = sum
         . concatMap (enumFromTo 1 . abs . (mean xs -))
         $ xs

parsePositions :: String -> [Int]
parsePositions = map read . splitOn ","

main :: IO ()
main = do
  positions <- parsePositions <$> readFile "inputs/input7"
  print $ part1 positions
  print $ part2 positions
