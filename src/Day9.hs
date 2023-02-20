module Day9 where

import           Data.List       ( transpose )
import           Data.List.Split ( chunksOf )

type Matrix = [[Int]]

data Order = Higher | Lower | Equal | Wall deriving (Eq, Show)

compareOrder :: Ord a => a -> a -> Order
compareOrder a b | a > b = Higher
                 | a < b = Lower
                 | otherwise = Equal

negOrder :: Order -> Order
negOrder Higher = Lower
negOrder Lower  = Higher
negOrder x      = x

getOrders :: [Int] -> [(Order, Int, Order)]
getOrders xs = (Wall, head xs, negOrder ((\(a, _, _) -> a) (head middleNumbers))) : middleNumbers ++ [(negOrder ((\(_, _, a) -> a) (last middleNumbers)), last xs, Wall)]
  where middleNumbers = zipWith3 (\a b c -> (compareOrder a b, b, compareOrder c b)) xs (tail xs) (tail (tail xs))

checkLowPoint :: (Order, Int, Order) -> (Order, Int, Order) -> (Int, Bool)
checkLowPoint (left, a, right) (up, _, down) | (left == Higher || left == Wall)
                                            && (right == Higher || right == Wall)
                                            && (up == Higher || up == Wall)
                                            && (down == Higher || down == Wall) = (a, True)
                                             | otherwise = (a, False)

part1 :: Matrix -> Int
part1 xs = sum $ map (((sum . map (+1)) . map fst) . filter snd) (zipWith (zipWith checkLowPoint) (map getOrders xs) (transpose . map getOrders $ transpose xs))

parseMatrix :: String -> Matrix
parseMatrix = map (map read . chunksOf 1) . lines

main :: IO ()
main = do
  matrix <- parseMatrix <$> readFile "inputs/input9"
  print $ part1 matrix
