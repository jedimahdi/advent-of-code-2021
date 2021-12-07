module Day6 where

import           Data.List.Split ( splitOn )

type Tally = [Int]

initTally :: [Int] -> Tally
initTally xs = map (\x -> length $ filter (== x) xs) [0..8]

step :: Tally -> Tally
step [zero, one, two,   three, four, five,       six,   seven, eight]
   = [one,  two, three, four,  five, six, seven + zero, eight, zero]
step _ = error "not possible"

solution :: Int -> Tally -> Int
solution days tally = sum $ iterate step tally !! days

parseTimers :: String -> [Int]
parseTimers = map read . splitOn ","

main :: IO ()
main = do
  timers <- parseTimers <$> readFile "inputs/in"
  let tally = initTally timers
  print $ solution 80 tally
  print $ solution 256 tally
