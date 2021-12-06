module Day6 where

import           Data.List.Split ( splitOn )
import qualified Data.Map.Strict as M

type Tally = M.Map Int Int

initTally :: [Int] -> Tally
initTally = foldr (\n t -> M.insertWith (+) n 1 t) initialMap
  where initialMap = M.fromList $ zip [0..8] (repeat 0)

step :: Tally -> Tally
step t =
  let (zs : lst) = M.elems t
      -- shift tally to left
      t' = M.fromList (zip [0..8] lst)
      -- append new eights
      t'' = M.insert 8 zs t'
      -- add new sixes
  in M.adjust (+ zs) 6 t''

solution :: Int -> Tally -> Int
solution days tally = sum $ M.elems $ iterate step tally !! days

parseTimers :: String -> [Int]
parseTimers = map read . splitOn ","

main :: IO ()
main = do
  timers <- parseTimers <$> readFile "inputs/input6"
  let tally = initTally timers
  print $ solution 80 tally
  print $ solution 256 tally
