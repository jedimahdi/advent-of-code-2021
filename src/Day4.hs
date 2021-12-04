{-# LANGUAGE TupleSections    #-}
{-# LANGUAGE TypeApplications #-}
module Day4 where

import           Data.List   ( groupBy, transpose, (\\) )
import           Utils       ( groupByCount, split )

data IsMarked = Marked | NotMarked deriving (Eq, Show)

type Board = [[(Int, IsMarked)]]

type DrawnNumbers = [Int]

isWinnerBoard :: Board -> Bool
isWinnerBoard board = checkRows board || checkRows (transpose board)
  where checkRows = any (all ((==) Marked . snd))

sumOfUnmarked :: Board -> Int
sumOfUnmarked = sum .  map (sum . map fst . filter ((==) NotMarked . snd))

applyNumber :: Int -> [Board] -> [Board]
applyNumber drawnNumber = map (map (map (\(n, marked) -> if n == drawnNumber then (n, Marked) else (n, marked))))

part1 :: DrawnNumbers -> Int -> [Board] -> Int
part1 numbers lastNumber boards | null winnerBoards = part1 (tail numbers) (head numbers) (applyNumber (head numbers) boards)
                        | otherwise = sumOfUnmarked (head winnerBoards) * lastNumber
                where winnerBoards = filter isWinnerBoard boards


part2 :: DrawnNumbers -> Int -> [Int] -> [Board] -> Int
part2 numbers lastNumber lastWinners boards | length winnerBoards /= length boards = part2 (tail numbers) (head numbers) (map fst winnerBoards) (applyNumber (head numbers) boards)
                        | otherwise = sumOfUnmarked (boards !! head ([0..length boards - 1] \\ lastWinners)) * lastNumber
                where winnerBoards = filter (isWinnerBoard . snd) . zip [0..] $ boards


parseBoards :: [String] -> [Board]
parseBoards = map (map (map ((, NotMarked) . (read @Int)) . filter (/= "") . split ' '))
            . groupByCount 5
            . filter (/= "")

parseDrawnNumbers :: String -> DrawnNumbers
parseDrawnNumbers = map read
                  . split ','

main :: IO ()
main = do
  inputLines <- lines <$> readFile "inputs/input4"
  let numbers = parseDrawnNumbers $ head inputLines
  let boards = parseBoards $ drop 2 inputLines
  print $ part1 numbers 0 boards
  print $ part2 numbers 0 [] boards

