{-# LANGUAGE TupleSections #-}
module Day5 where

import qualified Data.Map as M
import           Utils    ( split )

type Cord = (Int, Int)

type Line = (Cord, Cord)

type Diagram = M.Map Cord Int

range :: Int -> Int -> [Int]
range start end | start <= end = [start .. end]
                | otherwise    = [start, start-1 .. end]

findHorizenAndVerticalCordsFromLine :: Line -> [Cord]
findHorizenAndVerticalCordsFromLine ((x1, y1), (x2, y2))
  | x1 == x2 = map (x1,) (range y1 y2)
  | y1 == y2 = map (,y1) (range x1 x2)
  | otherwise = []

findAllCordsFromLine :: Line -> [Cord]
findAllCordsFromLine line@((x1, y1), (x2, y2)) 
  | abs (x1 - x2) == abs (y1 - y2) = zip (range x1 x2) (range y1 y2)
  | otherwise = findHorizenAndVerticalCordsFromLine line

fillInDiagram :: Diagram -> [Cord] -> Diagram
fillInDiagram  = foldl addCord
  where addCord d cord = case M.lookup cord d of
                         Nothing -> M.insert cord 1 d
                         Just _  -> M.adjust (+1) cord d

solution :: (Line -> [Cord]) -> [Line] -> Int
solution findCordsMethod
  = length 
  . filter (>=2) 
  . M.elems 
  . foldl fillInDiagram M.empty 
  . map findCordsMethod

parseLines :: String -> [Line]
parseLines = map parseLine . lines
  where parseLine s = let splited = split ' ' s in (parseCord (head splited), parseCord (last splited))
        parseCord s = let splited = split ',' s in (read (head splited), read (head (tail splited)))

main :: IO ()
main = do
  lines <- parseLines <$> readFile "inputs/input5"
  print $ solution findHorizenAndVerticalCordsFromLine lines
  print $ solution findAllCordsFromLine lines
