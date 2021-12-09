{-# LANGUAGE LambdaCase #-}
module Day8 where

import           Data.Bifunctor  ( bimap )
import           Data.List       ( delete, (\\) )
import           Data.List.Split ( splitOn )
import qualified Data.Map        as M

type Entry = ([String], [String])


data Segment = A | B | C | D | E | F | G deriving (Eq, Show)

type Patterns = M.Map Char Segment

part1 :: [Entry] -> Int
part1 = sum
      . map length
      . map (filter (\x -> length x == 4 || length x == 2 || length x == 3 || length x == 7))
      . map snd

getNLength :: Int -> [String] -> String
getNLength n = head . filter ((== n) . length)

findPatterns :: [String] -> Patterns
findPatterns patterns = let  a = head $ getNLength 3 patterns \\ getNLength 2 patterns
                             c = head $ getNLength 7 patterns \\ (head . filter (\p -> length (p \\ getNLength 2 patterns) == 5) . filter ((== 6) . length) $ patterns)
                             f = head $ delete c (getNLength 2 patterns)
                             e = head $ delete a $ getNLength 7 patterns \\ (head . filter (\p -> length (p \\ getNLength 4 patterns) == 2) . filter ((== 6) . length) $ patterns)
                             d = head $ getNLength 7 patterns \\ (head . filter (\p -> length (p \\ (getNLength 4 patterns \\ getNLength 2 patterns)) == 5) . filter ((== 6) . length) $ patterns)
                             b = head $ delete d $ getNLength 4 patterns \\ getNLength 2 patterns
                             g = head $ delete a (head . filter (\p -> length p == 2) . map (\p -> p \\ getNLength 4 patterns) . filter ((== 6) . length) $ patterns)
                         in M.fromList [(a, A), (b, B), (c, C), (d, D), (e, E), (f, F), (g, G)]

getValue :: [Segment] -> Int
getValue s | length s == 2 = 1
    | length s == 3 = 7
    | length s == 4 = 4
    | length s == 5 && B `elem` s = 5
    | length s == 5 && E `elem` s = 2
    | length s == 5 = 3
    | length s == 6 && D `elem` s && E `elem` s = 6
    | length s == 6 && D `elem` s = 9
    | length s == 6 = 0
    | length s == 7 = 8
    | otherwise = error "not possible"

f :: Entry -> Int
f entry = read $ mconcat $ map show values
  where
    patterns = findPatterns $ fst entry
    convertOutputs = map (map (\c -> case M.lookup c patterns of
                                       Just x  -> x
                                       Nothing -> error "not possible"
                                      )) $ snd entry
    values = map getValue convertOutputs

part2 :: [Entry] -> Int
part2 entries = sum $ map f entries

parseEntries :: String -> [Entry]
parseEntries = map parseEntry . lines
  where parseEntry = bimap (splitOn " ") (splitOn " ") . (\[a, b] -> (a, b)) . splitOn " | "

main :: IO ()
main = do
  entries <- parseEntries <$> readFile "inputs/input8"
  print $ part1 entries
  print $ part2 entries
