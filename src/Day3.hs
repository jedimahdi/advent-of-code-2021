{-# LANGUAGE RecordWildCards #-}
module Day3 where

import           Data.Bifunctor ( bimap, first )
import qualified Data.List      as List
import           Data.Maybe     ( mapMaybe )

data BinaryDigit = Zero | One deriving (Eq, Show)

type Binary = [BinaryDigit]

convertBinaryToInt :: Binary -> Int
convertBinaryToInt b = go (reverse b) 0
  where go [] _          = 0
        go (Zero : xs) n = go xs (n + 1)
        go (One : xs) n  = 2 ^ n + go xs (n + 1)

negDigit :: BinaryDigit -> BinaryDigit
negDigit One  = Zero

negDigit Zero = One

neg :: Binary -> Binary
neg = map negDigit

mostCommonBinaryDigit :: BinaryDigit -> Binary -> BinaryDigit
mostCommonBinaryDigit preferdDigit b = case uncurry compare p of
                                         EQ -> preferdDigit
                                         GT -> Zero
                                         LT -> One
  where p = bimap length length $ List.partition (== Zero) b

leastCommonBinaryDigit :: BinaryDigit -> Binary -> BinaryDigit
leastCommonBinaryDigit preferdDigit = negDigit . mostCommonBinaryDigit (negDigit preferdDigit)

data Ratings = Ratings { gammaRating           :: Int
                       , epsilonRating         :: Int
                       , oxygenGeneratorRating :: Int
                       , co2ScrubberRating     :: Int
                       }

findPart2Index :: [Binary] -> (Binary -> BinaryDigit) -> Int
findPart2Index xs commonFunction = go (zip xs [0..])
  where go bs | length bs == 1 = snd $ head bs
              | otherwise = go (map (first tail) $ filter (isCommon bs) bs)
        isCommon bs (b, _) = commonFunction (head (List.transpose (map fst bs))) == head b

solution :: [Binary] -> Ratings
solution bs = Ratings {..}
  where
    trans = List.transpose bs
    gammaRatingBinary = map (mostCommonBinaryDigit Zero) trans
    gammaRating = convertBinaryToInt gammaRatingBinary
    epsilonRating = convertBinaryToInt $ neg gammaRatingBinary
    oxygenGeneratorRating = convertBinaryToInt (bs !! findPart2Index bs (mostCommonBinaryDigit One))
    co2ScrubberRating = convertBinaryToInt (bs !! findPart2Index bs (leastCommonBinaryDigit Zero))

parseBinaryDigit :: Char -> Maybe BinaryDigit
parseBinaryDigit '0' = Just Zero
parseBinaryDigit '1' = Just One
parseBinaryDigit _   = Nothing

parseBinary :: String -> Binary
parseBinary = mapMaybe parseBinaryDigit

main :: IO ()
main = do
  inputLines <- lines <$> readFile "inputs/input3"
  let b = parseBinary <$> inputLines
  let Ratings {..} = solution b
  print $ gammaRating * epsilonRating
  print $ oxygenGeneratorRating * co2ScrubberRating
