module Day2_2 where

import           Data.Maybe ( mapMaybe )

data Position = Position { horizontalPosition :: Int
                         , depth              :: Int
                         , aim                :: Int
                         }
  deriving (Eq, Show)

initialPosition :: Position
initialPosition = Position { horizontalPosition = 0, depth = 0, aim = 0 }

data Command
  = Forward Int
  | Down Int
  | Up Int
  deriving (Eq, Show)

runCommand :: Command -> Position -> Position
runCommand (Forward x) p = p { horizontalPosition = horizontalPosition p + x, depth = depth p + aim p * x }
runCommand (Down x) p    = p { aim = aim p + x }
runCommand (Up x) p      = p { aim = aim p - x }

runCommands :: [Command] -> Position -> Position
runCommands cs p = foldl (flip runCommand) p cs

parseCommand :: String -> Maybe Command
parseCommand s = case words s of
                  ["forward", x] -> Just $ Forward (read x)
                  ["down", x]    -> Just $ Down (read x)
                  ["up", x]      -> Just $ Up (read x)
                  _              -> Nothing

parseCommands :: String -> [Command]
parseCommands = mapMaybe parseCommand . lines

main :: IO ()
main = do
  commands <- parseCommands <$> readFile "inputs/input2"
  let lastState = runCommands commands initialPosition
  print $ depth lastState * horizontalPosition lastState

