module Task02 (task02Impl) where
import Data.List.Split (splitOn)

data Option = Rock | Paper | Scissors
    deriving (Eq, Show)

parseOption :: String -> Option
parseOption "A" = Rock
parseOption "X" = Rock
parseOption "B" = Paper
parseOption "Y" = Paper
parseOption "C" = Scissors
parseOption "Z" = Scissors
parseOption _ = error "Invalid input"

optionValue :: Option -> Integer
optionValue Rock = 1
optionValue Paper = 2
optionValue Scissors = 3

winnings :: Option -> Option -> Integer
winnings opponent me
    | (opponent, me) `elem` [(Rock, Paper), (Paper, Scissors), (Scissors, Rock)] = 6
    | (opponent, me) `elem` [(Paper, Rock), (Scissors, Paper), (Rock, Scissors)] = 0
    | otherwise = 3

parseLinePart1 :: String -> Integer
parseLinePart1 line = optionValue rhs + winnings lhs rhs
    where [lhs, rhs] = map parseOption $ splitOn " " line

data Result = Win | Draw | Lose
    deriving (Eq, Show)

parseResult :: String -> Result
parseResult "X" = Lose
parseResult "Y" = Draw
parseResult "Z" = Win
parseResult _ = error "Invalid input"

resultValue :: Result -> Integer
resultValue Lose = 0
resultValue Draw = 3
resultValue Win = 6

beats :: Option -> Option
beats Rock = Paper
beats Paper = Scissors
beats Scissors = Rock

losesTo :: Option -> Option
losesTo Paper = Rock
losesTo Scissors = Paper
losesTo Rock = Scissors

chooseOption :: Result -> Option -> Option
chooseOption Draw = id
chooseOption Win = beats
chooseOption Lose = losesTo

parseLinePart2 :: String -> Integer
parseLinePart2 line = resultValue result + optionValue (chooseOption result opponent)
    where [lhs, rhs] = splitOn " " line
          opponent = parseOption lhs
          result = parseResult rhs

task02Impl :: String -> (Integer, Integer)
task02Impl input = (part1, part2)
    where part1 = sum $ map parseLinePart1 $ lines input
          part2 = sum $ map parseLinePart2 $ lines input