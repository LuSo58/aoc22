module Lib (task01, task02) where

import Task02 (task02Impl)

import Data.List (sortBy)
import Data.List.Split (splitOn)
import Data.Ord (Down (Down), comparing)

printResults :: (Show a) => a -> a -> IO ()
printResults part1 part2 = putStrLn $ "Part 1: " ++ show part1 ++ "\nPart 2: " ++ show part2

task01 :: IO ()
task01 = do
  input <- getContents
  let elves = map (sum . map read) (splitOn [""] $ lines input) :: [Integer]
  let part1 = maximum elves
  let part2 = sum $ take 3 $ sortBy (comparing Data.Ord.Down) elves
  printResults part1 part2


task02 :: IO ()
task02 = do
  input <- getContents
  let (part1, part2) = task02Impl input
  printResults part1 part2