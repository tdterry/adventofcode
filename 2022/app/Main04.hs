module Main where

import Data.List.Split

data Range = Range Int Int deriving (Show)
data Workers = Workers Range Range deriving (Show)

parseRange :: String -> Range
parseRange x = let (y: z: []) = map readInt . splitOn "-" $ x
  in Range y z

parseWorkers :: String -> Workers
parseWorkers x = let (y: z: []) = map parseRange . splitOn "," $ x
  in Workers y z

readInt :: String -> Int
readInt = read

full_overlap :: Workers -> Bool
full_overlap (Workers (Range a b) (Range c d)) = (a <= c && b >= d) || (a >= c && b <= d)

any_overlap :: Workers -> Bool
any_overlap (Workers (Range a b) (Range c d)) = max a c <= min b d

main = do
  content <- readFile "04-input.txt"
  let workers = map parseWorkers . lines $ content
--  print pairs

  let full_overlaps = map full_overlap workers
  print . length . filter id $ full_overlaps
--
  let any_overlaps = map any_overlap workers
  print . length . filter id $ any_overlaps
