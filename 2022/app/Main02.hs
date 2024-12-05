module Main where

import Data.List.Split

data Play = Rock | Paper | Scissors deriving (Show, Eq)
data Outcome = Win | Lose | Tie deriving (Show, Eq)
type Hand = (Play, Play)

play_scores = [(Rock, 1), (Paper, 2), (Scissors, 3)]
outcome_scores = [(Win, 6), (Lose, 0), (Tie, 3)]

find_score list x = snd . head . filter ((== x) . fst) $ list

score_play = find_score play_scores
score_outcome = find_score outcome_scores

-- opponent -> me -> outcome
outcome :: Play -> Play -> Outcome
outcome Rock Paper = Win
outcome Paper Scissors = Win
outcome Scissors Rock = Win
outcome x y = if x == y then Tie else Lose

parse_opponent :: Char -> Play
parse_opponent 'A' = Rock
parse_opponent 'B' = Paper
parse_opponent 'C' = Scissors

parse_me :: Char -> Play
parse_me 'X' = Rock
parse_me 'Y' = Paper
parse_me 'Z' = Scissors

desired_outcome :: Char -> Outcome
desired_outcome 'X' = Lose
desired_outcome 'Y' = Tie
desired_outcome 'Z' = Win

my_play :: Play -> Outcome -> Play
my_play opponent desired = head . filter (\x -> outcome opponent x == desired) $ [Rock, Paper, Scissors]

score :: Hand -> Int
score (opponent, me) = score_outcome (outcome opponent me) + score_play me

parse_hand_1 :: String -> Hand
parse_hand_1 (x:' ':y:"") = (parse_opponent x, parse_me y)

parse_hand_2 :: String -> Hand
parse_hand_2 (x:' ':y:"") = (opponent, me) where
    opponent = parse_opponent x
    me = my_play opponent (desired_outcome y)

main = do
  content <- readFile "02-input.txt"
  let hands_1 = map parse_hand_1 . splitOn "\n" $ content
--  print hands
  print . sum . map score $ hands_1

  let hands_2 = map parse_hand_2 . splitOn "\n" $ content
--  print hands_2
  print . sum . map score $ hands_2
