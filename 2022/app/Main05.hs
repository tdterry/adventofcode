module Main where

import Data.List.Split

parseStackLine :: String -> [String]
parseStackLine "" = []
parseStackLine s = take 4 s : parseStackLine (drop 4 s)

parseStackItem :: String -> Char
parseStackItem (_:x:_) = x

parseStacks :: [String] -> [String]
parseStacks = map ( map parseStackItem . parseStackLine)

headOfEach :: [String] -> String
headOfEach [] = ""
headOfEach x = map head x

tailOfEach :: [String] -> [String]
tailOfEach [] = []
tailOfEach x = map tail x

zipStacks :: [String] -> [String]
zipStacks [] = []
zipStacks ("":_) = []
zipStacks x = headOfEach x : zipStacks (tailOfEach x)

removeFrom :: Int -> [String] -> ([String], String)
removeFrom 1 (x:xs) = ((drop 1 x):xs, take 1 x)
removeFrom n (x:xs) = let (rest, c) = removeFrom (n-1) xs in (x:rest, c)

removeNFrom :: Int -> Int -> [String] -> ([String], String)
removeNFrom n 1 (x:xs) = ((drop n x):xs, take n x)
removeNFrom n from (x:xs) = let (rest, c) = removeNFrom n (from-1) xs in (x:rest, c)

addTo :: Int -> String -> [String] -> [String]
addTo 1 c (x:xs) = (c ++ x):xs
addTo n c (x:xs) = x : addTo (n-1) c xs

parseInstruction :: String -> (Int, Int, Int)
parseInstruction x = let (_:n:_:from:_:to:[]) = splitOn " " x in
  (read n, read from, read to)


moveNFromTo :: Int -> Int -> Int -> [String] -> [String]
moveNFromTo 0 _ _ stacks = stacks
moveNFromTo n from to stacks = let
  (fromStack, c) = removeFrom from stacks
  toStack = addTo to c fromStack
  in moveNFromTo (n-1) from to toStack

moveNFromTo_2 :: Int -> Int -> Int -> [String] -> [String]
moveNFromTo_2 0 _ _ stacks = stacks
moveNFromTo_2 n from to stacks = let
  (fromStack, c) = removeNFrom n from stacks
  in addTo to c fromStack

execute :: [(Int, Int, Int)] -> [String] -> [String]
execute [] stacks = stacks
execute ((n, from, to):xs) stacks = execute xs (moveNFromTo n from to stacks)

execute_2 :: [(Int, Int, Int)] -> [String] -> [String]
execute_2 [] stacks = stacks
execute_2 ((n, from, to):xs) stacks = execute_2 xs (moveNFromTo_2 n from to stacks)

main = do
  content <- readFile "05-input.txt"

  let parsedLines = parseStacks . takeWhile (\x -> let (_:a:_) = x in a /= '1') . lines $ content
--  print parsedLines

  let stacks = map (filter (\c -> c /= ' ')) . zipStacks $ parsedLines
--  print stacks

  let instructions = map parseInstruction . drop 1 . dropWhile (\x -> x /= "") . lines $ content
--  print instructions


  print . headOfEach $ execute instructions stacks
  print . headOfEach $ execute_2 instructions stacks

