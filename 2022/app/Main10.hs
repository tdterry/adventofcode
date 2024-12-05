module Main where

import qualified Data.Map as M
import qualified Data.Set as S

data Operation = Nop | Addx Int deriving (Show, Eq)

parse :: String -> [Operation]
parse s = let (op:val) = words s
          in case op of
               "noop" -> [Nop]
               "addx" -> [Nop, Addx (read (head val))]

doOp :: Operation -> Int -> Int
doOp Nop x = x
doOp (Addx x) y = x + y

strength :: Int -> [Int] -> Int
strength n values = n * (head . drop (n-1) $ values)

renderLine :: Int -> [Int] -> String
renderLine n [] = ""
renderLine n (v:vs) = let sprite = map (+v) [-1, 0, 1]
                  in (if (n `elem` sprite)
                      then '#'
                      else '.') : renderLine (n+1) vs

render :: Int -> [Int] -> [String]
render n [] = []
render n values = (renderLine n (take 40 values)) : render 0 (drop 40 values)

main = do
  content <- readFile "10-input.txt"
--  mapM_ putStrLn $ lines content

  let program = concatMap parse $ lines content
--  print program

  let values = scanl (\acc op -> doOp op acc) 1 program

  print $ sum . map (\n -> strength n values) $ [20, 60, 100, 140, 180, 220]

  mapM_ putStrLn $ render 0 values