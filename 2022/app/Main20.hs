module Main where

import Data.Char
import Data.Sort
import Debug.Trace
import qualified Data.Map as M
import qualified Data.Set as S
import Control.Monad
import Data.List
import Data.List.Split
import Text.Regex.PCRE

code :: M.Map Integer Integer -> [Integer] -> [Integer]
code m s = map (m M.!) s

move1 :: [Integer] -> Integer -> [Integer] -> Int -> [Integer]
move1 before x after n
  | n == 0 = before ++ [x] ++ after
  | n == length after = before ++ after ++ [x]
  | n > length after = take (n - length after) before ++ [x] ++ drop (n - length after) before ++ after
  | otherwise = before ++ (take n after) ++ [x] ++ (drop n after)

move :: M.Map Integer Integer -> [Integer] -> Integer -> [Integer]
move m xs n
  | n == toInteger (length xs) = xs
  | otherwise =
     let move = m M.! n
         move' = fromInteger (move `mod` (toInteger (length xs -1)))
         before = takeWhile (/= n) xs
         (x:after) = dropWhile (/= n) xs
     in move1 before x after move'

solve :: M.Map Integer Integer -> [Integer] -> [Integer]
solve m xs = foldl (move m) xs [0..toInteger (length xs)-1]

solveN :: M.Map Integer Integer -> [Integer] -> Integer -> [Integer]
solveN m xs n
  | n == 0 = xs
  | otherwise = solveN m (solve m xs) (n-1)

main :: IO ()
main =
  readFile "20-input.txt" >>= \content ->

--  (mapM_ putStrLn (lines content)) >>

  let numbers = M.fromList $ zip [0..] (map read $ lines content) :: M.Map Integer Integer
--  in print numbers >>

      order = take (M.size numbers) [0..]
--  in print order >>

--  let order' = move numbers (move numbers (move numbers order 0) 1) 2
--  in print (code numbers order') >>

      order'' = solve numbers order
--  in print order'' >>

--  in print (code numbers order'') >>

      (Just zeroLocation) = findIndex (== 0) (code numbers order'')
--  in print zeroLocation >>

  in print (
      (numbers M.! (order'' !! ((zeroLocation + 1000) `mod` length order''))) +
      (numbers M.! (order'' !! ((zeroLocation + 2000) `mod` length order''))) +
      (numbers M.! (order'' !! ((zeroLocation + 3000) `mod` length order'')))
    ) >>

  let numbers = M.fromList $ zip [0..] (map ((*811589153) . read) $ lines content) :: M.Map Integer Integer
      order = take (M.size numbers) [0..]
      order'' = solveN numbers order 10
      (Just zeroLocation) = findIndex (== 0) (code numbers order'')
  in print (
      (numbers M.! (order'' !! ((zeroLocation + 1000) `mod` length order''))) +
      (numbers M.! (order'' !! ((zeroLocation + 2000) `mod` length order''))) +
      (numbers M.! (order'' !! ((zeroLocation + 3000) `mod` length order'')))
    ) >>

  return ()
