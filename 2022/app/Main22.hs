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

type Grid = M.Map (Int, Int) Char

parse :: String -> (Grid, [Instruction])
parse s =
  let parts = splitOn "\n\n" s
      ls = zip[1..] $ lines (parts !! 0)
      pts = concatMap (\(y,xs) -> map (\(x,c) -> ((x,y), c)) xs) . map (fmap (filter (\(_,c) -> c /= ' ') . zip [1..])) $ ls
  in (M.fromList pts, parseInstructions (parts !! 1))

data Orientation = U | D | L | R deriving (Show, Eq)
data Instruction = Move Int | TurnLeft | TurnRight deriving (Show, Eq)
type Vector = ((Int, Int), Orientation)



parseInstructions :: String -> [Instruction]
parseInstructions "" = []
parseInstructions s =
  let nStr = takeWhile isDigit s
      dir = head s
      n = read nStr
  in if length nStr > 0
    then (Move n) : parseInstructions (drop (length nStr) s)
    else if dir == 'L' then TurnLeft : parseInstructions (tail s)
    else if dir == 'R' then TurnRight: parseInstructions (tail s)
    else error "bad instruction"

move :: Grid -> Vector -> Instruction -> Vector
move _ v (Move 0) = v
move g v@((x,y), o) (Move n) =
  let (x',y') = case o of
        U -> (x, y-1)
        D -> (x, y+1)
        L -> (x-1, y)
        R -> (x+1, y)
      (x'',y'') = if M.member (x',y') g
        then (x',y')
        else case o of
          U -> (x, maximum . map snd . filter ((==x) . fst) . M.keys $ g)
          D -> (x, minimum . map snd . filter ((==x) . fst) . M.keys $ g)
          L -> (maximum . map fst . filter ((==y) . snd) . M.keys $ g, y)
          R -> (minimum . map fst . filter ((==y) . snd) . M.keys $ g, y)
  in case M.lookup (x'',y'') g of
    Just '#' -> trace ("blocked at " ++ show v) v
    Just '.' -> trace ("moved to " ++ show ((x'',y''), o)) move g ((x'',y''), o) (Move (n-1))
    Nothing -> error "inconceivable"
move g (p, o) TurnLeft
  | o == U = (p, L)
  | o == D = (p, R)
  | o == L = (p, D)
  | o == R = (p, U)
move g (p, o) TurnRight
  | o == U = (p, R)
  | o == D = (p, L)
  | o == L = (p, U)
  | o == R = (p, D)

move2 :: Grid -> Vector -> Instruction -> Vector
move2 _ v (Move 0) = v
move2 g v@((x,y), o) (Move n) =
  let (x',y') = case o of
        U -> (x, y-1)
        D -> (x, y+1)
        L -> (x-1, y)
        R -> (x+1, y)
      (x'',y'') = if M.member (x',y') g
        then (x',y')
        else case o of
          U -> (x, maximum . map snd . filter ((==x) . fst) . M.keys $ g)
          D -> (x, minimum . map snd . filter ((==x) . fst) . M.keys $ g)
          L -> (maximum . map fst . filter ((==y) . snd) . M.keys $ g, y)
          R -> (minimum . map fst . filter ((==y) . snd) . M.keys $ g, y)
  in case M.lookup (x'',y'') g of
    Just '#' -> trace ("blocked at " ++ show v) v
    Just '.' -> trace ("moved to " ++ show ((x'',y''), o)) move2 g ((x'',y''), o) (Move (n-1))
    Nothing -> error "inconceivable"
move2 g (p, o) TurnLeft
  | o == U = (p, L)
  | o == D = (p, R)
  | o == L = (p, D)
  | o == R = (p, U)
move2 g (p, o) TurnRight
  | o == U = (p, R)
  | o == D = (p, L)
  | o == L = (p, U)
  | o == R = (p, D)



password :: Vector -> Integer
password ((x,y), o) = toInteger y * 1000 + toInteger x * 4 + case o of
  R -> 0
  D -> 1
  L -> 2
  U -> 3

main :: IO ()
main =
  readFile "22-input-sample.txt" >>= \content ->

  let (grid, instructions) = parse content
  in return () >>

  print grid >>
  print instructions >>

  let start = ((minimum . map fst . filter ((==1) . snd) . M.keys $ grid, 1), R)
  in print start >>

  let x = foldl' (move grid) start instructions
  in print x >>
  print (password x) >>

  let x = foldl' (move2 grid) start instructions
  in print x >>
  print (password x) >>

  return ()
