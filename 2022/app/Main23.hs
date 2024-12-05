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

type ElfPosition = (Int, Int)
type ElfPositions = S.Set ElfPosition

data Direction = N | S | W | E deriving (Show, Eq, Ord)
type ProposedPositions = M.Map ElfPosition ElfPosition

parse :: String -> [(Int, Int)]
parse s =
  let ls = zip[0..] $ lines s
      elves = concatMap (\(y,xs) -> map (\(x,c) -> (x,y)) xs) . map (fmap (filter (\(_,c) -> c == '#') . zip [0..])) $ ls
  in elves

calcProposedPosition :: [Direction] -> ElfPositions -> ElfPosition -> ElfPosition
calcProposedPosition [] _ p = p -- no where to go
calcProposedPosition (d:ds) elves (x,y)
  | d == N = if any (\dx -> S.member (x+dx,y-1) elves) [-1..1] then calcProposedPosition ds elves (x,y) else (x,y-1)
  | d == S = if any (\dx -> (x+dx,y+1) `S.member` elves) [-1..1] then calcProposedPosition ds elves (x,y) else (x,y+1)
  | d == W = if any (\dy -> (x-1,y+dy) `S.member` elves) [-1..1] then calcProposedPosition ds elves (x,y) else (x-1,y)
  | d == E = if any (\dy -> (x+1,y+dy) `S.member` elves) [-1..1] then calcProposedPosition ds elves (x,y) else (x+1,y)

calcProposedPositions :: ProposedPositions -> [Direction] -> ElfPositions -> [ElfPosition] -> ProposedPositions
calcProposedPositions pp _ _ [] = pp
calcProposedPositions pp ds elves (p:ps) =
  let (x,y) = p
      surrounding = [(x+dx,y+dy) | dx <- [-1..1], dy <- [-1..1], (dx,dy) /= (0,0)]
      surroundingElves = filter (\e -> e `S.member` elves) surrounding
      pp' = if null surroundingElves then pp else M.insert p (calcProposedPosition ds elves p) pp
  in calcProposedPositions pp' ds elves ps

moveElves :: ProposedPositions -> [ElfPosition] -> ElfPositions
moveElves pp [] = S.empty
moveElves pp (e:es) =
  let e' = M.findWithDefault e e pp
  in if (length . filter (==e') $ M.elems pp) > 1
    then S.insert e $ moveElves pp es
    else S.insert e' $ moveElves pp es

solve :: Int -> [Direction] -> ElfPositions -> ElfPositions
solve 0 _ elves = elves
solve n dirs@(d:ds) elves =
  let pp = calcProposedPositions M.empty dirs elves $ S.toList elves
      elves' = moveElves pp $ S.toList elves
  in solve (n-1) (ds ++ [d]) elves'

solve2 :: Int -> [Direction] -> ElfPositions -> Int
solve2 n dirs@(d:ds) elves =
  let pp = calcProposedPositions M.empty dirs elves $ S.toList elves
  in if M.size pp == 0 then n
    else trace ("n=" ++ show n) solve2 (n+1) (ds ++ [d]) (moveElves pp (S.toList elves))

answer :: ElfPositions -> Int
answer elves =
  let minX = minimum $ map fst $ S.toList elves
      maxX = maximum $ map fst $ S.toList elves
      minY = minimum $ map snd $ S.toList elves
      maxY = maximum $ map snd $ S.toList elves
  in (maxX - minX + 1) * (maxY - minY + 1) - length elves


main :: IO ()
main =
  readFile "23-input.txt" >>= \content ->

  let elves = S.fromList $ parse content
      dirs = [N, S, W, E]
  in return () >>

--   mapM_ print (parse content) >>

--  print (solve 1 dirs elves) >>
--  print (solve 2 dirs elves) >>
--  print (solve 3 dirs elves) >>
--  print (solve 10 dirs elves) >>
  print (answer (solve 10 dirs elves)) >>

  print (solve2 1 dirs elves) >>

  return ()
