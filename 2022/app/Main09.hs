module Main where

import qualified Data.Map as M
import qualified Data.Set as S

type Coords = (Int, Int)
type Rope = [Coords]

left (x, y) = (x - 1, y)
right (x, y) = (x + 1, y)
up (x, y) = (x, y - 1)
down (x, y) = (x, y + 1)

diff (x, y) (x', y') = (x - x', y - y')

moveHead :: String -> Coords -> Coords
moveHead "L" = left
moveHead "R" = right
moveHead "U" = up
moveHead "D" = down

moveRope1 :: String -> Rope -> Rope
moveRope1 d (h:ts) = scanl followHead ( dmoveHead h) ts

moveRope :: String -> Int -> (Rope, S.Set Coords) -> (Rope, S.Set Coords)
moveRope d 1 (r,set) = let newRope = moveRope1 d r
                       in (newRope, S.insert (last newRope) set)
moveRope d n (r,set) = moveRope d (n-1) (moveRope d 1 (r,set))

followHead :: Coords -> Coords -> Coords
followHead newHead oldTail = let (dx, dy) = diff newHead oldTail in
  if abs dx < 2 && abs dy < 2 then oldTail
  else if abs dx == 0 then (fst oldTail, snd oldTail + if dy < 0 then -1 else 1)
  else if abs dy == 0 then (fst oldTail + if dx < 0 then -1 else 1, snd oldTail)
  else (fst oldTail + if dx < 0 then -1 else 1, snd oldTail + if dy < 0 then -1 else 1)

execute :: String -> (Rope, S.Set Coords) -> (Rope, S.Set Coords)
execute l = let [d,n] = words l in moveRope d (read n)

main = do
  content <- readFile "09-input.txt"
--  mapM_ putStrLn $ lines content

  let rope1 = take 2 (repeat (0,0))
  let (_, s1) = foldl (flip execute) (rope1, S.empty) (lines content)
  print $ S.size s1

  let rope2 = take 10 (repeat (0,0))
  let (_, s2) = foldl (flip execute) (rope2, S.empty) (lines content)
  print $ S.size s2