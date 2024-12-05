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
import Data.Word
import Data.Bits


data Distance = Dist Int | Infinity deriving (Show, Eq, Ord)
data Vertex = Vertex String Int deriving (Show, Eq, Ord)
data Edge = Edge String String Int deriving (Show, Eq, Ord)
data Graph = Graph [Vertex] [Edge] deriving (Show, Eq, Ord)
type CachedShortestPaths = M.Map (Vertex, Vertex) Distance
type CachedScore = M.Map Int Int -- step => score

instance Num Distance where
    (+) (Dist a) (Dist b) = Dist (a + b)
    (+) _ _ = Infinity

parseLine :: String -> (Vertex, [Edge])
parseLine s =
  let regex = "Valve ([A-Z]{2}) has flow rate=(\\d+); tunnels? leads? to valves? (.+)"
      [[_, n, r, ts]] = s =~ regex :: [[String]]
      vertex = Vertex n (read r)
      edges = map (\n2 -> Edge n n2 1) . (splitOn ", ") $ ts
  in (vertex, edges)

data Coord = Coord Int Int deriving (Show, Eq, Ord)
data Piece = Piece [String] deriving (Show, Eq, Ord)

type BytePiece = [Word8]
type BytePieces = [BytePiece]
type ByteBoard = [Word8]

bytePieces :: BytePieces
bytePieces = [
  map fromIntegral [0xf0], -- ####....

  map fromIntegral [0x40, 0xe0, 0x40], -- .#......
                                       -- ###.....
                                       -- .#......

  map fromIntegral [0x20, 0x20, 0xe0], -- ..#.....
                                       -- ..#.....
                                       -- ###.....

  map fromIntegral [0x80, 0x80, 0x80, 0x80], -- #.......
                                             -- #.......
                                             -- #.......
                                             -- #.......

  map fromIntegral [0xc0, 0xc0] -- ##......
                                -- ##......
  ]


canMoveLeft :: BytePiece -> Bool
canMoveLeft p = all (\x -> x .&. 0x80 == 0) p  -- #.......

canMoveRight :: BytePiece -> Bool
canMoveRight p = all (\x -> x .&. 0x02 == 0) p -- ......#.

moveBytePiece :: Char -> BytePiece -> BytePiece
moveBytePiece '<' p = if canMoveLeft p then map (`shift` 1) p else p
moveBytePiece '>' p = if canMoveRight p then map (`shift` (-1)) p else p

canFitBytePiece :: BytePiece -> ByteBoard -> Bool
canFitBytePiece p b
  | length p > length b = False
  | otherwise = all (\(x, y) -> x .&. y == 0) (zip p b)

dropBytePiece :: BytePiece -> ByteBoard -> ByteBoard
dropBytePiece p (b:bs)
  | canFitBytePiece p bs = dropBytePiece p bs
  | otherwise = zipWith (.|.) p (b:bs) ++ drop (length p) bs

printByteBoard :: ByteBoard -> IO ()
printByteBoard b = mapM_ putStrLn ((map (\r -> "|" ++ map (\i -> if r `shift` (-i) .&. 1 == 1 then '#' else '.') [7,6..1] ++ "|") b) ++ ["+-------+", ""])

solveBytePiece :: String -> BytePiece -> ByteBoard -> (String, ByteBoard)
solveBytePiece (m:ms) p board@(b:bs) =
  let p' = moveBytePiece m p
      p'' = if canFitBytePiece p' board then p' else p
      board' = zipWith (.|.) p'' board ++ drop (length p'') board
  in if canFitBytePiece p'' bs
      then let (moves, b') = solveBytePiece (ms++[m]) p'' bs
           in (moves, dropWhile (==0) (b:b'))
      else (ms ++ [m], dropWhile (==0) board')

nextBytePiece :: BytePieces -> (BytePiece, BytePieces)
nextBytePiece (p:ps) = (p, ps ++ [p])

solveByteBoard :: Integer -> String -> BytePieces -> ByteBoard -> (String, ByteBoard)
solveByteBoard 0 moves _ board = (moves, board)
solveByteBoard n moves pieces board =
  let (p, pieces') = nextBytePiece pieces
      p' = moveBytePiece '>' (moveBytePiece '>' p)
      (moves', board') = solveBytePiece moves p' $ take (length p' + 3) (repeat 0x00) ++ board
      (moves'', board'') = solveByteBoard (n-1) moves' pieces' board'
  in (moves'', board'')

type CachedSolution = M.Map (BytePiece, String, ByteBoard) (Integer, Integer)

-- 0. cache key = (piece, moves, board)
-- 1. check cache for previous solution
-- 2. if found, that was the height the last time this piece was dropped
--  3.  add multiples of the height difference to the current height
--  4.  solve the rest of the board
-- 5. if not found, solve the board with the piece dropped
--  6.  store the _previous_ height in the cache (because we want to compare before the dropped piece in the future)
-- 7. repeat

cachedSolve :: CachedSolution -> Integer -> String -> BytePieces -> ByteBoard -> (Integer, String, ByteBoard, CachedSolution)
cachedSolve cache 0 moves _ board = (toInteger (length board), moves, board, cache)
cachedSolve cache n moves pieces board =
  let (p, pieces') = nextBytePiece pieces
      key = (p, take 50 moves, take 50 board)
      in case M.lookup key cache of
        Just (nOld, hOld) -> trace ("found cached solution " ++ " n=" ++ show n ++ " nOld=" ++ show nOld ++ " hOld=" ++ show hOld ++ " height=" ++ show (length board)) $
                  let cycleLen = nOld - n
                      cycleHeight = fromIntegral (length board) - hOld
                      skippedCycles = n `div` cycleLen
                      skippedHeight = skippedCycles * cycleHeight
                      skippedPieces = skippedCycles * cycleLen
                      (m', b') = solveByteBoard (n-skippedPieces) moves pieces board
                  in (skippedHeight + fromIntegral (length b'), m', b', cache)
        Nothing ->
          let p' = moveBytePiece '>' (moveBytePiece '>' p)
              (moves', board') = solveBytePiece moves p' $ take (length p' + 3) (repeat 0x00) ++ board
              cache' = M.insert key (n, fromIntegral (length board)) cache
              (height'', moves'', board'', cache'') = cachedSolve cache' (n-1) moves' pieces' board'
          in (height'', moves'', board'', cache'')

main :: IO ()
main =
  readFile "17-input.txt" >>= \content ->

--  (mapM_ putStrLn (lines content)) >>

  let moves = content
      [piece1, piece2, piece3, piece4, piece5] = bytePieces
      board = []
  in mapM_ printByteBoard bytePieces >>

--  let (h, b) = solveByteBoard 47 moves bytePieces []
--  in print h >>
--  printByteBoard b >>
--
  let (h, _, _, _) = cachedSolve M.empty 2022 moves bytePieces []
  in print h >>

  let (h, _, _, _) = cachedSolve M.empty 1000000000000 moves bytePieces []
  in print h >>

  return ()
