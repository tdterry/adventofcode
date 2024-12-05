module Main where

import Data.Char
import Data.Sort
import Debug.Trace
import qualified Data.Map as M
import qualified Data.Set as S
import Control.Monad

data Expression = Array [Expression] | Number Int deriving (Show, Eq)

instance Ord (Expression) where
    compare (Number a) (Number b) = compare a b
    compare (Array a) (Array b) = let result = dropWhile (==EQ) . zipWith compare a $ b
                                  in case result of
                                    [] -> compare (length a) (length b)
                                    otherwise -> head result
    compare (Number a) (Array b) = compare (Array [Number a]) (Array b)
    compare (Array a) (Number b) = compare (Array a) (Array [Number b])

parseArray :: String -> [Expression] -> (Expression, String)
parseArray (',':xs) es = let (e, s') = parseArray xs es in (e, s')
parseArray (']':xs) es = (Array (reverse es), xs)
parseArray s es = let (e, s') = parseExpr s
  in parseArray s' (e:es)

parseExpr :: String -> (Expression, String)
parseExpr "" = error "Empty string"
parseExpr ('[':xs) = parseArray xs []
parseExpr s = let d = takeWhile isDigit s
                  xs = dropWhile isDigit s
              in (Number (read d), xs)

comparePairs :: [Expression] -> [Ordering]
comparePairs [] = []
comparePairs (_:[]) = []
comparePairs (a:b:xs) = (compare a b):(comparePairs xs)

main :: IO ()
main =
  readFile "13-input.txt" >>= \content ->

  (mapM_ putStrLn (lines content)) >>

  let es = map (fst . parseExpr) . filter ((>0) . length) . lines $ content
      correct =filter ((==LT) . snd) . zip [1..] . comparePairs $ es
      answer1 = sum . map fst $ correct
  in print answer1 >>

  let marker2 = Array [Array [Number 2]]
      marker6 = Array [Array [Number 6]]
      es' = marker2 : marker6 : es
      sorted = zip [1..] (sort es')
      marker2Index = fst . head . filter ((==marker2) . snd) $ sorted
      marker6Index = fst . head . filter ((==marker6) . snd) $ sorted
      answer2 = marker2Index * marker6Index

  in print answer2 >>

  mempty
