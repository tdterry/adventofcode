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

data Expr = Humn | Val Integer | Add String String | Mul String String | Sub String String | Div String String deriving (Show)

parseLine :: String -> (String, Expr)
parseLine s =
  let regexVal = "(\\w+): (\\d+)"
      regexExpr = "(\\w+): (\\w+) ([\\+\\-\\*\\/]) (\\w+)"
      val = s =~~ regexVal :: Maybe [[String]]
      expr = s =~~ regexExpr :: Maybe [[String]]
  in case val of
    Nothing -> case expr of
      Nothing -> error "Invalid input"
      Just [[_, name, var1, op, var2]] -> case op of
        "+" -> (name, Add var1 var2)
        "-" -> (name, Sub var1 var2)
        "*" -> (name, Mul var1 var2)
        "/" -> (name, Div var1 var2)
    Just [[_, name, val]] -> (name, Val (read val))

eval :: M.Map String Expr -> String -> Integer
eval m s =
  let expr = m M.! s
  in case expr of
    Val v -> v
    Add v1 v2 -> eval m v1 + eval m v2
    Sub v1 v2 -> eval m v1 - eval m v2
    Mul v1 v2 -> eval m v1 * eval m v2
    Div v1 v2 -> eval m v1 `div` eval m v2

canEval :: M.Map String Expr -> String -> Bool
canEval m s =
  let expr = m M.! s
  in case expr of
    Humn -> False
    Val _ -> True
    Add v1 v2 -> canEval m v1 && canEval m v2
    Sub v1 v2 -> canEval m v1 && canEval m v2
    Mul v1 v2 -> canEval m v1 && canEval m v2
    Div v1 v2 -> canEval m v1 && canEval m v2

solveFor :: M.Map String Expr -> String -> Integer -> Integer
solveFor m s v =
  let expr = m M.! s
  in case expr of
    Humn -> v
    Val _ -> 0
    Add v1 v2 -> if canEval m v1 then solveFor m v2 (v - eval m v1) else solveFor m v1 (v - eval m v2)
    Sub v1 v2 -> if canEval m v1 then solveFor m v2 (eval m v1 - v) else solveFor m v1 (v + eval m v2)
    Mul v1 v2 -> if canEval m v1 then solveFor m v2 (v `div` eval m v1) else solveFor m v1 (v `div` eval m v2)
    Div v1 v2 -> if canEval m v1 then solveFor m v2 (eval m v1 `div` v) else solveFor m v1 (v * eval m v2)

solve :: M.Map String Expr -> String -> Integer
solve m s =
  let (Add x y) = m M.! s
      canEvalX = canEval m x
      canEvalY = canEval m y
  in if canEvalX
    then
      let valX = eval m x
      in solveFor m y valX
    else
      let valY = eval m y
      in solveFor m x valY

main :: IO ()
main =
  readFile "21-input.txt" >>= \content ->

  (mapM_ print (map parseLine $ lines content)) >>

  let monkeys = M.fromList . map parseLine . lines $ content
  in print (eval monkeys "root") >>

  let monkeys' = M.insert "humn" Humn monkeys
  in print (solve monkeys' "root") >>



  return ()
