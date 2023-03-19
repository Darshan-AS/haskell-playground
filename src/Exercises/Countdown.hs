module Exercises.Countdown where

import Control.Monad (filterM)
import Data.List (delete, nub)
import Katas.Permutations (permutations)

-- Given a list of numbers and a target numberm find all arithmetic expressions using the numbers that evalute to target
-- Numbers can't be reused
-- Allowed operations are: addition, subtraction, multiplication, division
-- All intermediate results must be positive

data Op = Add | Sub | Mul | Div

instance Show Op where
  show Add = " + "
  show Sub = " - "
  show Mul = " * "
  show Div = " / "

data Expr = Val Int | App Op Expr Expr

instance Show Expr where
  show (Val x) = show x
  show (App op le re) = "(" ++ show le ++ show op ++ show re ++ ")"

-- To ensure intermediate results are positive and optimizations to avoid commutative and identity repitations
valid :: Op -> Int -> Int -> Bool
valid Add x y = x >= y
valid Sub x y = x > y
valid Mul x y = x >= y && x /= 1 && y /= 1
valid Div x y = x `mod` y == 0 && y /= 0

apply :: Op -> Int -> Int -> Int
apply Add x y = x + y
apply Sub x y = x - y
apply Mul x y = x * y
apply Div x y = x `div` y

eval :: Expr -> [Int]
eval (Val x) = [x | x > 0]
eval (App op le re) = [apply op lv rv | lv <- eval le, rv <- eval re, valid op lv rv]

split :: [a] -> [([a], [a])]
split xs = [splitAt i xs | i <- [1 .. (length xs - 1)]]

choices :: Eq a => [a] -> [[a]]
choices xs = [p | n <- [0 .. length xs], p <- permutations n xs]

makeExpr :: [Int] -> [(Expr, Int)]
makeExpr [] = []
makeExpr [x] = [(Val x, x) | x > 0]
makeExpr xs =
  [ (App op le re, apply op lv rv)
    | (ls, rs) <- split xs,
      (le, lv) <- makeExpr ls,
      (re, rv) <- makeExpr rs,
      op <- [Add, Sub, Mul, Div],
      valid op lv rv
  ]

countdown :: [Int] -> Int -> [Expr]
countdown xs n = [e | c <- choices xs, (e, v) <- makeExpr c, v == n]
