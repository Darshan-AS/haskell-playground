module Exercises.List where

-- List comprehensions exercises

-- Pythagorean triplents
pyths :: Int -> [(Int, Int, Int)]
pyths n = [(a, b, c) | a <- [1 .. n], b <- [1 .. n], c <- [1 .. n], a ^ 2 + b ^ 2 == c ^ 2]

-- A positive integer is perfect if it is equal to sum of it's factors except itself
perfects :: Int -> [Int]
perfects n = [x | x <- [1 .. n], perfect x]
  where
    perfect :: Int -> Bool
    perfect n = n == (sum . init . factors) n
    factors :: Int -> [Int]
    factors n = [x | x <- [1 .. n], n `mod` x == 0]

scalarProduct :: [Int] -> [Int] -> Int
scalarProduct xs ys = sum [x * y | (x, y) <- zip xs ys]
