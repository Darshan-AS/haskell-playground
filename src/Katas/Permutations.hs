module Katas.Permutations where

import Data.List (delete, nub)

permutations :: Eq a => Int -> [a] -> [[a]]
permutations 0 _ = [[]]
permutations _ [] = []
permutations n xs = [x : p | x <- nub xs, p <- permutations (n - 1) $ delete x xs]
