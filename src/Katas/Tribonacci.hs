module Katas.Tribonacci where

nibonacci :: Num a => [a] -> [a]
nibonacci [] = []
nibonacci seed@(x : xs) = x : nibonacci (xs ++ [sum seed])

tribonacci :: Num a => (a, a, a) -> Int -> [a]
tribonacci (a, b, c) n = take n $ nibonacci [a, b, c]
