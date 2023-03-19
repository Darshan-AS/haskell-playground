module Katas.PileOfCubes where

-- The parameter of the function findNb (find_nb, find-nb, findNb, ...) will be an integer m and you have to return the integer n such as n^3 + (n-1)^3 + ... + 1^3 = m if such a n exists or -1 if there is no such n.

findNb :: Integer -> Integer
findNb m = do
  let x = takeWhile (<= m) . scanl (+) 0 . map (^ 3) $ [1 ..]
  toInteger $ if last x == m then length x - 1 else -1
