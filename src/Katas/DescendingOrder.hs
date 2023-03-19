module Katas.DescendingOrder where

import Data.List (sort)

digits :: Integral a => a -> [a]
digits 0 = []
digits n = n `mod` 10 : digits (n `div` 10)

undigits :: Integral a => [a] -> a
undigits = foldl (\n x -> n * 10 + x) 0

descendingOrder :: Integral a => a -> a
descendingOrder = undigits . reverse . sort . digits

descendingOrder' :: (Integral a, Show a, Read a) => a -> a
descendingOrder' = read . reverse . sort . show
