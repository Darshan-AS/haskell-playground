{-# OPTIONS_GHC -Wno-incomplete-patterns #-}

module Katas.MinMax where

import Data.Bifunctor (Bifunctor (bimap))

minMax :: (Ord a) => [a] -> (a, a)
minMax [x] = (x, x)
minMax (x : xs) = bimap (min x) (max x) (minMax xs)

minMax' :: (Ord a) => [a] -> (a, a)
minMax' (x : xs) = foldr (\k (l, h) -> (min k l, max k h)) (x, x) xs
