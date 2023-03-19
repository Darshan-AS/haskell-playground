module Katas.PowerSet where

import Data.Foldable (toList)

powerSet :: [a] -> [[a]]
powerSet = foldr (\x ps -> ps ++ ((x :) <$> ps)) [[]]

powerSet' :: [a] -> [[a]]
powerSet' [] = [[]]
powerSet' (x : xs) = powerSet xs ++ ((x :) <$> powerSet xs)

powerSetDups :: Eq a => [a] -> [[a]]
powerSetDups [] = [[]]
powerSetDups s@(x : xs) =
  ps ++ [replicate c x ++ p | p <- ps, c <- [1 .. count x s]]
  where
    ps = powerSetDups . filter (/= x) $ xs

count :: (Foldable f, Eq a) => a -> f a -> Int
count x = length . filter (== x) . toList
