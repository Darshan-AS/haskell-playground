module Katas.XO where

import Data.Char (toLower)
import Data.Function (on)

-- | Returns true if the number of
-- Xs is equal to the number of Os
-- (case-insensitive)
xo :: String -> Bool
xo s = on (==) (`count` map toLower s) 'x' 'o'

xo' :: String -> Bool
xo' = (\s -> count 'x' s == count 'o' s) . map toLower

count :: Eq a => a -> [a] -> Int
count x = length . filter (x ==)
