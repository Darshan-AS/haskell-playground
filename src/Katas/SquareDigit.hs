module Katas.SquareDigit where

import Data.Char (digitToInt)
import Data.List (intercalate)

squareDigit :: Int -> Int
squareDigit = applyToAbs (read . join . map (^ 2) . digits)
  where
    digits :: (Integral a, Show a) => a -> [Int]
    digits = map digitToInt . show

    join :: Show a => [a] -> String
    join = intercalate "" . map show

    applyToAbs :: Real a => (a -> a) -> a -> a
    applyToAbs f x = signum x * f (abs x)

squareDigit' :: Int -> Int
squareDigit' = applyToAbs $ read . concatMap (show . (^ 2) . digitToInt) . show
  where
    applyToAbs :: Real a => (a -> a) -> a -> a
    applyToAbs f x
      | x < 0 = negate . f . negate $ x
      | otherwise = f x
