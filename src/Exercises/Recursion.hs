module Exercises.Recursion where

-- Recursion exercises

and' :: [Bool] -> Bool
and' [] = True
and' (b : bs) = b && and' bs

concat' :: [[a]] -> [a]
concat' [] = []
concat' (xs : xss) = xs ++ concat' xss

replicate' :: Int -> a -> [a]
replicate' 0 _ = []
replicate' n x = x : replicate' (n - 1) x

(!!!) :: [a] -> Int -> a
(x : _) !!! 0 = x
(_ : xs) !!! n = xs !!! (n - 1)

elem' :: Eq a => a -> [a] -> Bool
elem' _ [] = False
elem' x (y : ys) = x == y && elem' x ys

insertionSort :: Ord a => [a] -> [a]
insertionSort [] = []
insertionSort (x : xs) = insert x (insertionSort xs)
  where
    insert :: Ord a => a -> [a] -> [a]
    insert x [] = [x]
    insert x (y : ys) = if x <= y then x : y : ys else y : insert x ys

quickSort :: Ord a => [a] -> [a]
quickSort [] = []
quickSort (x : xs) = quickSort lefts ++ [x] ++ quickSort rights
  where
    lefts = [y | y <- xs, y < x]
    rights = [y | y <- xs, y >= x]

mergeSort :: Ord a => [a] -> [a]
mergeSort [] = []
mergeSort [x] = [x]
mergeSort xs = merge (mergeSort lefts) (mergeSort rights)
  where
    mid = length xs `div` 2
    lefts = take mid xs
    rights = drop mid xs
    merge :: Ord a => [a] -> [a] -> [a]
    merge [] ys = ys
    merge xs [] = xs
    merge (x : xs) (y : ys) = if x <= y then x : merge xs (y : ys) else y : merge (x : xs) ys
