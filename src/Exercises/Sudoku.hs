module Exercises.Sudoku where

import Data.List ((\\))

type Grid = Matrix Value

type Matrix a = [Vector a]

type Vector a = [a]

type Value = Char

-- Also avialable in Data.List
transpose :: [[a]] -> [[a]]
transpose [] = []
transpose ([] : yss) = transpose yss
transpose ((x : xs) : xss) = (x : [y | (y : _) <- xss]) : transpose (xs : [ys | (_ : ys) <- xss])

chop :: Int -> [a] -> [[a]]
chop _ [] = []
chop n xs = take n xs : chop n (drop n xs)

rows :: Matrix a -> [Vector a]
rows = id

columns :: Matrix a -> [Vector a]
columns = transpose

boxes :: Matrix a -> [Vector a]
boxes = unpack . map columns . pack
  where
    split = chop 3
    pack = split . map split
    unpack = map concat . concat

noDups :: Eq a => [a] -> Bool
noDups [] = True
noDups (x : xs) = notElem x xs && noDups xs

valid :: Grid -> Bool
valid g = all noDups (rows g) && all noDups (columns g) && all noDups (boxes g)

solve1 :: Grid -> [Grid]
solve1 = filter valid . explode . choices

choices :: Grid -> Matrix [Value]
choices = map $ map choice
  where
    choice '.' = ['1' .. '9']
    choice v = [v]

explode :: Matrix [a] -> [Matrix a]
explode = cartesianProduct . map cartesianProduct

cartesianProduct :: [[a]] -> [[a]]
cartesianProduct [] = [[]]
cartesianProduct (xs : xss) = [y : ys | y <- xs, ys <- cartesianProduct xss]

prune :: Matrix [Value] -> Matrix [Value]
prune = pruneBy boxes . pruneBy columns . pruneBy rows
  where
    pruneBy f = f . map reduce . f

reduce :: Vector [Value] -> Vector [Value]
reduce xss = [if single xs then xs else xs \\ singles | xs <- xss]
  where
    singles = concat $ filter single xss

single :: [a] -> Bool
single [_] = True
single _ = False

solve2 :: Grid -> [Grid]
solve2 = filter valid . explode . prune . choices

-- value x is fix point of a function f iff f(x) = x
fix :: Eq a => (a -> a) -> a -> a
fix f x = if x == x' then x else fix f x'
  where
    x' = f x

solve3 :: Grid -> [Grid]
solve3 = filter valid . explode . fix prune . choices

void :: Matrix [Value] -> Bool
void = any $ any null

safe :: Matrix [Value] -> Bool
safe m = all consistent (rows m) && all consistent (columns m) && all consistent (boxes m)
  where
    consistent = noDups . concat . filter single

blocked :: Matrix [Value] -> Bool
blocked m = void m || not (safe m)

search :: Matrix [Value] -> [Grid]
search m
  | blocked m = []
  | all (all single) m = explode m
  | otherwise = [g | m' <- expand m, g <- search . prune $ m']

expand :: Matrix [Value] -> [Matrix [Value]]
expand m = [rows1 ++ [row1 ++ [c] : row2] ++ rows2 | c <- cs]
  where
    (rows1, row : rows2) = span (all single) m
    (row1, cs : row2) = span single row

solve4 :: Grid -> [Grid]
solve4 = search . prune . choices

-- Solvable only using the basic rules:
easy :: Grid
easy =
  [ "2....1.38",
    "........5",
    ".7...6...",
    ".......13",
    ".981..257",
    "31....8..",
    "9..8...2.",
    ".5..69784",
    "4..25...."
  ]

-- First gentle example from sudoku.org.uk:
gentle :: Grid
gentle =
  [ ".1.42...5",
    "..2.71.39",
    ".......4.",
    "2.71....6",
    "....4....",
    "6....74.3",
    ".7.......",
    "12.73.5..",
    "3...82.7."
  ]

-- First diabolical example:
diabolical :: Grid
diabolical =
  [ ".9.7..86.",
    ".31..5.2.",
    "8.6......",
    "..7.5...6",
    "...3.7...",
    "5...1.7..",
    "......1.9",
    ".2.6..35.",
    ".54..8.7."
  ]

-- First "unsolvable" (requires backtracking) example:
unsolvable :: Grid
unsolvable =
  [ "1..9.7..3",
    ".8.....7.",
    "..9...6..",
    "..72.94..",
    "41.....95",
    "..85.43..",
    "..3...7..",
    ".5.....4.",
    "2..8.6..9"
  ]

-- Minimal sized grid (17 values) with a unique solution:
minimal :: Grid
minimal =
  [ ".98......",
    "....7....",
    "....15...",
    "1........",
    "...2....9",
    "...9.6.82",
    ".......3.",
    "5.1......",
    "...4...2."
  ]

-- Empty grid:
blank :: Grid
blank = replicate n (replicate n '.')
  where
    boxsize = 3
    n = boxsize ^ 2
