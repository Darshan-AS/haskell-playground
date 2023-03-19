module Katas.GenerateParenthesis where

balancedParens :: Int -> [String]
balancedParens 0 = [""]
balancedParens n = ["(" ++ ins ++ ")" ++ outs | i <- [0 .. n - 1], ins <- balancedParens i, outs <- balancedParens (n - i - 1)]
