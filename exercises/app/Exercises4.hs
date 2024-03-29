module Exercises4 where

-- Exercise 12.2
numEqual :: Int -> [Int] -> Int
numEqual x xs = length [a | a <- xs, a == x]
