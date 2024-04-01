module Main where

import SmallestKSets

main :: IO ()
main = do
  let list = [-1, 2, -3, 4, -5]
  let k = 3
  -- let list = [x * (-1) ^ x | x <- [1 .. 100]]
  -- let k = 15
  -- let list = [24, -11, -34, 42, -24, 7, -19, 21]
  -- let k = 6
  -- let list = [3, 2, -4, 3, 2, -5, -2, 2, 3, -3, 2, -5, 6, -2, 2, 3]
  -- let k = 8
  let sortedTuples = take k (sortTuples (subarrayDetails list))
  printTuples sortedTuples list
