module Main where

import Exercises1
import Exercises2
import Exercises3

main :: IO ()
main = do
  -- print (unzipFun [(1,2), (3,4)])
  print $ isSubsequentString "ace" "abcdefg"