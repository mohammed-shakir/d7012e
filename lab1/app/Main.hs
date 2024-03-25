module Main where

import Test

square :: Int -> Int
square x = x * x

main :: IO ()
main = do
  putStrLn "Test 1"
  putStrLn varStr
  print varInt
  print (addIntegers 1 2)
  print (square 2)