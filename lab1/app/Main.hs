module Main where

import Test (addIntegers, variableInt)
import Test2 (variableString)

main :: IO ()
main = do
  putStrLn "Hello, Haskell!"
  print variableInt
  print (addIntegers 1 2)
  putStrLn variableString