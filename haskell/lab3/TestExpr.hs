{- Test for Expr-}
module TestExpr where

import Dictionary qualified
import Expr (fromString, value)

dict :: Dictionary.T String Integer
dict =
  Dictionary.insert ("x", 1) $
    Dictionary.insert ("y", 2) Dictionary.empty

testValue :: String -> Integer
testValue string = value (fromString string) dict

n1 :: Integer
n1 = testValue "1" {- 1 -}

n2 :: Integer
n2 = testValue "x" {- 1 -}

n3 :: Integer
n3 = testValue "x+y" {- 3 -}

n4 :: Integer
n4 = testValue "x-y-y" {- -3 -}

n21 :: Integer
n21 = testValue "1/(2-y)" {-  Expr.value: division by 0 -}

n31 :: Integer
n31 = testValue "2+z" {-  Expr.value: undefined variable z -}

runTests :: IO ()
runTests = do
  print n1
  print n2
  print n3
  print n4
  print n21
  print n31
