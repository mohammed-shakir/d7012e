module Haskell_Exercises4 where

-- Exercise 12.2
numEqual :: Int -> [Int] -> Int
numEqual x xs = length [a | a <- xs, a == x]

-- Exercise 12.3
oneLookupFirst :: (Eq a) => [(a, b)] -> a -> b
oneLookupFirst [] _ = error "Not found"
oneLookupFirst ((x, y) : xys) z
  | x == z = y
  | otherwise = oneLookupFirst xys z

oneLookupSecond :: (Eq b) => [(a, b)] -> b -> a
oneLookupSecond [] _ = error "Not found"
oneLookupSecond ((x, y) : xys) z
  | y == z = x
  | otherwise = oneLookupSecond xys z

-- Exercise 12.5
class Visible a where
  toString :: a -> String

instance Visible Int where
  toString = show

convertToString :: (Visible a) => a -> String
convertToString = toString

convertIntToString :: Int -> String
convertIntToString = toString
