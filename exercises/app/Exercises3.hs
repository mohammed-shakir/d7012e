module Exercises3 where

-- Exercise 9.2
myLength :: [a] -> Int
myLength x = sum (map convert x)
  where
    convert _ = 1

-- Exercise 9.6
square :: [Int] -> [Int]
square = map sq
  where
    sq y = y * y

sumOfSquares :: [Int] -> Int
sumOfSquares x = sum (square x)

-- Exercise 9.7
minValue :: (Int -> Int) -> Int -> Int
minValue f n
  | n == 1 = f 1
  | otherwise = min (f n) (minValue f (n - 1))

myEqual :: (Int -> Int) -> Int -> Bool
myEqual f n
  | n == 0 = True
  | otherwise = f n == f (n - 1) && myEqual f (n - 1)

checkF :: (Int -> Int) -> Int -> Bool
checkF f n
  | n == 0 = True
  | otherwise = f n >= f (n - 1) && f n > 0 && checkF f (n - 1)

-- Exercise 9.9
