module Exercises1 where

-- Exercise 3.7
threeDifferent :: Int -> Int -> Int -> Bool
threeDifferent m n p = (m /= n) && (n /= p) && (p /= m)

-- Exercise 3.8
threeEqual :: Int -> Int -> Int -> Bool
threeEqual m n p = (m == n) && (n == p)

fourEqual :: Int -> Int -> Int -> Int -> Bool
fourEqual m n p q = threeEqual m n p && (p == q)

-- Exercise 3.15
numberNDroots :: Float -> Float -> Float -> Int
numberNDroots a b c
  | equation > 0 = 2
  | equation == 0 = 1
  | otherwise = 0
  where
    equation = b ^ (2 :: Integer) - 4 * a * c

-- Exercise 3.16
numberRoots :: Float -> Float -> Float -> Int
numberRoots a b c
  | a == 0 = 1
  | b == 0 = 1
  | c == 0 = 1
  | otherwise = numberNDroots a b c

-- Exercise 3.17
smallerRoot, largerRoot :: Float -> Float -> Float -> Float
smallerRoot a b c
  | numberNDroots a b c == 0 || (a == 0 && b == 0 && c == 0) = 0
  | otherwise = (-b - sqrt (b ^ 2 - 4 * a * c)) / (2 * a)
largerRoot a b c
  | numberNDroots a b c == 0 || (a == 0 && b == 0 && c == 0) = 0
  | otherwise = (-b + sqrt (b ^ 2 - 4 * a * c)) / (2 * a)

-- Exercise 4.7
-- usinf the addtion function over natural numbers, give a recursive defination of multiplication of natural numbers
funNaturalNumbers :: Int -> Int -> Int
funNaturalNumbers m n
  | n == 0 = 0
  | n == 1 = m
  | n > 0 && m > 0 = m + funNaturalNumbers m (n - 1)
  | otherwise = error "Only positive numbers are allowed"

-- Exercise 4.8
intSquareRoot :: Int -> Int
intSquareRoot n
  | n == 0 = 0
  | n == 1 = 1
  | n > 1 = intSquareRoot' n 1
  | otherwise = error "Only positive numbers are allowed"
  where
    intSquareRoot' n x
      | x * x > n = x - 1
      | otherwise = intSquareRoot' n (x + 1)

-- Exercise 4.9
f2 :: Int -> Int
f2 n
  | n < 1 = 1
  | n == 2 = 2
  | n > 10 && n < 20 = 15
  | n > 30 = 100
  | otherwise = 300

fMax :: Int -> Int
fMax n
  | n == 1 = f2 1
  | otherwise = max (f2 n) (fMax (n - 1))

-- Exercise 4.14
powerOf2 :: Int -> Int
powerOf2 n
  | n == 0 = 1
  | n == 1 = 2
  | even n = powerOf2 (n `div` 2) ^ 2
  | otherwise = powerOf2 (n `div` 2) ^ 2 * 2
