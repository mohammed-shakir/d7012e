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
iter :: Int -> (Int -> Int) -> Int -> Int
iter n f x
  | n == 0 = x
  | otherwise = iter (n - 1) f (f x)

mySquare :: Int -> Int
mySquare n = n * n

-- Exercise 9.10
twoPowerN :: Int -> Int
twoPowerN n = iter n double 1
  where
    double x = 2 * x

-- Exercise 9.11
sumOfSquares2 :: Int -> Int
sumOfSquares2 n = foldr1 (+) (map (\x -> x * x) [1 .. n])

-- Exercise 9.16
helperFunction :: Int -> Bool
helperFunction n
  | n <= 10 = True
  | otherwise = False

filterFirst :: (a -> Bool) -> [a] -> [a]
filterFirst p (x : xs)
  | p x == False = xs
  | otherwise = x : filterFirst p xs

-- Exercise 9.17
myReverse :: [a] -> [a]
myReverse [] = []
myReverse (x : xs) = myReverse xs ++ [x]

filterLast :: (a -> Bool) -> [a] -> [a]
filterLast p lst = myReverse (filterFirst p (myReverse lst))

-- Exercise 10.3
composeList :: [a -> a] -> (a -> a)
composeList = foldr (.) id

-- Exercise 10.7
myFlip :: (a -> b -> c) -> (b -> a -> c)
myFlip f x y = f y x

-- Exercise 10.8
checkChar :: Char -> Bool
checkChar x = not (elem x [' ', '\t', '\n'])

-- Exercise 10.13
sec :: [Int] -> [Int]
sec = map (\x -> x + 1) . filter (\y -> y > -1)