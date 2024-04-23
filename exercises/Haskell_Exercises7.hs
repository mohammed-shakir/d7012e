module Haskell_Exercises7 where

-- Exercise 18.1
getNput :: IO ()
getNput = do
  line <- getLine
  putStrLn line

getInt :: IO Int
getInt = do
  line <- getLine
  return (read line :: Int)

put4times :: String -> IO ()
put4times str = do
  putStrLn str
  putStrLn str
  putStrLn str
  putStrLn str

getPalindrome :: IO ()
getPalindrome = do
  line <- getLine
  if line == reverse line
    then putStrLn "It's a palindrome!"
    else putStrLn "It's not a palindrome!"

-- Exercise 18.2
addInt :: IO ()
addInt = do
  x <- getInt
  y <- getInt
  print (x + y)

-- Exercise 18.3
sumNIntegers :: IO ()
sumNIntegers = do
  n <- getInt
  sumNIntegers' n 0

sumNIntegers' :: Int -> Int -> IO ()
sumNIntegers' 0 sum = print sum
sumNIntegers' n sum = do
  x <- getInt
  sumNIntegers' (n - 1) (sum + x)

-- Exercise 18.4
palindromeLoop :: IO ()
palindromeLoop = do
  putStrLn "Enter a string:"
  line <- getLine
  if null line
    then return ()
    else do
      if line == reverse line
        then putStrLn "It's a palindrome!"
        else putStrLn "It's not a palindrome!"
      palindromeLoop

-- Exercise 18.5
iSort :: Int -> [Int] -> [Int]
iSort x [] = [x]
iSort x (y : ys)
  | x <= y = x : y : ys
  | otherwise = y : iSort x ys

sortList :: [Int] -> [Int]
sortList = foldr iSort []

sortLoop :: IO ()
sortLoop = do
  putStrLn "Enter a number:"
  x <- getInt
  if x == 0
    then return ()
    else do
      sortLoop' [x]

sortLoop' :: [Int] -> IO ()
sortLoop' xs = do
  x <- getInt
  if x == 0
    then print (sortList xs)
    else sortLoop' (iSort x xs)

-- Exercise 18.6
mapF :: (a -> b) -> IO a -> IO b
mapF f ioa = do
  a <- ioa
  return (f a)
