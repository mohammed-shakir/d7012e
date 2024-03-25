module Main where

threeDifferent :: Int -> Int -> Int -> Bool
threeDifferent m n p = m /= n && n /= p && m /= p

fourEqual :: Int -> Int -> Int -> Int -> Bool
fourEqual a b c d = a == b && a == c && a == d

main :: IO ()
main = do
    print (threeDifferent 3 4 3)
    print (fourEqual 1 1 1 1)