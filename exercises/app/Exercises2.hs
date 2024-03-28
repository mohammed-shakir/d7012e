module Exercises2 where

-- Exrecise 5.2
orderTriple :: (Int, Int, Int) -> (Int, Int, Int)
orderTriple (x, y, z)
    | x <= y && y <= z = (x, y, z)
    | x <= z && z <= y = (x, z, y)
    | y <= x && x <= z = (y, x, z)
    | y <= z && z <= x = (y, z, x)
    | z <= x && x <= y = (z, x, y)
    | otherwise = (z, y, x)

-- Exrecise 5.10
divisors :: Int -> [Int]
divisors n = [x | x <- [1..n], n `mod` x == 0]

isPrime :: Int -> Bool
isPrime n = divisors n == [1, n]

-- Exrecise 5.11
matches :: Int -> [Int] -> [Int]
matches n lst = [x | x <- lst, x == n]

elem2 :: Int -> [Int] -> Bool
elem2 n lst = not (null (matches n lst))

-- Exrecise 5.22
onSeperateLine :: [String] -> String
onSeperateLine [] = ""
onSeperateLine [x] = x
onSeperateLine (x:xs) = x ++ "\n" ++ onSeperateLine xs

-- Exrecise 5.23
duplicate :: String -> Int -> String
duplicate str n = concat [str | _ <- [1..n]]

-- Exrecise 5.24
pushRight :: String -> String
pushRight str = replicate (length str) ' ' ++ str

-- Exrecise 5.29


-- Exrecise 5.10


-- Exrecise 7.2


-- Exrecise 7.3


-- Exrecise 7.4


-- Exrecise 7.5


-- Exrecise 7.7


-- Exrecise 7.8
reverse2 :: [a] -> [a]
reverse2 [] = []
reverse2 (x : xs) = reverse2 xs ++ [x]

unzipFun :: [(a, b)] -> ([a], [b])
unzipFun [] = ([], [])
unzipFun ((x, y) : xy) = (x : xs, y : ys)
    where
        (xs, ys) = unzipFun xy

-- Exercise 7.9
iSort :: Int -> [Int] -> [Int]
iSort x [] = [x]
iSort x (y : ys)
    | x <= y = x : y : ys
    | otherwise = y : iSort x ys

sortList :: [Int] -> [Int]
sortList = foldr iSort []

-- Exercise 7.14
myDrop :: Int -> [Int] -> [Int]
myDrop n lst = [lst!!n | n <- [n..length lst - 1]]

-- Exercise 7.18
isSubString :: String -> String -> Bool
isSubString [] _ = True
isSubString _ [] = False
isSubString (x:xs) (y:ys)
    | x == y = isSubString xs ys
    | otherwise = isSubString (x:xs) ys

isSubsequentString :: String -> String -> Bool
isSubsequentString [] _ = True
isSubsequentString _ [] = False
isSubsequentString (x:xs) ys = checkSubsequent xs ys
  where
    checkSubsequent _ [] = False
    checkSubsequent xs (y:yt)
      | x == y    = matchConsecutive xs yt
      | otherwise = checkSubsequent (x:xs) yt

    matchConsecutive [] _ = True
    matchConsecutive _ [] = False
    matchConsecutive (a:as) (b:bs)
      | a == b    = matchConsecutive as bs
      | otherwise = False

-- Exercise 7.25
isPalin :: String -> Bool
isPalin [] = True
isPalin lst = lst == reverse2 lst

-- Exercise 7.26
subst :: String -> String -> String -> String
subst oldSub newSub st =
    if oldSub `isPrefixOf` st
        then newSub ++ drop (length oldSub) st
        else case st of
            "" -> ""
            (x:xs) -> x : subst oldSub newSub xs

-- Helper function to check if one string is the prefix of another
isPrefixOf :: (Eq a) => [a] -> [a] -> Bool
isPrefixOf [] _ = True
isPrefixOf _ [] = False
isPrefixOf (x:xs) (y:ys) = x == y && isPrefixOf xs ys