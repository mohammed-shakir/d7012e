module SmallestKSets where

-- Tails function
tails :: [a] -> [([a], Int)]
tails xs = zip (tails' xs) [0 ..]
  where
    tails' [] = [[]]
    tails' (x : xs) = (x : xs) : tails' xs

-- All contiguous subarrays
subarrays :: [Int] -> [([Int], Int)]
subarrays xs = [(take i ys, idx) | (ys, idx) <- tails xs, i <- [1 .. length ys]]

-- Compute sum of each subarray, and save indices of head and tail
subarrayDetails :: [Int] -> [(Int, Int, Int)]
subarrayDetails xs = [(sum ys, idx, idx + length ys - 1) | (ys, idx) <- subarrays xs, not (null ys)]

-- Sort tuples by first element
insert :: (Int, Int, Int) -> [(Int, Int, Int)] -> [(Int, Int, Int)]
insert x [] = [x]
insert x (y : ys) | first x <= first y = x : y : ys | otherwise = y : insert x ys
  where
    first (a, _, _) = a

sortTuples :: [(Int, Int, Int)] -> [(Int, Int, Int)]
sortTuples = foldr insert []

-- Print the result
printTuples :: [(Int, Int, Int)] -> [Int] -> IO ()
printTuples [] _ = return ()
printTuples ((sum, i, j) : tuples) list = do
  putStrLn $ "  " ++ show sum ++ "   " ++ show (i + 1) ++ "   " ++ show (j + 1) ++ "   " ++ show (take (j - i + 1) (drop i list))
  printTuples tuples list