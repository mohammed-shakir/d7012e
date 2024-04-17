module SmallestKSets where

-- Tails function
tails :: [a] -> [([a], Int)]
tails xs = zip (tails' xs) [0 ..]
  where
    tails' [] = [[]]
    tails' (x : rest) = (x : rest) : tails' rest

-- All contiguous subarrays
subarrays :: [Int] -> [([Int], Int)]
subarrays xs = [(take i ys, idx) | (ys, idx) <- tails xs, i <- [1 .. length ys]]

-- Compute sum of each subarray, and save indices of head and tail
subarrayDetails :: [Int] -> [(Int, Int, Int)]
subarrayDetails [] = error "Empty list"
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
printTuples tuples list = do
  putStrLn "size      i      j      sublist"
  let printEachTuple [] = return ()
      printEachTuple ((sumVal, i, j) : xs) = do
        let sumValStr = show sumVal
        let iStr = show (i + 1)
        let jStr = show (j + 1)
        let sublistStr = show (take (j - i + 1) (drop i list))

        let spaces = 6
        let sizeHeaderLength = 4
        let iHeaderLength = 1
        let jHeaderLength = 1

        let sumValPad = replicate (sizeHeaderLength - length sumValStr) ' '
        let iPad = replicate (iHeaderLength - length iStr + spaces) ' '
        let jPad = replicate (jHeaderLength - length jStr + spaces) ' '
        let sublistPad = replicate spaces ' '

        putStrLn $ sumValPad ++ sumValStr ++ iPad ++ iStr ++ jPad ++ jStr ++ sublistPad ++ sublistStr
        printEachTuple xs
  printEachTuple tuples

main :: IO ()
main = do
  let list = [-1, 2, -3, 4, -5]
  let k = 3
  -- let list = [x * (-1) ^ x | x <- [1 .. 100]]
  -- let k = 15
  -- let list = [24, -11, -34, 42, -24, 7, -19, 21]
  -- let k = 6
  -- let list = [3, 2, -4, 3, 2, -5, -2, 2, 3, -3, 2, -5, 6, -2, 2, 3]
  -- let k = 8
  let sortedTuples = take k (sortTuples (subarrayDetails list))
  printTuples sortedTuples list
