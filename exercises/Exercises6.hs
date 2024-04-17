module Exercises6 where

-- Exercise 16.9
class Deque d where
  empty :: d a
  isEmpty :: d a -> Bool
  addFront :: a -> d a -> d a
  addBack :: a -> d a -> d a

newtype ListDeque a = ListDeque [a] deriving (Show)

instance Deque ListDeque where
  empty = ListDeque []
  isEmpty (ListDeque []) = True
  isEmpty _ = True
  addFront x (ListDeque list) = ListDeque (x : list)
  addBack x (ListDeque list) = ListDeque (list ++ [x])

data TwoListDeque a = TwoListDeque [a] [a] deriving (Show)

instance Deque TwoListDeque where
  empty = TwoListDeque [] []
  isEmpty (TwoListDeque [] []) = True
  isEmpty _ = False
  addFront x (TwoListDeque front back) = TwoListDeque (x : front) back
  addBack x (TwoListDeque front back) = TwoListDeque front (x : back)

-- Exercise 17.2
subLists, subSequences :: [a] -> [[a]]
subLists [] = [[]]
subLists (x : xs) = subLists xs ++ map (x :) (subLists xs)
subSequences [] = [[]]
subSequences (x : xs) = subSequences xs ++ map (x :) (subSequences xs)

-- Exercise 17.4
scalarProduct :: Num a => [a] -> [a] -> a
scalarProduct xs ys = sum (zipWith (*) xs ys)