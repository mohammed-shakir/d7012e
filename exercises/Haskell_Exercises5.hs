module Haskell_Exercises5 where

-- Exercise 14.1
data Temp = Cold | Hot deriving (Show)

data Season = Spring | Summer | Autumn | Winter deriving (Eq)

weather :: Season -> Temp
weather s = if s == Summer then Hot else Cold

-- Exercise 14.4 and 14.5 and 14.9
data Shape
  = Circle Float (Float, Float)
  | Rectangle Float Float (Float, Float)
  | Triangle Float Float Float (Float, Float)
  deriving (Show)

lengthOfPerimeter :: Shape -> Int
lengthOfPerimeter sh = case sh of
  Circle r _ -> round (2 * pi * r)
  Rectangle w h _ -> round (w * h)
  Triangle a b c _ -> round (a + b + c)

-- Exercise 14.6
isRegularShape :: Shape -> Bool
isRegularShape sh = case sh of
  Circle _ _ -> True
  Rectangle w h _ -> w == h
  Triangle a b c _ -> a == b && b == c

-- Exercise 14.8
instance Eq Shape where
  Circle r1 _ == Circle r2 _ = r1 <= 0 && r2 <= 0 || r1 == r2
  Rectangle w1 h1 _ == Rectangle w2 h2 _ = w1 >= 0 && h1 >= 0 && w2 >= 0 && h2 >= 0 && w1 == w2 && h1 == h2
  Triangle a1 b1 c1 _ == Triangle a2 b2 c2 _ = a1 == a2 && b1 == b2 && c1 == c2
  _ == _ = False

-- Exercise 14.10
type NewShape = Shape

move :: Float -> Float -> NewShape -> NewShape
move x y shape = case shape of
  Circle radius (cx, cy) -> Circle radius (cx + x, cy + y)
  Rectangle w h (cx, cy) -> Rectangle w h (cx + x, cy + y)
  Triangle a b c (cx, cy) -> Triangle a b c (cx + x, cy + y)

-- Exercise 14.15 and 14.17
data Expr
  = Lit Int
  | Add Expr Expr
  | Sub Expr Expr
  | Mul Expr Expr
  | Div Expr Expr

eval :: Expr -> Int
eval (Lit n) = n
eval (Add e1 e2) = eval e1 + eval e2
eval (Sub e1 e2) = eval e1 - eval e2
eval (Mul e1 e2) = eval e1 * eval e2
eval (Div e1 e2) = eval e1 `div` eval e2

-- Exercise 14.16
size :: Expr -> Int
size (Lit _) = 0
size (Add e1 e2) = 1 + size e1 + size e2
size (Sub e1 e2) = 1 + size e1 + size e2
size (Mul e1 e2) = 1 + size e1 + size e2
size (Div e1 e2) = 1 + size e1 + size e2

-- Exercise 14.18
data Expr2
  = Lit2 Int
  | Op Ops Expr2 Expr2

data Ops = Add2 | Sub2 | Mul2 | Div2

eval2 :: Expr2 -> Int
eval2 e = case e of
  Lit2 n -> n
  Op Add2 e1 e2 -> eval2 e1 + eval2 e2
  Op Sub2 e1 e2 -> eval2 e1 - eval2 e2
  Op Mul2 e1 e2 -> eval2 e1 * eval2 e2
  Op Div2 e1 e2 -> eval2 e1 `div` eval2 e2

size2 :: Expr2 -> Int
size2 e = case e of
  Lit2 _ -> 0
  Op _ e1 e2 -> 1 + size2 e1 + size2 e2

-- Exercise 14.21
data NTree
  = NilT
  | Node Int NTree NTree
  deriving (Show)

-- Example of NTree
myNTree :: NTree
myNTree =
  Node
    5
    ( Node
        3
        (Node 1 NilT NilT)
        (Node 4 NilT NilT)
    )
    ( Node
        8
        (Node 7 NilT NilT)
        (Node 9 NilT NilT)
    )

-- The example tree above looks like this
--     5
--    / \
--   3   8
--  / \ / \
-- 1  4 7  9

left :: NTree -> NTree
left NilT = NilT
left (Node _ l _) = l

right :: NTree -> NTree
right NilT = NilT
right (Node _ _ r) = r

-- Exercise 14.22
eleInTree :: Int -> NTree -> Bool
eleInTree _ NilT = False
eleInTree n (Node x l r) = n == x || eleInTree n l || eleInTree n r

-- Exercise 14.23
maxInTree :: NTree -> Int
maxInTree NilT = minBound
maxInTree (Node x l r) = max x (max (maxInTree l) (maxInTree r))

minInTree :: NTree -> Int
minInTree NilT = maxBound
minInTree (Node x l r) = min x (min (minInTree l) (minInTree r))

-- Exercise 14.24
reflect :: NTree -> NTree
reflect NilT = NilT
reflect (Node x l r) = Node x (reflect r) (reflect l)

-- Exrecise 14.29
data Either2 a b
  = Left2 a
  | Right2 b
  deriving (Eq, Ord, Read, Show)

twist :: Either2 a b -> Either2 b a
twist (Left2 x) = Right2 x
twist (Right2 x) = Left2 x

-- Exercise 14.33
data GTree a
  = Leaf a
  | Gnode [GTree a]
  deriving (Show)

-- Example of a GTree
myGTree :: GTree Int
myGTree =
  Gnode
    [ Leaf 10,
      Gnode
        [ Leaf 20,
          Leaf 30,
          Gnode
            [ Leaf 40,
              Leaf 50
            ]
        ],
      Leaf 60
    ]

-- The example tree above looks like this
--         Gnode
--        /  |  \
--       /   |   \
--      /    |    \
--   Leaf  Gnode  Leaf
--   (10)  / | \  (60)
--        /  |  \
--       /   |   Gnode
--      /    |  /  \
--   Leaf  Leaf Leaf Leaf
--   (20)  (30) (40) (50)

numLeaves :: GTree a -> Int
numLeaves (Leaf _) = 1
numLeaves (Gnode ts) = sum (map numLeaves ts)

depthOfTree :: GTree a -> Int
depthOfTree (Leaf _) = 1
depthOfTree (Gnode ts) = 1 + maximum (map depthOfTree ts)

sumGT :: (Num a) => GTree a -> a
sumGT (Leaf x) = x
sumGT (Gnode ts) = sum (map sumGT ts)

eleInGTree :: (Eq a) => a -> GTree a -> Bool
eleInGTree x (Leaf y) = x == y
eleInGTree x (Gnode ts) = any (eleInGTree x) ts

mapGT :: (a -> b) -> GTree a -> GTree b
mapGT f (Leaf x) = Leaf (f x)
mapGT f (Gnode ts) = Gnode (map (mapGT f) ts)

flatten :: GTree a -> [a]
flatten (Leaf x) = [x]
flatten (Gnode ts) = concatMap flatten ts