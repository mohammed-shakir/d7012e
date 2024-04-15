module Parser
  ( module CoreParser,
    T,
    digit,
    digitVal,
    chars,
    letter,
    err,
    lit,
    number,
    iter,
    accept,
    require,
    token,
    spaces,
    word,
    (-#),
    (#-),
  )
where

import CoreParser
import Data.Char
import Prelude hiding (fail, return)

infixl 7 -#, #-

type T a = Parser a

err :: String -> Parser a
err message cs = error (message ++ " near " ++ cs ++ "\n")

iter :: Parser a -> Parser [a]
iter m = m # iter m >-> cons ! return []

cons (a, b) = a : b

-- Assignment 1 (-#)
(-#) :: Parser a -> Parser b -> Parser b
m -# n = m # n >-> snd

-- Assignment 1 (#-)
(#-) :: Parser a -> Parser b -> Parser a
m #- n = m # n >-> fst

-- Assignment 1 spaces
spaces :: Parser String
spaces = iter (char ? isSpace)

token :: Parser a -> Parser a
token m = m #- spaces

-- Assignment 1 letter
letter :: Parser Char
letter = char ? isAlpha

word :: Parser String
word = token (letter # iter letter >-> cons)

-- Assignment 1 chars
chars :: Int -> Parser String
chars n = char # chars (n - 1) >-> cons

accept :: String -> Parser String
accept w = token (chars (length w)) ? (== w)

-- Assignment 1 require
require :: String -> Parser String
require w = accept w ! err ("expecting " ++ w)

lit :: Char -> Parser Char
lit c = token char ? (== c)

digit :: Parser Char
digit = char ? isDigit

digitVal :: Parser Integer
digitVal = digit >-> digitToInt >-> fromIntegral

number' :: Integer -> Parser Integer
number' n =
  digitVal
    #> (\d -> number' (10 * n + d))
    ! return n

number :: Parser Integer
number = token (digitVal #> number')
