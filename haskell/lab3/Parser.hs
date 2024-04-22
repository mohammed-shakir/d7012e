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
import Data.Char (Char, digitToInt, isAlpha, isDigit, isSpace)
import Prelude hiding (fail, return)

infixl 7 -#, #-

type T a = Parser a

err :: String -> Parser a
err message cs = error (message ++ " near " ++ cs ++ "\n")

-- It takes a parser m and returns a parser that applies m zero or more times
iter :: Parser a -> Parser [a]
iter m = m # iter m >-> cons ! return []

cons :: (a, [a]) -> [a]
cons (a, b) = a : b

-- Assignment 1
-- Checks if both parsers m and n succeed, it returns the result of the second parser n
(-#) :: Parser a -> Parser b -> Parser b
m -# n = m # n >-> snd

-- Checks if both parsers m and n succeed, it returns the result of the first parser m
(#-) :: Parser a -> Parser b -> Parser a
m #- n = m # n >-> fst

-- It parses zero or more whitespace characters
spaces :: Parser String
spaces = iter (char ? isSpace)

-- Parses using m and then skips any trailing whitespace
token :: Parser a -> Parser a
token m = m #- spaces

-- Parses a single alphabetic character
letter :: Parser Char
letter = char ? isAlpha

-- Parses a sequence of letters as a single word, skipping trailing spaces
word :: Parser String
word = token (letter # iter letter >-> cons)

-- Parses n characters
chars :: Int -> Parser String
chars 0 = return []
chars n = char # chars (n - 1) >-> cons

-- Checks if the next part of the input matches exactly the string w, skipping trailing spaces
accept :: String -> Parser String
accept w = token (chars (length w)) ? (== w)

-- Similar to accept but fails with a specific error message if the string does not match
require :: String -> Parser String
require w = accept w ! err ("expecting " ++ w)

-- Parses a specific character c, skipping any whitespace
lit :: Char -> Parser Char
lit c = token char ? (== c)

-- Parses a single digit character
digit :: Parser Char
digit = char ? isDigit

-- Parses a single digit and converts it to an Integer.
digitVal :: Parser Integer
digitVal = digit >-> digitToInt >-> fromIntegral

number' :: Integer -> Parser Integer
number' n =
  digitVal
    #> (\d -> number' (10 * n + d))
    ! return n

-- Parses an entire number, as an Integer.
number :: Parser Integer
number = token (digitVal #> number')
