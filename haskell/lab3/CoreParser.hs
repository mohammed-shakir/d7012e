module CoreParser
  ( Parser,
    char,
    return,
    fail,
    (#),
    (!),
    (?),
    (#>),
    (>->),
    Parse,
    parse,
    toString,
    fromString,
  )
where

import Prelude hiding (fail, return)

infixl 3 !

infixl 7 ?

infixl 6 #

infixl 5 >->

infixl 4 #>

class Parse a where
  parse :: Parser a
  fromString :: String -> a
  fromString cs =
    case parse cs of
      Just (s, []) -> s
      Just (s, cs) -> error ("garbage '" ++ cs ++ "'")
      Nothing -> error "Nothing"
  toString :: a -> String

-- Function that takes a String and returns a Maybe (a, String)
type Parser a = String -> Maybe (a, String)

-- Parses the first character of a string
char :: Parser Char
char [] = Nothing
char (c : cs) = Just (c, cs)

-- Returns a parser that always returns the value a
return :: a -> Parser a
return a cs = Just (a, cs)

-- Returns a parser that always fails
fail :: Parser a
fail cs = Nothing

-- Tries the first parser m, and if it fails, tries the second parser n
(!) :: Parser a -> Parser a -> Parser a
(m ! n) cs = case m cs of
  Nothing -> n cs
  mcs -> mcs

-- It applies a condition p to the result of parser m
(?) :: Parser a -> (a -> Bool) -> Parser a
(m ? p) cs =
  case m cs of
    Nothing -> Nothing
    Just (r, s) -> if p r then Just (r, s) else Nothing

-- It runs two parsers in sequence
(#) :: Parser a -> Parser b -> Parser (a, b)
(m # n) cs =
  case m cs of
    Nothing -> Nothing
    Just (a, cs') ->
      case n cs' of
        Nothing -> Nothing
        Just (b, cs'') -> Just ((a, b), cs'')

-- Transforms the result of parser m using a function b
(>->) :: Parser a -> (a -> b) -> Parser b
(m >-> b) cs =
  case m cs of
    Just (a, cs') -> Just (b a, cs')
    Nothing -> Nothing

-- It applies a function k to the result of parser p
(#>) :: Parser a -> (a -> Parser b) -> Parser b
(p #> k) cs =
  case p cs of
    Nothing -> Nothing
    Just (a, cs') -> k a cs'
