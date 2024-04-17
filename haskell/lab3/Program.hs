module Program (T, parse, fromString, toString, exec) where

import Dictionary qualified
import Parser hiding (T)
import Statement qualified
import Prelude hiding (fail, return)

-- Assignment 4 and 5
newtype T = Program [Statement.T]

instance Parse T where
  parse = iter Statement.parse >-> Program
  toString (Program stmts) = concatMap Statement.toString stmts
  fromString cs = case parse cs of
    Just (program, []) -> program
    Just (_, remaining) -> error ("Remains: " ++ remaining)
    Nothing -> error "Nothing"

exec :: T -> [Integer] -> [Integer]
exec (Program stmts) = Statement.exec stmts Dictionary.empty