module Program (T, parse, fromString, toString, exec) where

import CoreParser qualified
import Dictionary qualified
import Parser (Parse (..), iter, (>->))
import Statement qualified
import Prelude hiding (fail, return)

-- Assignment 4 and 5
newtype T = Program [Statement.T]

instance Parse T where
  parse :: CoreParser.Parser T
  parse = iter Statement.parse >-> Program
  toString :: T -> String
  toString (Program stmts) = concatMap Statement.toString stmts

exec :: T -> [Integer] -> [Integer]
exec (Program stmts) = Statement.exec stmts Dictionary.empty