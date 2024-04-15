module Program (T, parse, fromString, toString, exec) where

import Dictionary qualified
import Parser hiding (T)
import Statement qualified
import Prelude hiding (fail, return)

newtype T = Program () -- to be defined

instance Parse T where
  parse = error "Program.parse not implemented"
  toString = error "Program.toString not implemented"

exec = error "Program.exec not implemented"
