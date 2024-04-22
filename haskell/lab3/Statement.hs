module Statement (T, parse, toString, fromString, exec) where

import Dictionary qualified
import Expr qualified
import Parser
  ( Parse (..),
    Parser,
    accept,
    iter,
    require,
    word,
    (!),
    (#),
    (#-),
    (-#),
    (>->),
  )
import Prelude hiding (fail, return)

type T = Statement

-- Assignment 3
-- Assignment 3.a
data Statement
  = Assignment String Expr.T
  | If Expr.T Statement Statement
  | Skip
  | Begin [Statement]
  | While Expr.T Statement
  | Read String
  | Write Expr.T
  | Repeat Statement Expr.T
  deriving (Show)

-- Assignment 3.b
-- Assignment
assignment :: Parser Statement
assignment = word #- accept ":=" # Expr.parse #- require ";" >-> buildAss

buildAss :: (String, Expr.T) -> Statement
buildAss (v, e) = Assignment v e

-- If
myIf :: Parser Statement
myIf = accept "if" -# Expr.parse # require "then" -# parse # require "else" -# parse >-> buildIf

buildIf :: ((Expr.T, Statement), Statement) -> Statement
buildIf ((e, s1), s2) = If e s1 s2

-- Skip
skip :: Parser Statement
skip = accept "skip" -# require ";" >-> buildSkip

buildSkip :: p -> Statement
buildSkip _ = Skip

-- Begin
begin :: Parser Statement
begin = accept "begin" -# iter parse #- require "end" >-> buildBegin

buildBegin :: [Statement] -> Statement
buildBegin = Begin

-- While
myWhile :: Parser Statement
myWhile = accept "while" -# Expr.parse # require "do" -# parse >-> buildWhile

buildWhile :: (Expr.T, Statement) -> Statement
buildWhile (e, s) = While e s

-- Read
readStmt :: Parser Statement
readStmt = accept "read" -# word #- require ";" >-> buildRead

buildRead :: String -> Statement
buildRead = Read

-- Write
write :: Parser Statement
write = accept "write" -# Expr.parse #- require ";" >-> buildWrite

buildWrite :: Expr.T -> Statement
buildWrite = Write

-- Repeat
repeatStmt :: Parser Statement
repeatStmt = accept "repeat" -# parse # require "until" -# Expr.parse #- require ";" >-> buildRepeat

buildRepeat :: (Statement, Expr.T) -> Statement
buildRepeat (s, e) = Repeat s e

-- Assignment 3.d
-- Takes list of statements, dictionary and list of integers as input
exec :: [T] -> Dictionary.T String Integer -> [Integer] -> [Integer]
exec [] _ _ = []
exec (If cond thenStmts elseStmts : stmts) dict input =
  if Expr.value cond dict > 0
    then exec (thenStmts : stmts) dict input
    else exec (elseStmts : stmts) dict input
-- Assignment
exec (Assignment var expr : stmts) dict input =
  exec stmts (Dictionary.insert (var, Expr.value expr dict) dict) input
-- Skip
exec (Skip : stmts) dict input =
  exec stmts dict input
-- Begin
exec (Begin stmts' : stmts) dict input =
  exec (stmts' ++ stmts) dict input
-- While
exec (While cond stmt' : stmts) dict input =
  if Expr.value cond dict > 0
    then exec (stmt' : While cond stmt' : stmts) dict input
    else exec stmts dict input
-- Read
exec (Read var : stmts) dict (input : inputs) =
  exec stmts (Dictionary.insert (var, input) dict) inputs
-- Write
exec (Write expr : stmts) dict input =
  Expr.value expr dict : exec stmts dict input
-- Repeat
exec (Repeat stmt cond : stmts) dict input =
  exec (stmt : If cond Skip (Repeat stmt cond) : stmts) dict input

-- Assignment 3.c
instance Parse Statement where
  parse :: Parser Statement
  parse =
    assignment
      ! myIf
      ! skip
      ! begin
      ! myWhile
      ! readStmt
      ! write
      ! repeatStmt
  toString :: Statement -> String
  toString = statementToString

-- Assignment 5
statementToString :: Statement -> String
-- Assignment
statementToString (Assignment var expr) = var ++ " := " ++ Expr.toString expr ++ ";\n"
-- If
statementToString (If cond thenStmts elseStmts) =
  "if " ++ Expr.toString cond ++ " then\n" ++ toString thenStmts ++ "else\n" ++ toString elseStmts
-- Skip
statementToString Skip = "skip;\n"
-- Begin
statementToString (Begin stmts) = "begin\n" ++ concatMap toString stmts ++ "end\n"
-- While
statementToString (While cond stmt) =
  "while " ++ Expr.toString cond ++ " do\n" ++ toString stmt ++ "\n"
-- Read
statementToString (Read var) = "read " ++ var ++ ";\n"
-- Write
statementToString (Write expr) = "write " ++ Expr.toString expr ++ ";\n"
-- Repeat
statementToString (Repeat stmt cond) =
  "repeat\n" ++ toString stmt ++ "until " ++ Expr.toString cond ++ ";\n"
