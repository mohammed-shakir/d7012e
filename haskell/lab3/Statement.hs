module Statement (T, parse, toString, fromString, exec) where

import Dictionary qualified
import Expr qualified
import Parser hiding (T)
import Prelude hiding (fail, return)

type T = Statement

-- Assignment 3
data Statement
  = Assignment String Expr.T
  | If Expr.T Statement Statement
  | Skip
  | Begin [Statement]
  | While Expr.T Statement
  | Read String
  | Write Expr.T
  deriving (Show)

-- Assignment
assignment = word #- accept ":=" # Expr.parse #- require ";" >-> buildAss

buildAss (v, e) = Assignment v e

-- If
myIf = accept "if" -# Expr.parse # require "then" -# parse # require "else" -# parse >-> buildIf

buildIf ((e, s1), s2) = If e s1 s2

-- Skip
skip = accept "skip" -# require ";" >-> buildSkip

buildSkip _ = Skip

-- Begin
begin = accept "begin" -# iter parse #- require "end" >-> buildBegin

buildBegin = Begin

-- While
myWhile = accept "while" -# Expr.parse # require "do" -# parse >-> buildWhile

buildWhile (e, s) = While e s

-- Read
readStmt = accept "read" -# word #- require ";" >-> buildRead

buildRead = Read

-- Write
write = accept "write" -# Expr.parse #- require ";" >-> buildWrite

buildWrite = Write

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

instance Parse Statement where
  parse =
    assignment
      ! myIf
      ! skip
      ! begin
      ! myWhile
      ! readStmt
      ! write
  toString = statementToString

-- Assignment 5
statementToString :: Statement -> String
statementToString (Assignment var expr) = var ++ " := " ++ Expr.toString expr ++ ";\n"
statementToString (If cond thenStmts elseStmts) =
  "if " ++ Expr.toString cond ++ " then\n" ++ toString thenStmts ++ "else\n" ++ toString elseStmts
statementToString Skip = "skip;\n"
statementToString (Begin stmts) = "begin\n" ++ concatMap toString stmts ++ "end\n"
statementToString (While cond stmt) =
  "while " ++ Expr.toString cond ++ " do\n" ++ toString stmt ++ "\n"
statementToString (Read var) = "read " ++ var ++ ";\n"
statementToString (Write expr) = "write " ++ Expr.toString expr ++ ";\n"