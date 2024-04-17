{- Test for Parser.hs -}
module TestParser where

import Parser
  ( accept,
    chars,
    letter,
    require,
    spaces,
    word,
    (-#),
  )
import Prelude hiding (fail, return)

l1 :: Maybe (Char, String)
l1 = letter "abc" {- Just('a',"bc") -}

l2 :: Maybe (Char, String)
l2 = letter "123" {- Nothing -}

l3 :: Maybe (Char, String)
l3 = letter "" {- Nothing -}

w1 :: Maybe (String, String)
w1 = spaces "abc" {- Just("","abc") -}

w2 :: Maybe (String, String)
w2 = spaces "  \t abc" {- Just("  \t ","abc") -}

c1 :: Maybe (String, String)
c1 = chars 2 "abc" {-  Just ("ab","c")  -}

c2 :: Maybe (String, String)
c2 = chars 0 "ab" {-  Just ("","ab")  -}

c3 :: Maybe (String, String)
c3 = chars 3 "ab" {-  Nothing)  -}

r1 :: Maybe (String, String)
r1 = require ":=" ":= 1" {- Just (":=","1") -}

r2 :: Maybe (String, String)
r2 = require "else" "then" {- Program error: expecting else near then -}

a4 :: Maybe (String, String)
a4 = (accept "read" -# word) "read count" {-  Just ("count","") -}
