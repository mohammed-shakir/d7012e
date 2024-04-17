{- Test for Program -}
module TestProgram where

import Distribution.TestSuite (Test)
import Program (T, exec, fromString, toString)

p, p1, p2 :: Program.T
p =
  fromString
    "\
    \read k;\
    \read n;\
    \m := 1;\
    \while n-m do\
    \  begin\
    \    if m - m/k*k then\
    \      skip;\
    \    else\
    \      write m;\
    \    m := m + 1;\
    \  end"
p1 =
  fromString
    "\
    \read n;\
    \read b;\
    \m := 1;\
    \s := 0;\
    \p := 1;\
    \while n do\
    \  begin\
    \    q := n/b;\
    \    r := n - q*b;\
    \    write r;\
    \    s := p*r+s;\
    \    p := p*10;\
    \    n :=q;\
    \  end\
    \write s;"
p2 =
  fromString
    "\
    \count := 0;\
    \repeat\
    \   begin\
    \       count := count + 1;\
    \   end\
    \until 1;\
    \write count;"

sp :: IO ()
sp = putStr (toString p)

rp :: [Integer]
rp = Program.exec p [3, 16]

rp1 :: [Integer]
rp1 = Program.exec p1 [1024, 2]

rp2 :: [Integer]
rp2 = Program.exec p2 []

runTests :: IO ()
runTests = do
  sp
  print rp
  print rp1
  print rp2
