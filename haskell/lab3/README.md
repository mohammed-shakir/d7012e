# Notes

## Testing and running

Load necessary files in ghci:

```
:l Program.hs TestProgram.hs TestExpr.hs TestStatement.hs TestParser.hs
```

Run tests:

```
TestParser.runTests
TestExpr.runTests
TestStatement.runTests
TestProgram.runTests
```

## Understanding the lab and the code

The lab is about creating a simple programming language and implementing a parser for it. The language is a simple imperative language with variables, assignments, conditionals, loops, and print statements. The parser is implemented using the Parsec library.
