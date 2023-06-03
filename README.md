# Micro Haskell
This directory contains an implementation of a very small subset of Haskell.
It uses combinators for the runtime execution.

## Language
The language is a subset of Haskell.  There is no static type checking (yet).

It has the following features:
* variables
* application
* lambda, just variables, not pattern matching
* integer literals
* character literals
* string (list of characters) literals
* case expressions, no nested patterns, must be exhaustive
* let expressions, no mutual recursion, no pattern matching
* tuples
* list syntax
* arithmetic and comparison operators, but only for `Int`
* qualified `do` notation, e.g., `IO.do`
* data type declarations
* type signatures that are ignored
* importing of other modules, `qualified` and `as` supported, but no import list
* exporting with mandatory export list, only module exports allowed

## Example
The file `Example.hs` contains the following:
```Haskell
module Example(module Example) where
import Prelude

fac n =
  case n <= 0 of
    True  -> 1
    False -> n * fac(n-1)

main = do
  let
    rs = map fac [1,2,3,10]
  putStrLn "Some factorials"
  putStrLn $ showList showInt rs
```

First, make sure the binaries are built.  E.g., by doing `make test`.
Then compile the file by `bin/uhs -ilib Example` which produces `out.comb`.
Finally, run the combinator file by `bin/eval out.comb`.
This should produce
```
Some factorials
[1,2,6,3628800]
```

## Libraries
There are a number of libraries that some of the standard Haskell functions.
But in general, the `Prelude` contains less.

## Compiler
The compiler is written in Haskell (not Micro Haskell yet).
It takes a name of a module and compiles it to a file called `out.comb`.

### Compiler modules

* `Main`, the main module.  Decodes flags, compiles, and writes result.
* `Compile`, top level compiler.  Maintains a cache of already compiled modules.
* `Exp`, simple expression type, combinator abstraction and optimization
* `Desugar`, desugar full expressions to simple expressions.
* `Parse`, parse and build and abstract syntax tree.
* `ParserComb`, simple parser combinator library.

## Runtime
The runtime system is written in C and is in `eval.c`.
It uses combinators for handling variables, and has some primitive operations
for integers and for executing IO operations.
There is a also a simple mark-scan garbage collector.

### Features
The runtime system can serialize and deserialize any expression
and keep its graph structure (sharing and cycles).
The only exception to this is file handles, which cannot be serialized.
