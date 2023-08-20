# Micro Haskell
This directory contains an implementation of a very small subset of Haskell.
It uses combinators for the runtime execution.

The compiler can compile itself.

## Language
The language is a subset of Haskell.  There is only simple Hindley-Milner overloading,
no type classes.

It has the following features:
* variables
* application
* lambda
* integer literals
* character literals
* string (list of characters) literals
* case expressions
* let expressions, no mutual recursion
* tuples
* list syntax
* list comprehensions
* arithmetic and comparison operators, but only for `Int`
* qualified `do` notation, e.g., `IO.do`
* data type declarations
* type synonyms
* type signatures
* importing of other modules, `qualified` and `as` supported, but no import list
* exporting with mandatory export list
* the `Prelude` has to be imported explicitely

## Example
The file `Example.hs` contains the following:
```Haskell
module Example(module Example) where
import Prelude

fac :: Int -> Int
fac 0 = 1
fac n = n * fac(n-1)

main :: IO ()
main = do
  let
    rs = map fac [1,2,3,10]
  putStrLn "Some factorials"
  putStrLn $ showList showInt rs
```

First, make sure the binaries are built.  E.g., by doing `make test`.
Then compile the file by `bin/mhs -ilib Example` which produces `out.comb`.
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
The compiler is written in Micro Haskell.
It takes a name of a module and compiles it to a file called `out.comb`.

### Compiler flags
* `-iDIR` add `DIR` to search path for modules
* `-oFILE` output combinators to `FILE` instead of `out.comb`
* `-r` run directly, does work if compiled with GHC
* `-v` be more verbose, flag can be repeated

### Compiler modules

* `Main`, the main module.  Decodes flags, compiles, and writes result.
* `Compile`, top level compiler.  Maintains a cache of already compiled modules.
* `Exp`, simple expression type, combinator abstraction and optimization
* `Desugar`, desugar full expressions to simple expressions.
* `Parse`, parse and build and abstract syntax tree.
* `ParserComb`, simple parser combinator library.
* `Translate`, convert an expression tree to its value.
* `TypeCheck`, type checker

## Runtime
The runtime system is written in C and is in `eval.c`.
It uses combinators for handling variables, and has some primitive operations
for integers and for executing IO operations.
There is a also a simple mark-scan garbage collector.

### Runtime flags
* `-HSIZE` set heap size to `SIZE` cells, can be suffixed by `k`, `M`, or `G`
* `-KSIZE` set stack size to `SIZE` entries, can be suffixed by `k`, `M`, or `G`
* `-rFILE` read combinators from `FILE`
* `-v` be more verbose, flag can be repeated.
* `--` end of flags, the rest of the arguments are available to the running program

### Features
The runtime system can serialize and deserialize any expression
and keep its graph structure (sharing and cycles).
The only exception to this is file handles, which cannot be serialized.

## Bootstrapping
It is possible to recompile the compiler without access to a Haskell compiler.
The combinator file for the compiler itself is available in `comb/mhs.comb`.
To bootstrap:
 * build the evaluator `make bin/eval`, this requires a C compiler
 * compiler the compiler
```
bin/eval -H50M -rcomb/mhs.comb -- -ilib -isrc -onewmhs.comb MicroHs.Main
```
