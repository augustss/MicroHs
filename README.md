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
* terrible, terrible error messages

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
But in general, the `Prelude` contains much less.

## Compiler
The compiler is written in Micro Haskell.
It takes a name of a module and compiles it to a file called `out.comb`.

### Compiler flags
* `-iDIR` add `DIR` to search path for modules
* `-oFILE` output combinators to `FILE` instead of `out.comb`
* `-r` run directly, does work if compiled with GHC
* `-v` be more verbose, flag can be repeated

With the `-v` flag the processing time for each module is reported.
E.g.
  ```
  importing done MicroHs.Exp, 716ms (368 + 348)
  ```
which means tha processing `MicroHs.Exp.hs` took 716ms,
with parsing taking 368ms and typecheck&desugar taking 348ms.

### Compiler modules

* `Main`, the main module.  Decodes flags, compiles, and writes result.
* `Compile`, top level compiler.  Maintains a cache of already compiled modules.
* `Exp`, simple expression type, combinator abstraction and optimization
* `Desugar`, desugar full expressions to simple expressions.
* `Parse`, parse and build and abstract syntax tree.
* `Translate`, convert an expression tree to its value.
* `TypeCheck`, type checker

## Runtime
The runtime system is written in C and is in `eval.c`.
It uses combinators for handling variables, and has some primitive operations
for integers and for executing IO operations.
There is a also a simple mark-scan garbage collector.
It is written in a reasonably portable C code.

### Runtime flags
* `-HSIZE` set heap size to `SIZE` cells, can be suffixed by `k`, `M`, or `G`, default is `100k`
* `-KSIZE` set stack size to `SIZE` entries, can be suffixed by `k`, `M`, or `G`, default is`10k`
* `-rFILE` read combinators from `FILE`, instead of `out.comb`
* `-v` be more verbose, flag can be repeated
* `--` end of flags, the rest of the arguments are available to the running program

### Features
The runtime system can serialize and deserialize any expression
and keep its graph structure (sharing and cycles).
The only exception to this is file handles, which cannot be serialized.

## Bootstrapping
It is possible to recompile the compiler without access to a Haskell compiler.
The combinator file for the compiler itself is available in `comb/mhs.comb`.
The bootstrapping process takes about 30s.
To bootstrap:
 * build the evaluator, `make bin/eval`, this requires a C compiler
 * compile the compiler
   ```
   bin/eval -H50M -rcomb/mhs.comb -- -ilib -isrc -onewmhs.comb MicroHs.Main
   ```
 * The file `newmhs.comb` is the new combinator binary and it should be
   identical to `comb/mhs.comb`.

**NOTE** The GC mark phase currently uses a ridiculously deep stack.
You might have to increase it on your system.

# FAQ
* 
  * Q: When will it get _insert feature_?
  * A: Maybe some time, maybe never.
*
  * Q: Why are the error messages so bad?
  * A: Error messages are boring.  But I plan to add location information to them.
*
  * Q: Why is the so much source code?
  * A: I wonder this myself.  Over 5000 lines of Haskell seems excessive.
       1500 lines of C is also more than I'd like for such a simple system.
*
  * Q: Why are the binaries so big?
  * A: The combinator file is rather verbose.  Compressed the combinator file
       for the compiler shrinks from 150kB to 20kB.  The evaluator is 44kB so
       the total size for runtime and (compressed) compiler is about 65k.
       I'm sorry if you're running on a 16 bit system.
