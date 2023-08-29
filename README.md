# Micro Haskell
This directory contains an implementation of a small subset of Haskell.
It uses combinators for the runtime execution.

The compiler can compile itself.

## Language
The language is a subset of Haskell.  There is only simple Hindley-Milner polymorphism,
no type classes (yet).

It has the following features:
* variables
* application
* lambda
* integer literals
* character literals
* string (list of characters) literals
* case expressions
* let expressions, no mutual recursion (yet)
* tuples
* list syntax
* list comprehensions
* arithmetic and comparison operators, but only for `Int`
* qualified `do` notation, e.g., `IO.do`
* data (and newtype) type declarations
* type synonyms
* type signatures
* importing of other modules, `qualified` and `as` supported, but no import list
* exporting with mandatory export list
* the `Prelude` has to be imported explicitely
* terrible, terrible error messages

## Example
The file `Example.hs` contains the following:
```Haskell
module Example(main) where
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
Finally, run the combinator file by `bin/eval +RTS -rout.comb`.
This should produce
```
Some factorials
[1,2,6,3628800]
```

## Libraries
There are a number of libraries that have some of the standard Haskell functions.
But in general, the `Prelude` contains much, much less.

## Compiler
The compiler is written in Micro Haskell.
It takes a name of a module and compiles it to a file called `out.comb`.

### Compiler flags
* `-iDIR` add `DIR` to search path for modules
* `-oFILE` output combinators to `FILE` instead of `out.comb`
* `-r` run directly, does not work if compiled with GHC
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
* `Exp`, simple expression type, combinator abstraction and optimization.
* `Desugar`, desugar full expressions to simple expressions.
* `Lex`, lexical analysis and indentation processing.
* `Parse`, parse and build and abstract syntax tree.
* `Translate`, convert an expression tree to its value.
* `TypeCheck`, type checker.

## Runtime
The runtime system is written in C and is in `eval.c`.
It uses combinators for handling variables, and has primitive operations
for integers and for executing IO operations.
There is a also a simple mark-scan garbage collector.
It is written in a reasonably portable C code.

### Runtime flags
Runtime flags are given between the flags `+RTS` and `-RTS`.
Between those the runtime decodes the flags, everything else is available to
the running program.

* `-HSIZE` set heap size to `SIZE` cells, can be suffixed by `k`, `M`, or `G`, default is `50M`
* `-KSIZE` set stack size to `SIZE` entries, can be suffixed by `k`, `M`, or `G`, default is`100k`
* `-rFILE` read combinators from `FILE`, instead of `out.comb`
* `-v` be more verbose, flag can be repeated

For example, `bin/eval +RTS -H1M -v -RTS hello` runs `out.comb` and the program gets the argument `hello`,
whereas the runtime system sets the heap to 1M cells and is verbose.


### Features
The runtime system can serialize and deserialize any expression
and keep its graph structure (sharing and cycles).
The only exception to this is file handles, which cannot be serialized (except for `stdin`, `stdout`, and `stderr`).

### Memory layout
Memory allocation is based on cells.  Each cell has room for two pointers (i.e., two words, i.e., 16 bytes),
so it can represent an application node.  One bit is used to indicate if
the cell has an application or something else.  If it is something else one
word is a tag indicating what it is, e.g., a combinator or an integer.
The second word is then used to store any payload, e.g., the number itself for an integer node.

Memory allocation has a bitmap with one bit per cell.
Allocating a cell consists of finding the next free cell using the bitmap,
and then marking it as used.  The garbage collector first clears the bitmap
and then (recursively) marks every used cell in the bitmap.
There is no explicit scan phase since that is baked into the allocation.

It is possible to use smaller cells by using 32 bit "pointers" instead of 64 bit pointers.
This has a performance penalty, though.

### Portability
The C code for the evaluator does not use any special features, and should be
portable to many platforms.  It has mostly been test with MacOS and Linus,
so there are undoubtedly problems on Windows.

The code has only been tested on 64 bit platforms, so again, there are lurking problems
with other word sizes.

## Bootstrapping
It is possible to recompile the compiler without access to a Haskell compiler.
The combinator file for the compiler itself is available in `comb/mhs.comb`.
The bootstrapping process takes about 15s (on a modern machine).
To bootstrap:
 * build the evaluator, `make bin/eval`, this requires a C compiler
 * compile the compiler
   ```
   bin/eval +RTS -rcomb/mhs.comb -RTS -ilib -isrc -onewmhs.comb MicroHs.Main
   ```
 * The file `newmhs.comb` is the new combinator binary and it should be
   identical to `comb/mhs.comb`.
 * It is also possible to bake the combinator code into the binary.
   See `make` target `bin/cmhs` for how it is done.
 * For systems where `upx` works you can compress further compress
   the binary.  See `bin/umhs` target.

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
  * A: I wonder this myself.  Over 5500 lines of Haskell seems excessive.
       1600 lines of C is also more than I'd like for such a simple system.
*
  * Q: Why are the binaries so big?
  * A: The combinator file is rather verbose.  Compressed the combinator file
       for the compiler shrinks from 150kB to 20kB.  The evaluator is about 40kB so
       the total size for runtime and (compressed) compiler is about 40k.
       I'm sorry if you're running on a 16 bit system.
