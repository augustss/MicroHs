# Micro Haskell
This directory contains an implementation of a small subset of Haskell.
It uses combinators for the runtime execution.

The compiler can compile itself.

## Compiling MicroHs
There are three different ways to compile MicroHs
* Using GHC with standard `Prelude` and libraries. `Makefile` target `bin/mhs`
* Using GHC, but with `Prelude` and libraries from MicroHs. `Makefile` target `bin/bootmhs`
* Using mhs, with the supplied `comb/mhs.comb`. `Makefile` target `comb/mhs-new.comb`

These different ways of compiling need slightly different imports etc.
To accomodate this each source file is preprocessed for the first two targets.
When compiling with GHC and standard libraries the strings `--X` and `--W` are removed from the source file.
When compiling with GHC and MicroHs libraries the strings `--Y` and `--W` are removed from the source file.
This way anything special things needed with GHC is just treated as comments by mhs.

Compiling MicroHs is really best done using `make`, but there is also a `MicroHs.cabal` file
for use with `cabal`.  This only builds what corresponds to the first target.

Also note that there is no need to have a Haskell compiler to run MicroHs.
All you need is a C compiler, and MicroHs can bootstrap, given the included combinator file (`comb/mhs.comb`).

## Language
The language is a subset of Haskell.  There is only simple Hindley-Milner polymorphism,
no type classes (yet).

It has the following features:
* variables
* application
* lambda
* integer literals
* double literals (no exponents)
* character literals
* string (list of characters) literals
* case expressions
* let expressions
* tuples
* list syntax
* list comprehensions
* arithmetic and comparison operators, the prelude exports the ones for `Int`, but for the other types you need to do a qulified import (e.g for `Double` and for `Word`).
* qualified `do` notation, e.g., `IO.do`
* data (and newtype) type declarations
* type synonyms
* type signatures
* importing of other modules, `qualified` and `as` supported
* exporting with mandatory export list
* the `Prelude` has to be imported explicitely
* mandatory type signatures at the top level, with mandatory `forall` for polymorphism
* terrible error messages, some errors are not even flagged

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
Finally, run the combinator file by `bin/eval`.
This should produce
```
Some factorials
[1,2,6,3628800]
```

## Libraries
There are a number of libraries that have some of the standard Haskell functions.
But in general, the `Prelude` contains much, much less.

## Types
There are some primitive data types, e.g `Int`, `Handle`, and `Double`.  These are known by the runtime system and various primitive operations work on them.  The function type, `->`, is (of course) also built in.

All other types are defined with the language.  They are converted to lambda terms using
the Scott encoding.   The runtime system knows how lists are encoded and booleans are encoded.


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
* `Expr`, parsed expression type.
* `Desugar`, desugar full expressions to simple expressions.
* `Lex`, lexical analysis and indentation processing.
* `Parse`, parse and build and abstract syntax tree.
* `Translate`, convert an expression tree to its value.
* `TypeCheck`, type checker.

## Interactive mode
If no module name is given the compiler enters interactive mode.
You can enter expressions to be evaluated, or top level definitions.
Simple line editing is available.

All definitions is saved in the file `Interactive.hs` and all input
lines as saved in `.mhsi`.  The latter file is read on startup so
the command history is persisted.

Available commands:

* `:quit` Quit the interactive system
* `:clear` Get back to start state
* `:del STR` Delete all definitions that begin with `STR`
* `expr` Evaluate expression. ***NOTE*** Currently only expressions of type `Int` are allowed.
* `defn` Add definition (can also be an `import`)

***NOTE*** When you `import` a module it is cached.
If the file changes and you import it again it will not reload.
You can use `:clear` no get back to an empty cache.
This is a bug.

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
The bootstrapping process takes about 20s (on a modern machine).
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
 * For systems where `upx` works you can further compress
   the binary.  See `bin/umhs` target.

**NOTE** The GC mark phase currently uses a ridiculously deep stack.
You might have to increase it on your system.

# FAQ
* 
  * Q: When will it get _insert feature_?
  * A: Maybe some time, maybe never.
* 
  * Q: Why are the error messages so bad?
  * A: Error messages are boring.
* 
  * Q: Why is the so much source code?
  * A: I wonder this myself.  Over 5000 lines of Haskell seems excessive.
       2000 lines of C is also more than I'd like for such a simple system.
* 
  * Q: Why are the binaries so big?
  * A: The combinator file is rather verbose.  The combinator file
       for the compiler shrinks from 170kB to 30kB when compressed.
       The evaluator is about 60kB.
       The total compressed size for runtime and compiler is about 50k.
       I'm sorry if you're running on a 16 bit system.
