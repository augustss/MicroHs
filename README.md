# Micro Haskell
This directory contains an implementation of a small subset of Haskell.
It uses combinators for the runtime execution.

The compiler can compile itself.

## Compiling MicroHs
There are two different ways to compile MicroHs
* Using GHC with standard `Prelude` and libraries. `Makefile` target `bin/gmhs`
* Using the included combinator file and runtime.  `Makefile` target `bin/mhs`

These different ways of compiling need slightly different imports etc.
To accomodate this each source file is preprocessed for the first target.
When compiling with GHC the string `--X` is removed from the source file.
This way anything special needed with GHC is just treated as comments by mhs.

Compiling MicroHs is really best done using `make`, but there is also a `MicroHs.cabal` file
for use with `cabal`.  This only builds what corresponds to the first target.

Also note that there is no need to have a Haskell compiler to run MicroHs.
All you need is a C compiler, and MicroHs can bootstrap, given the included combinator file.

To install `mhs` use `make install`.  You also need to set the environment variable `MHSDIR`.

## Language
The language is an extended subset of Haskell-98.

Differences:
 * Top level definitions must have a type signature.
 * Type variables need an explicit `forall`.
 * Type variables without a kind annotation are assumed to have kind `Type`.
 * There is no prefix negation.
 * There is no `Read` class.
 * There is no deriving.
 * The `Prelude` has to be imported explicitly.
 * Polymorphic types are never inferred; use a type signature if you need it.
 * A module must have an export list.
 * Always enabled extension:
   * ConstraintKinds
   * EmptyDataDecls
   * ExtendedDefaultRules
   * FlexibleContexts
   * FlexibleInstance
   * ForeignFunctionInterface
   * IncoherentInstances
   * KindSignatures
   * MonoLocalBinds
   * MultiParamTypeClasses
   * NegativeLiterals
   * NoMonomorphismRestriction
   * OverlappingInstances
   * RankNTypes
   * QualifiedDo
   * ScopedTypeVariables
   * StarIsType
   * TupleSections
   * TypeSynonymInstances
   * UndecidableInstances
   * UndecidableSuperClasses
 * In the works
   * FunctionalDependencies
   * ExistentialQuantification
 * `main` in the top module given to `mhs` serves at the program entry point.
 * Many things that should be an error (but which are mostly harmless) are not reported.
 * More differences that I don't remember right now.

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
  print rs
```

First, make sure the compiler is built by doing `make`.
Then compile the file by `bin/mhs Example -oEx` which produces `Ex`.
Finally, run the binary file by `./Ex`.
This should produce
```
Some factorials
[1,2,6,3628800]
```

## Libraries
There are a number of libraries that have some of the standard Haskell functions.
But in general, the `Prelude` contains less.

## Types
There are some primitive data types, e.g `Int`, `IO`, `Ptr`, and `Double`.
These are known by the runtime system and various primitive operations work on them.
The function type, `->`, is (of course) also built in.

All other types are defined with the language.  They are converted to lambda terms using
the Scott encoding.   The runtime system knows how lists and booleans are encoded.


## Compiler
The compiler is written in Micro Haskell.
It takes a name of a module and compiles to a target (see below).
This module should contain the function `main` of type `IO ()` and
it will be the entry point to the program.

### Compiler flags
* `-iDIR` add `DIR` to search path for modules
* `-oFILE` output file.  If the `FILE` ends in `.comb` it will produce a textual combinator file.  If `FILE` ends in `.c` it will produce a C file with the combinators.  For all other `FILE` it will compiler the combinators together with the runtime system to produce a regular executable.
* `-r` run directly (does not work if compiled with GHC)
* `-v` be more verbose, flag can be repeated

With the `-v` flag the processing time for each module is reported.
E.g.
  ```
  importing done MicroHs.Exp, 284ms (91 + 193)
  ```
which means that processing the module `MicroHs.Exp` took 284ms,
with parsing taking 91ms and typecheck&desugar taking 193ms.

### Environment variables
* `MHSDIR` the directory where `lib/` and `src/` are expected to be.  Defaults to `./`.
* `MHSCC` command use to compile C file to produce binaries.  Look at the source for more information.

### Compiler modules

* `Compile`, top level compiler.  Maintains a cache of already compiled modules.
* `Desugar`, desugar full expressions to simple expressions.
* `Exp`, simple expression type, combinator abstraction and optimization.
* `Expr`, parsed expression type.
* `Graph`, strongly connected component algorithm.
* `Ident`, identifiers and related types.
* `IdentMap`, map from identifiers to something.
* `Interactive`, top level for the interactive REPL.
* `Lex`, lexical analysis and indentation processing.
* `Main`, the main module.  Decodes flags, compiles, and writes result.
* `MakeCArray`, generate a C version of the combinator file.
* `Parse`, parse and build and abstract syntax tree.
* `StateIO`, state + IO monad.
* `TCMonad`, type checking monad.
* `Translate`, convert an expression tree to its value.
* `TypeCheck`, type checker.

## Interactive mode
If no module name is given the compiler enters interactive mode.
You can enter expressions to be evaluated, or top level definitions (including `import`).
Simple line editing is available.

All definitions are saved in the file `Interactive.hs` and all input
lines as saved in `.mhsi`.  The latter file is read on startup so
the command history is persisted.

Available commands:

* `:quit` Quit the interactive system
* `:clear` Get back to start state
* `:del STR` Delete all definitions that begin with `STR`
* `:reload` Reload all modules
* `expr` Evaluate expression.
* `defn` Add definition (can also be an `import`)

***NOTE*** When you `import` a module it is cached.
If the file changes and you import it again it will not reload.
You can use `:clear` (or `:reload`) to get back to an empty cache.
This is a bug.

## Runtime
The runtime system is written in C and is in `src/runtime/eval.c`.
It uses combinators for handling variables, and has primitive operations
for built in types and for executing IO operations.
There is a also a simple mark-scan garbage collector.
The runtime system is written in a reasonably portable C code.

### Runtime flags
Runtime flags are given between the flags `+RTS` and `-RTS`.
Between those the runtime decodes the flags, everything else is available to
the running program.

* `-HSIZE` set heap size to `SIZE` cells, can be suffixed by `k`, `M`, or `G`, default is `50M`
* `-KSIZE` set stack size to `SIZE` entries, can be suffixed by `k`, `M`, or `G`, default is`100k`
* `-rFILE` read combinators from `FILE`, instead of `out.comb`
* `-v` be more verbose, flag can be repeated

For example, `bin/mhseval +RTS -H1M -v -RTS hello` runs `out.comb` and the program gets the argument `hello`,
whereas the runtime system sets the heap to 1M cells and is verbose.

### FFI
MicroHs supports calling C functions, but all such functions must be in a table in the runtime system.

### Features
The runtime system can serialize and deserialize any expression
and keep its graph structure (sharing and cycles).
The only exceptions to this are C pointers file handles, which cannot be serialized (except for `stdin`, `stdout`, and `stderr`).

### Memory layout
Memory allocation is based on cells.  Each cell has room for two pointers (i.e., two words, typically 16 bytes),
so it can represent an application node.  One bit is used to indicate if
the cell is an application or something else.  If it is something else one
word is a tag indicating what it is, e.g., a combinator or an integer.
The second word is then used to store any payload, e.g., the number itself for an integer node.

Memory allocation has a bitmap with one bit per cell.
Allocating a cell consists of finding the next free cell using the bitmap,
and then marking it as used.  The garbage collector first clears the bitmap
and then (recursively) marks every used cell in the bitmap.
There is no explicit scan phase since that is baked into the allocation.
Allocation is fast assuming the CPU has some kind of FindFirstSet instruction.

It is possible to use smaller cells by using 32 bit "pointers" instead of 64 bit pointers.
This has a performance penalty, though.

### Portability
The C code for the evaluator does not use any special features, and should be
portable to many platforms.  It has mostly been test with MacOS and Linux,
and somewhat with Windows.

The code has only been tested on 64 bit platforms, so again, there are lurking problems
with other word sizes, but they should be easy to fix.

The `src/runtime/` directory contains configuration files for different platform.
Edit `src/runtime/eval.c` to `#include` the right one.

## Bootstrapping
The compiler can compile itself.  To replace `bin/mhs` with a new version,
do `make bootstrap`.  This will recompile the compiler twice and compare
the outputs to make sure the new compiler still works.


# FAQ
* 
  * Q: When will it get _insert feature_?
  * A: Maybe some time, maybe never.
* 
  * Q: Why are the error messages so bad?
  * A: Error messages are boring.
* 
  * Q: Why is the so much source code?
  * A: I wonder this myself.  7000+ lines of Haskell seems excessive.
       2500+ lines of C is also more than I'd like for such a simple system.
* 
  * Q: Why are the binaries so big?
  * A: The combinator file is rather verbose.  The combinator file
       for the compiler shrinks from 350kB to 75kB when compressed with upx.
       The evaluator alone is about 70kB (26kB compressed with upx).
