# Micro Haskell
This repository contains an implementation of an extended subset of Haskell.
It uses combinators for the runtime execution.

The runtime system has minimal dependencies, and can be compiled even for micro-controllers.
The `boards/` directory contains some samples, e.g., some sample code for an STM32F407G-DISC1 board.

The compiler can compile itself.

## Presentation
You can find my [presentation from the Haskell Symposium 2024](https://docs.google.com/presentation/d/1WsSiSwypNVTm0oZ3spRYF8gA59wyteOsIkPRjCUAZec/edit?usp=sharing), [video](https://m.youtube.com/watch?v=uMurx1a6Zck&t=36m).
There is also a short paper at [`doc/hs2024.pdf`](https://github.com/augustss/MicroHs/blob/master/doc/hs2024.pdf).

## Compiling MicroHs
There are two different ways to compile MicroHs:
* Using GHC. `Makefile` target `bin/gmhs`
* Using the included combinator file and runtime.  `Makefile` target `bin/mhs`

These different ways of compiling need slightly different imports etc.
This happens by GHC looking in the `ghc/` subdirectory first for any extras/overrides.

Compiling MicroHs is really best done using `make`, but there is also a `MicroHs.cabal` file
for use with `cabal`/`mcabal`.  This only builds what corresponds to the first target.
Doing `cabal install` will install the compiler.
Note that `mhs` built with ghc does not have all the functionality.

Also note that there is no need to have a Haskell compiler to run MicroHs.
All you need is a C compiler, and MicroHs can bootstrap, given the included combinator file.

To install `mhs` use `make minstall`.  This will install `mhs` in `~/.mcabal` in the same
way as `mcabal` (MicroCabal) would have.  It will install a compiler binary and a compiled base package.
You will have to add `~/.mcabal/bin` to your `PATH`.

Alternatively, to install `mhs` use `make oldinstall`.  By default this copies the files to `/usr/local`,
but this can be overridden by `make PREFIX=dir oldinstall`.
You also need to set the environment variable `MHSDIR`.

To compile on Windows make sure `cl` is in the path, and then use `nmake` with `Makefile.windows`.

The compiler can also be used with emscripten to produce JavaScript/WASM.

### Using GMP for `Integer`
The default implementation of the `Integer` type is written in Haskell and is quite slow.
It is possible to use the [GMP](https://gmplib.org/) library instead.
To use GMP you need to uncomment the first few lines in the `Makefile`, and also
modify the definition that directs the C compiler where to find GMP.

***NOTE*** To switch between using and not using GMP you need to do `make clean`.
You might also need to do `make USECPPHS=cpphs bootstrapcpphs` if there are complaints.

## Example
The file `Example.hs` contains the following:
```Haskell
module Example(main) where

fac :: Int -> Int
fac 0 = 1
fac n = n * fac(n-1)

main :: IO ()
main = do
  let rs = map fac [1,2,3,10]
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
The `Prelude` contains the functions from the Haskell Report and a few extensions,
with the notable exception that `Foldable` and `Traversable` are not part of the `Prelude`.
They can be imported separately, though.

### Not importing `Prelude`
To completely avoid importing the prelude (which can be needed in base to
avoid circular imports) use the incantation `import qualified Prelude()`.
This will not even try to open `Prelude.hs`.  It also avoids the automagic
(qualified) import of `Mhs.Builtin` that normally happens.


## Interactive mode
If no module name is given the compiler enters interactive mode.
You can enter expressions to be evaluated, or top level definitions (including `import`).
Simple line editing is available.

## MHS as a cross compiler
When `mhs` is built, targets.conf is generated. It will look something like this:
```ini
[default]
cc = "cc"
conf = "unix-64"
```

You can add other targets to this file, changing which compiler command is used and which runtime is
selected and then use the `-t` argument to select which target you would like.

## Files
There is a number of subdirectories:
* `Tools/` a few useful tools for compressions etc.
* `bin/` executables are put here
* `generated/` this contains the (machine generated) combinator file for the compiler.
* `lib/` this contains the `Prelude` and other base library file.
* `src/MicroHs/` the compiler source
* `src/runtime/` the runtime source
* `tests/` some tests

# Preprocessor
Sadly, compiling a lot of Haskell packages needs the C preprocessor.
To this end, the distribution contains the combinator code for `cpphs`.
Doing `make bin/cpphs` will create the binary for the preprocessor.

To bootstrap `cpphs` you can do `make bootstrapcpphs`.
This assumes that you have `git` to download the needed packages.
At the moment, the downloaded packages are forks of the original to
make it compile with `mhs`.

To identify that it is MicroHs that is the compiler it defines the symbol `__MHS__`.

# Contributing
Contributions are very welcome!

When modifying the compiler, run `make newmhs` or `make newmhsz` (the latter compresses the binary)
to generate a compiler that includes your changes.

## Libraries

The libraries live in the `lib/` directory. Adding missing functions/instances/types from the report is a welcome contribution.
Common things from `base` and GHC boot libraries that use a lot of GHC-specific code (such as `array`, `bytestring`, `text`, ...) can also be added.

## Tests

The test suite is located in the `tests/` directory.

To add a new test, create a `MyTest.hs` file and a corresponding `MyTest.ref` file for the expected output.
Then add it to the `test` rule in `tests/Makefile`:
```makefile
	$(TMHS) MyTest     && $(EVAL) > MyTest.out     && diff MyTest.ref MyTest.out
```

If you want to test that a module fails to compile with a certain error message,
add it to `tests/errmsg.test`, for example:
```
module E() where
x :: Int
x = y
-----
"../tmp/E.hs": line 4, col 5: undefined value: y

=====
```

To run the test suite, do
* `make runtest` to use the GHC-compiled compiler
* `make runtestmhs` to use the MicroHs-compiled compiler
* `make runtestemscripten` to use the MicroHs-compiled compiler targeting JavaScript

## Primitives

If you want to add a new primitive, you need to modify a few things:
* in `src/runtime/eval.c`
  - add a variant to `enum node_tag`
  - add an entry to the `primops` table (mapping the name of your primitive to the tag)
  - add a `case` in `printrec`
  - implement the primitive by adding a `case` in `evali` (you can use the other primitives as a guide)
* in `src/MicroHs/Translate.hs`
  - add an entry to the `primTable`, to make the primitive available in the interactive mode
* in `ghc/PrimTable.hs`
  - add an entry to `primOps`, if the primitive should be available in the GHC-compiled interactive mode
* in `hugs/PrimTable.hs`
  - add an entry to `primOps`, if the primitive should be available in the Hugs-compiled interactive mode

Then you can use the primitive via the `_primitive` keyword (`_primitive "myPrimitive"`).

# Thank You
A big thanks goes to the people who have contributed to MicroHs:
@amigalemming
@arossato
@benclifford
@claudeha
@dmjio
gay@disroot.org
@gergoerdi
@ikervagyok
@jmaessen
@juhp
@kolmodin
@konsumlamm
@liolin
@lortabac
@MaximilianAlgehed
@meooow25
@philderbeast
@rewbert
@sol
@Superstar64
@thielema
@thimc
@thma
@treeowl
@TristanCacqueray
@tvmaaren
@yobson


# FAQ
*
  * Q: When will it get _insert feature_?
  * A: Maybe some time, maybe never.  But it doesn't hurt to ask for it.
*
  * Q: Why are the error messages so bad?
  * A: Error messages are boring.

---

For more information, see the [wiki](https://github.com/augustss/MicroHs/wiki).

---

# Finding modules *NOT YET IMPLEMENTED*
A module `M` is located using the _sourcePath_ and the _packageDbPath_.
Both of the paths are a list of directories and are searched left-to-right.
This lookup is used both `import` and modules given on the command line.

Note: If the module name given on the command line has suffix `.hs` or `.lhs`
it is used as a file name, rather than a module name.

First, the the _sourcePath_ is used.  For each directory in the
path:
 * the module name `M` is appended with `.` changed to `/`.
 * then the suffixes `.hs`, `.hsc`, and `.lhs` are tried in order
 * if the file exists it is assumed to be the source for module `M`

Second, if no source file is found, then each directory in the
_packageDbPath_ is tried.  If the module is found, the corresponing
package is loaded and the compiled module 'M' from that package is used.

## Setting the _sourcePath_
The default _sourcePath_ is just the current directory, `.`.
The path can be cleared by using the `-i` command line flag.
To append to the path use `-iPATH`, where `PATH` is a colon (`:`)
separated list of directories.

## Setting the _packageDbPath_
The default _packageDbPath_ is take from the `mhs.config` file
in the section `[mhs]` and key `packageDbPath`.
If this does not exists the defaults path is empty.

In case this does not exists the path defaults to `~/.mcabal/mhs-VERSION`,
where `~` will be replaced by the home directory and `VERSION` is the
compiler version (as displayed by `mhs --numeric-version`).

The directories in _packageDbPath_ can start with a '%', and in this
case the value of the key `packageDbPrefix` is prepended to the path.

The path can be cleared by using the `-a` command line flag.
To append to the path use `-aPATH`, where `PATH` is a colon (':')
separated list if directories.

## Installing packages
The default _packageDb_ for installing packages is given by the
key `packageDbDefaultInstall`.
If absent, it defaults to `~/.mcabal/mhs-VERSION`.

The _packageDb_ directory can also be given directly with the `-Q`
flag used to install.

The `mcabal` command can install to a particular package db by using
`--install=PATH`.

## Examples
### Status Quo
All packages go to the same place.

Compile a module with `mhs Foo`.


mhs.config:
```
[mhs]
packageDbPath=~/.mcabal/mhs-0.15.5.0
packageDbDefaultInstall=~/.mcabal/mhs-0.15.5.0
...
```

### GhcUp
Separate system and user package dbs.

Compile a module with `mhs Foo`.

mhs.config:
```
[mhs]
packageDbPath=~/.mcabal/system:~/.mcabal-user
packageDbDefaultInstall=~/.mcabal-user
...
```

### Nix
A system package db and then a separate db for each package.

Compile a module with `mhs -a%transformers:%mtl Foo`.

```
[mhs]
packageDbPath=~/.mcabal/system
packageDbDefaultInstall=NO_INSTALL_GIVEN
packageDbPrefix=~/.mcabal-user-dbs
...
```
