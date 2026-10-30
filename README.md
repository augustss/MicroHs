# Micro Haskell
The [MicroHs repository](https://github.com/augustss/MicroHs) contains an implementation of an extended subset of Haskell.
It uses combinators for the runtime execution.

[Try it in your browser](https://augustss.github.io/MicroHs/web-mhs/)

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
module Example(fac, main) where

fac :: Integer -> Integer
fac 0 = 1
fac n = n * fac(n - 1)

main :: IO ()
main = do
  let rs = map fac [1,2,3,10,52]
  putStrLn "Some factorials"
  print rs
```

First, make sure the compiler is built by doing `make`.
Then compile the file by `bin/mhs Example -oEx` which produces `Ex`.
Finally, run the binary file by `./Ex`.
This should produce
```
Some factorials
[1,2,6,3628800,80658175170943878571660636856403766975289505440883277824000000000000]
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
When `mhs` is built, `mhs.conf` is generated with a number of predefined targets.
It will look something like this:
```ini
[unix]
cc = "cc"
ccflags = "-w -Wall -O3 "
cclibs = " -lm"
conf = "unix"
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

## JavaScript FFI
With the emscripten targets (`-temscripten`, `-tbrowser`, ...) Haskell code can call JavaScript,
hold JavaScript values, and be called back from JavaScript.
The module `GHC.Wasm.Prim` follows GHC's WebAssembly backend, so code written for it needs few changes.
See `tests/JSVal.hs` and `tests/JSCallback.hs` for examples.

### Calling JavaScript
```Haskell
foreign import javascript [unsafe|safe|interruptible] "code" name :: type
```
The string is JavaScript code, with the arguments as `$1`, `$2`, etc.
It can be an expression, or statements that use `return`:
```Haskell
foreign import javascript "$1 + $2"                            add     :: Int -> Int -> Int
foreign import javascript "return document.getElementById($1)" getElem :: JSString -> IO JSVal
```
* The code becomes an emscripten `EM_JS` function in the output, so it is ordinary JavaScript
  that emscripten's optimizer sees; nothing is compiled at run time.
* It is statements if, outside brackets, strings and comments, it contains a `;` or the word `return`,
  or starts with a statement keyword (`if`, `let`, `throw`, ...); otherwise it is an expression.
* A `$k` that is not an argument is a compile time error; the arguments start at `$1`, not `$0`.
* The code can use the emscripten `Module` object (e.g., `Module.UTF8ToString($1)`) and `mhsjs`,
  the table of `JSVal`s.

The allowed argument and result types are `Int`, `Word`, `Char`, `Bool`, `Double`, `Float`, `Ptr`, `JSVal`,
newtypes of these (e.g., `JSString`), and `()` as a result.
There is no automatic conversion of `String`, `Text`, lists, or 64 bit integers;
strings go through `toJSString`/`textToJSString` (see below).

The safety annotation selects what happens on a JavaScript exception, and whether the code can wait:

| annotation | the code is | a JavaScript exception |
|---|---|---|
| `unsafe` (the default) | a function | is fatal |
| `safe` | a function in a `try`/`catch` | is raised as a `JSException` |
| `interruptible` | an `async` function, so it can use `await` | is raised as a `JSException`; so is a rejected Promise |

With `interruptible` the calling Haskell thread waits for the Promise while the other threads keep running.
Note that, unlike the Haskell report, an import without a safety annotation is `unsafe`
(a JavaScript exception in it is a programming error, as with GHC's JavaScript backend).

### JavaScript values
A `JSVal` is a reference to a JavaScript value, which is kept in a table on the JavaScript side
until the `JSVal` is garbage collected, or `freeJSVal` is called.
`GHC.Wasm.Prim` exports:
```Haskell
JSVal, JSString(..), JSException(..)
toJSString, fromJSString            -- String
textToJSString, textFromJSString    -- Text
freeJSVal
jsNull, jsUndefined, isNull, isUndefined
syncCallback,  syncCallback1,  syncCallback2,  syncCallback3     -- IO ()
syncCallback', syncCallback1', syncCallback2', syncCallback3'    -- IO JSVal, the callback returns a value
asyncCallback, asyncCallback1, asyncCallback2, asyncCallback3
```
The string conversions go through UTF-8 with explicit lengths, so a NUL character and a leading U+FEFF survive.
`JSString` has no `Eq`/`Show` instances, like GHC's.

### Callbacks: calling Haskell from JavaScript
A Haskell function of type `JSVal -> ... -> IO ()` (or `IO JSVal`) can be turned into a JavaScript function
with `syncCallback`/`asyncCallback` above, or with a `"wrapper"` import:
```Haskell
foreign import javascript "wrapper sync" mkHandler :: (JSVal -> IO ()) -> IO JSVal
```
* `"wrapper sync"` makes a JavaScript function that runs the Haskell code when called and returns when it is done.
* `"wrapper"` makes one that runs the Haskell code later, from the event loop, and returns a `Promise`.

A callback runs as a Haskell thread of its own (so `catch`, `forkIO`, `putMVar`, etc. work),
first and without preemption.
* Called from the JavaScript event loop it may block (e.g., in `takeMVar` or an `interruptible` import):
  it then waits like any other thread, and its JavaScript caller gets `undefined`
  (or, for `"wrapper"`, an already resolved Promise).
* A callback that returns a value, one called from the JavaScript code of an import (a Haskell thread
  is then on the C stack), and a `foreign export` called from JavaScript must not block; that is a fatal error.
* An uncaught Haskell exception in a callback is thrown to the JavaScript caller as an `Error`
  (an asynchronous callback rejects its Promise).
* A callback is released when its `JSVal` is garbage collected or freed with `freeJSVal`.

### The program and the event loop
Haskell threads keep their state in the heap, so no C stack has to be preserved and emscripten's ASYNCIFY is not needed.
* Whenever all Haskell threads are blocked (`main` included) the runtime returns to the JavaScript event loop;
  a callback, a timer (for `threadDelay`), or a settling Promise resumes the threads.
* While a program has live callbacks the runtime stays alive after `main` returns, so JavaScript events can
  still call them. The program ends when `main` has finished and no callback is left.
* A fatal runtime error, or a JavaScript exception from an `unsafe` import inside a callback, stops the runtime.
  In the browser the page lives on, so every later call into Haskell throws an `Error` saying that the runtime
  has stopped, instead of running on a corrupted runtime.

### Targets
| target | output | notes |
|---|---|---|
| `emscripten` | WebAssembly + JavaScript, for node | |
| `emscripten_js` | JavaScript only (`-sWASM=0`), for node | |
| `browser` | WebAssembly + JavaScript for the browser | no file system; `-Oz -sSUPPORT_LONGJMP=wasm`; 4M cell heap |
| `browser_rts` | as `browser`, but the program is fetched at run time | see below |
| `browser_js` | JavaScript only for the browser | `-O3`; 4M cell heap |

The browser targets use a 4M cell heap (about 60MB) instead of the default 50M, so that garbage collection,
which also releases the JavaScript values referenced from Haskell, happens regularly;
change it with `-optc -DHEAP_CELLS=N`.

The `browser` target compiles with `-Oz -sSUPPORT_LONGJMP=wasm`.
The evaluator uses `setjmp`, and with emscripten's default JavaScript implementation of `longjmp` every call
out of a function that uses `setjmp` goes through a JavaScript trampoline, which makes `-Oz` (which does not
inline) several times slower than `-O3`.
With WebAssembly exception handling (browsers since 2022) `-Oz` is as fast as `-O3` and about 12% smaller compressed.
`browser_js` has no WebAssembly and stays at `-O3`.

### The browser runtime (`browser_rts`)
With `browser` every change of the program means running `emcc`.
`browser_rts` builds the runtime once; a program is then compiled to a combinator file in seconds
and fetched at run time:
```
mhs -tbrowser_rts [--embed-ffis PKG:...] -o rts.js Main src/runtime/web/rts-web.js   # once
mhs -o app.comb Main                                                                 # per change
```
`src/runtime/web/rts-web.js` fetches `app.comb` (or `Module.combUrl`, the `data-comb` attribute of the
script tag, or in node the `MHS_COMB` environment variable) and runs it.
The runtime's FFI table has the foreign imports of the program it was built with and of the `--embed-ffis`
packages; a program with other foreign imports needs a runtime built for it.
The `.comb` file lists its foreign imports after a `#####` line, so a tool can tell when that is.

### Embedded JavaScript files
JavaScript files can be embedded in the generated output by giving `FILE.js` on the command line
(e.g., a JavaScript library used via the FFI).
A target supports this if it has a `js` key in `mhs.conf` giving the C compiler option to use
(`js = "--pre-js"` for emscripten); for other targets the files are ignored.
Installed packages can also carry JavaScript files, in a `jsbits` directory next to the package
(mcabal puts the `js-sources` of a package there); these are embedded in the same way.

The embedded files go through emscripten's JavaScript optimizer, which removes definitions that nothing uses.
A definition is used when its name occurs in the code of a `foreign import javascript`
(or in JavaScript code that is used); a function reached only through a computed name
(`globalThis["f" + x]()`) is invisible to it.

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
The default _packageDbPath_ is take from the `mhs.conf` file
in the section `[mhs]` and key `packageDbPath`.
The path can contain environment variables that are expanded.
In addition $MHSPKG expand to the package directory relative to the executing binary.
Furthermore $VERSION expands to the mhs version number.

The path can be cleared by using the `-a` command line flag.
To append to the path use `-aPATH`, where `PATH` is a colon (':')
separated list if directories.

## Installing packages

The _packageDb_ directory can be given directly with the `-Q`
flag used to install.

The `mcabal` command can install to a particular package db by using
`--install=PATH`.
