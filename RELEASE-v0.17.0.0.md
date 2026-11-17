# New in MicroHs 0.17.0.0

The biggest change in this release is a new runtime representation of data types, which makes programs faster and smaller. There are also several language extensions, a nicer interactive mode, better error messages, a more complete `Data.Text`, and a rewritten [manual](doc/MicroHs.md).

## Language
 * Qualified strings: `M."..."` means `M.fromString "..."`, so string literals of different types can be mixed without `OverloadedStrings` (see [Qualified strings](doc/MicroHs.md#qualified-strings)).
 * String interpolation: `s"Hello ${name}"` (see [String interpolation](doc/MicroHs.md#string-interpolation)). It also works with multi-line strings, and a qualified form, `M.s"..."`, lets you provide your own interpretation. `Data.String.Interpolate` has helpers for padding and number formatting.
 * `ExtraCommas`: a leading or trailing comma is allowed in most comma separated lists, e.g., `[1, 2, 3,]` (see [Extra commas](doc/MicroHs.md#extra-commas)). GHC will get the same extension.
 * `MultiClassInstances`: one instance declaration can define instances of several classes, e.g., `instance (Eq T, Show T) where ...` with the methods of both classes (see [Multi-class instances](doc/MicroHs.md#multi-class-instances)).
 * `HexFloatLiterals`, e.g., `0x1.8p1`.
 * A binding can have a type annotation on the left hand side, `x :: Int = 5`.
 * A record update can follow a field selection, e.g., `e.a{x = 1}.b`.
 * `'x'#` is the same as `'x'`, and `"abc"#` is a `ByteString` literal.
 * A record pattern in a `do` binding no longer needs `MonadFail` when it cannot fail.
 * A default method for something that is not a method of the class is now an error.
 * Lexing fixes: operators that start with `#`, like `(#)` and `(#>)`, work, and `0x` is `0` followed by `x`.

## Compiler
 * Much nicer error messages, which show the offending source line, in color on a terminal. Set `NO_COLOR` to turn the colors off.
 * `-fno-code` only type checks.
 * `-C[PATH]` gives the location of the compilation cache, and the cache is now keyed by module name rather than file name.
 * `foreign export javascript` makes Haskell functions callable from JavaScript, and `.js` files on the command line are embedded in the output (see [JavaScript](doc/MicroHs.md#javascript)).
 * FFI: imports whose C name is generated, e.g., `capi` value imports and JavaScript imports, no longer clash between modules. The type of a foreign import can have an explicit `forall`.
 * A `case` with many `Int`, `Char`, or `Word` literals uses a binary search.
 * `-z` now works for `.comb` files too.
 * Preprocessor fixes: paths with special characters are quoted properly, whole file names are kept in `#line` directives, and stale output is no longer read.

## Interactive mode
 * `:{` and `:}` for multi-line input.
 * Ctrl-R does an incremental search in the history.
 * `:set +t` shows the type of each evaluated expression, and `:set prompt=...` and `:set prompt-cont=...` change the prompts.
 * Definitions can be written with `let`, as in GHCi.
 * A qualified name from a module that has not been imported, e.g., `Data.Map.empty`, imports the module automatically.

## Libraries
 * `Integer` now uses [imath](https://github.com/creachadair/imath) (in C) by default, which is faster than the old Haskell implementation. GMP can still be used instead.
 * `Data.Text` is much more complete. It uses UTF-8, with primitives in the runtime, its IO handles all of Unicode, and case conversion and case folding follow the Unicode tables, including conversions that give several characters.
 * Type safe serialization: `hSerializeDescr` and `hDeserializeDescr` check that the type of the value read is the expected one (see [Serialization](doc/MicroHs.md#serialization)). Serialization errors are now exceptions.
 * Input that is not valid UTF-8 is accepted, and is written back unchanged.
 * New functions and instances, e.g., `hGetEncoding`, `hFileSize`, `castWord64ToDouble` and friends, `rotate` for fixed-width `Bits` instances, `Monoid (IO a)`, more `Read` instances, more instances for `Identity`, `Const`, and `Down`, and `Num` for `Max` and `Min`.
 * `cprint` and `cuprint` have moved from the `Prelude` to the new module `Mhs.Print`.
 * In `System.IO.TimeMilli`, `getTimeMilli` returns a `Word`, and `getTimeMicro` and `getBootTimeMicro` are new.
 * Behavior closer to the Report and GHC:
   * `enumFromThenTo` for fractional types follows the Report.
   * `elem` and `notElem` have fixity `infix 4`.
   * `(**)` is computed the same way as in GHC.
   * The type variables of `try` are in the same order as in GHC, which matters for type applications.
 * Fixes:
   * `Integer` division by zero raises an exception.
   * ``minBound `rem` (-1)`` no longer crashes.
   * `throwTo` does not return until the exception has been raised in the target thread.
   * `MVar`s and exceptions interact correctly.
   * `atomicModifyIORef` is more atomic.
   * `enumFrom` is stricter, so it does not overflow the stack as easily.
   * `createDirectoryIfMissing` reports failures, and `ioeGetFileName` works for `openFile` errors.
   * A `ByteString` could be corrupted after `forkIO`.

## Runtime
 * A new representation of data types. A constructor or `case` is no longer encoded as a function. Instead, constructors and `case` are special combinators, which are stored in the pointer itself and take no heap space. This gives fewer reductions, less allocation, and faster programs (see [Data type encoding](doc/MicroHs.md#data-type-encoding)).
 * Faster memory management: allocation takes cells from runs of free cells, and the garbage collector prefetches cells while marking. When compiling the compiler, garbage collection is now about 2.5 times faster.
 * `make install-pgo` installs an `mhs` built with profile guided optimization.
 * Programs compiled to JavaScript/WASM are much faster, since they no longer give control back to the JavaScript event loop so often.
 * Loading combinator code is faster, since primitives are looked up with a hash table.
 * The default stack size is now 300k entries.
 * Configurations without `WANT_LZMA`, `WANT_UTF8`, or `WANT_FLOAT32` build again, and a 64 bit problem on Windows is fixed.

## Tools, testing, and documentation
 * The manual has been rewritten, and is now in [doc/MicroHs.md](doc/MicroHs.md). The combinator file format is described in [doc/COMB.md](doc/COMB.md).
 * A new test driver, `bin/tester`, runs the tests listed in `tests/all.tests` and `tests/err.tests`.
 * The `benchmarks/` directory has a set of benchmark programs.
 * Fixes for building with Hugs.
 * There is an [AI policy](AI-POLICY.md) for contributions.

## Incompatible changes
 * The combinator file format is now version 9.1, so `.comb` files and packages from earlier versions must be rebuilt.
 * `cprint` and `cuprint` must be imported from `Mhs.Print`.
 * `getTimeMilli` returns a `Word` instead of an `Int`.
 * The type variables of `try` are in a different order.
