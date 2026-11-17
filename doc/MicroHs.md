# MicroHs

MicroHs (`mhs`) is a compiler for an extended subset of Haskell. It compiles Haskell to combinators, which are then executed by a small runtime system written in C. The compiler is written in MicroHs and can compile itself. All you need to build and run MicroHs is a C compiler.

Contents:
* [Language](#language)
* [Compiler](#compiler)
* [Packages](#packages)
* [Implementation](#implementation)
* [Platforms](#platforms)
* [Contributing](#contributing)

---

# Language

The language is an extended subset of [Haskell 2010](https://www.haskell.org/onlinereport/haskell2010/).

Differences from Haskell 2010 and GHC:
* All supported extensions are always enabled. The only exception is `CPP`, which must be turned on with `{-# LANGUAGE CPP #-}` or the `-XCPP` flag.
* Kind variables need an explicit `forall`.
* Data type contexts are not supported.
* `main` in the top module given to `mhs` is the program entry point, whatever the module is called.
* Many things that should be errors (but are mostly harmless) are not reported.
* Source code and text file I/O always use UTF-8.
* `BangPatterns` are parsed, but only have an effect in a top level `let`/`where`.
* `Int` arithmetic raises an exception on overflow instead of wrapping (on platforms where the runtime enables it).
* Unboxed tuple and sum syntax is accepted, but nothing is actually unboxed.
* Mutually recursive modules are allowed, the same way as in GHC, using `.hs-boot` files.

## Extensions

The following extensions are supported:
* [AllowAmbiguousTypes](https://downloads.haskell.org/ghc/latest/docs/users_guide/exts/ambiguous_types.html#extension-AllowAmbiguousTypes)
* [BangPatterns](https://downloads.haskell.org/ghc/latest/docs/users_guide/exts/strict.html#extension-BangPatterns) (see above)
* [BinaryLiterals](https://downloads.haskell.org/ghc/latest/docs/users_guide/exts/binary_literals.html#extension-BinaryLiterals)
* [BlockArguments](https://downloads.haskell.org/ghc/latest/docs/users_guide/exts/block_arguments.html#extension-BlockArguments)
* [CApiFFI](https://downloads.haskell.org/ghc/latest/docs/users_guide/exts/ffi.html#extension-CApiFFI)
* [ConstrainedClassMethods](https://downloads.haskell.org/ghc/latest/docs/users_guide/exts/constrained_class_methods.html#extension-ConstrainedClassMethods)
* [ConstraintKinds](https://downloads.haskell.org/ghc/latest/docs/users_guide/exts/constraint_kind.html#extension-ConstraintKinds)
* [CPP](https://downloads.haskell.org/ghc/latest/docs/users_guide/phases.html#extension-CPP)
* [DefaultSignatures](https://downloads.haskell.org/ghc/latest/docs/users_guide/exts/default_signatures.html#extension-DefaultSignatures)
* [DeriveAnyClass](https://downloads.haskell.org/ghc/latest/docs/users_guide/exts/derive_any_class.html#extension-DeriveAnyClass)
* [DeriveDataTypeable](https://downloads.haskell.org/ghc/latest/docs/users_guide/exts/deriving_extra.html#extension-DeriveDataTypeable)
* [DeriveFoldable](https://downloads.haskell.org/ghc/latest/docs/users_guide/exts/deriving_extra.html#extension-DeriveFoldable)
* [DeriveFunctor](https://downloads.haskell.org/ghc/latest/docs/users_guide/exts/deriving_extra.html#extension-DeriveFunctor)
* [DeriveTraversable](https://downloads.haskell.org/ghc/latest/docs/users_guide/exts/deriving_extra.html#extension-DeriveTraversable)
* [DerivingStrategies](https://downloads.haskell.org/ghc/latest/docs/users_guide/exts/deriving_strategies.html#extension-DerivingStrategies)
* [DerivingVia](https://downloads.haskell.org/ghc/latest/docs/users_guide/exts/deriving_via.html#extension-DerivingVia)
* [DisambiguateRecordFields](https://downloads.haskell.org/ghc/latest/docs/users_guide/exts/disambiguate_record_fields.html#extension-DisambiguateRecordFields)
* [DoAndIfThenElse](https://downloads.haskell.org/ghc/latest/docs/users_guide/exts/doandifthenelse.html#extension-DoAndIfThenElse)
* [DuplicateRecordFields](https://downloads.haskell.org/ghc/latest/docs/users_guide/exts/duplicate_record_fields.html#extension-DuplicateRecordFields)
* [EmptyCase](https://downloads.haskell.org/ghc/latest/docs/users_guide/exts/empty_case.html#extension-EmptyCase)
* [EmptyDataDecls](https://downloads.haskell.org/ghc/latest/docs/users_guide/exts/nullary_types.html#extension-EmptyDataDecls)
* [EmptyDataDeriving](https://downloads.haskell.org/ghc/latest/docs/users_guide/exts/empty_data_deriving.html#extension-EmptyDataDeriving)
* [ExistentialQuantification](https://downloads.haskell.org/ghc/latest/docs/users_guide/exts/existential_quantification.html#extension-ExistentialQuantification)
* [ExplicitForAll](https://downloads.haskell.org/ghc/latest/docs/users_guide/exts/explicit_forall.html#extension-ExplicitForAll)
* [ExplicitNamespaces](https://downloads.haskell.org/ghc/latest/docs/users_guide/exts/explicit_namespaces.html#extension-ExplicitNamespaces)
* [ExtendedDefaultRules](https://downloads.haskell.org/ghc/latest/docs/users_guide/ghci.html#extension-ExtendedDefaultRules)
* ExtraInstanceBindings
* [FlexibleContexts](https://downloads.haskell.org/ghc/latest/docs/users_guide/exts/flexible_contexts.html#extension-FlexibleContexts)
* [FlexibleInstances](https://downloads.haskell.org/ghc/latest/docs/users_guide/exts/instances.html#extension-FlexibleInstances)
* [FunctionalDependencies](https://downloads.haskell.org/ghc/latest/docs/users_guide/exts/functional_dependencies.html#extension-FunctionalDependencies)
* [GADTs](https://downloads.haskell.org/ghc/latest/docs/users_guide/exts/gadt.html#extension-GADTs)
* [GADTSyntax](https://downloads.haskell.org/ghc/latest/docs/users_guide/exts/gadt_syntax.html#extension-GADTSyntax)
* [GeneralisedNewtypeDeriving](https://downloads.haskell.org/ghc/latest/docs/users_guide/exts/newtype_deriving.html#extension-GeneralisedNewtypeDeriving)
* [ImportQualifiedPost](https://downloads.haskell.org/ghc/latest/docs/users_guide/exts/import_qualified_post.html#extension-ImportQualifiedPost)
* ImportShadowing
* [IncoherentInstances](https://downloads.haskell.org/ghc/latest/docs/users_guide/exts/instances.html#extension-IncoherentInstances)
* [InstanceSigs](https://downloads.haskell.org/ghc/latest/docs/users_guide/exts/instances.html#extension-InstanceSigs)
* [KindSignatures](https://downloads.haskell.org/ghc/latest/docs/users_guide/exts/kind_signatures.html#extension-KindSignatures)
* [LambdaCase](https://downloads.haskell.org/ghc/latest/docs/users_guide/exts/lambda_case.html#extension-LambdaCase)
* [LiberalTypeSynonyms](https://downloads.haskell.org/ghc/latest/docs/users_guide/exts/liberal_type_synonyms.html#extension-LiberalTypeSynonyms)
* [MagicHash](https://downloads.haskell.org/ghc/latest/docs/users_guide/exts/magic_hash.html#extension-MagicHash)
* [MonomorphismRestriction](https://downloads.haskell.org/ghc/latest/docs/users_guide/exts/monomorphism.html#extension-MonomorphismRestriction)
* [MultilineStrings](https://downloads.haskell.org/ghc/latest/docs/users_guide/exts/multiline_strings.html#extension-MultilineStrings)
* [MultiParamTypeClasses](https://downloads.haskell.org/ghc/latest/docs/users_guide/exts/multi_param_type_classes.html#extension-MultiParamTypeClasses)
* [MultiWayIf](https://downloads.haskell.org/ghc/latest/docs/users_guide/exts/multiway_if.html#extension-MultiWayIf)
* [NamedDefaults](https://downloads.haskell.org/ghc/latest/docs/users_guide/exts/named_defaults.html#extension-NamedDefaults)
* [NamedFieldPuns](https://downloads.haskell.org/ghc/latest/docs/users_guide/exts/record_puns.html#extension-NamedFieldPuns)
* [NegativeLiterals](https://downloads.haskell.org/ghc/latest/docs/users_guide/exts/negative_literals.html#extension-NegativeLiterals)
* [NoDatatypeContexts](https://downloads.haskell.org/ghc/latest/docs/users_guide/exts/datatype_contexts.html#extension-DatatypeContexts)
* [NondecreasingIndentation](https://downloads.haskell.org/ghc/latest/docs/users_guide/bugs.html#extension-NondecreasingIndentation)
* [NoStarIsType](https://downloads.haskell.org/ghc/latest/docs/users_guide/exts/poly_kinds.html#extension-StarIsType)
* [NumericUnderscores](https://downloads.haskell.org/ghc/latest/docs/users_guide/exts/numeric_underscores.html#extension-NumericUnderscores)
* [OrPatterns](https://downloads.haskell.org/ghc/latest/docs/users_guide/exts/or_patterns.html#extension-OrPatterns)
* [OverlappingInstances](https://downloads.haskell.org/ghc/latest/docs/users_guide/exts/instances.html#extension-OverlappingInstances)
* [OverloadedRecordDot](https://downloads.haskell.org/ghc/latest/docs/users_guide/exts/overloaded_record_dot.html#extension-OverloadedRecordDot)
* [OverloadedRecordUpdate](https://downloads.haskell.org/ghc/latest/docs/users_guide/exts/overloaded_record_update.html#extension-OverloadedRecordUpdate)
* [OverloadedStrings](https://downloads.haskell.org/ghc/latest/docs/users_guide/exts/overloaded_strings.html#extension-OverloadedStrings)
* [PatternSynonyms](https://downloads.haskell.org/ghc/latest/docs/users_guide/exts/pattern_synonyms.html#extension-PatternSynonyms)
* [PolyKinds](https://downloads.haskell.org/ghc/latest/docs/users_guide/exts/poly_kinds.html#extension-PolyKinds)
* PolymorphicComponents
* [PostfixOperators](https://downloads.haskell.org/ghc/latest/docs/users_guide/exts/rebindable_syntax.html#extension-PostfixOperators)
* [QualifiedDo](https://downloads.haskell.org/ghc/latest/docs/users_guide/exts/qualified_do.html#extension-QualifiedDo)
* QualifiedStrings (see [below](#qualified-strings))
* [RankNTypes](https://downloads.haskell.org/ghc/latest/docs/users_guide/exts/rank_polymorphism.html#extension-RankNTypes)
* [RecordWildCards](https://downloads.haskell.org/ghc/latest/docs/users_guide/exts/record_wildcards.html#extension-RecordWildCards)
* [RecursiveDo](https://downloads.haskell.org/ghc/latest/docs/users_guide/exts/recursive_do.html#extension-RecursiveDo) (without optimal `mdo` desugaring)
* [RequiredTypeArguments](https://downloads.haskell.org/ghc/latest/docs/users_guide/exts/required_type_arguments.html#extension-RequiredTypeArguments)
* [ScopedTypeVariables](https://downloads.haskell.org/ghc/latest/docs/users_guide/exts/scoped_type_variables.html#extension-ScopedTypeVariables)
* [StandaloneDeriving](https://downloads.haskell.org/ghc/latest/docs/users_guide/exts/standalone_deriving.html#extension-StandaloneDeriving)
* [StandaloneKindSignatures](https://downloads.haskell.org/ghc/latest/docs/users_guide/exts/poly_kinds.html#extension-StandaloneKindSignatures)
* StringInterpolation (see [below](#string-interpolation))
* [TupleSections](https://downloads.haskell.org/ghc/latest/docs/users_guide/exts/tuple_sections.html#extension-TupleSections) (only pairs right now)
* [TypeApplications](https://downloads.haskell.org/ghc/latest/docs/users_guide/exts/type_applications.html#extension-TypeApplications)
* TypeLits
* [TypeOperators](https://downloads.haskell.org/ghc/latest/docs/users_guide/exts/type_operators.html#extension-TypeOperators)
* [TypeSynonymInstances](https://downloads.haskell.org/ghc/latest/docs/users_guide/exts/instances.html#extension-TypeSynonymInstances)
* [UndecidableInstances](https://downloads.haskell.org/ghc/latest/docs/users_guide/exts/instances.html#extension-UndecidableInstances)
* [UndecidableSuperClasses](https://downloads.haskell.org/ghc/latest/docs/users_guide/exts/undecidable_super_classes.html#extension-UndecidableSuperClasses)
* [UnicodeSyntax](https://downloads.haskell.org/ghc/latest/docs/users_guide/exts/unicode_syntax.html#extension-UnicodeSyntax)
* [ViewPatterns](https://downloads.haskell.org/ghc/latest/docs/users_guide/exts/view_patterns.html#extension-ViewPatterns)

Deriving is supported for `Bounded`, `Data`, `Enum`, `Eq`, `Foldable`, `Functor`, `Generic`, `Generic1`, `Ix`, `Lift`, `Ord`, `Read`, `Show`, and `Traversable`.  `Typeable` is automatic.

## Records

MicroHs implements the record dot extensions, so accessing a field `a` in record `r` is written `r.a`, as well as the usual `a r`. The former is overloaded and can access any `a` field, whereas the latter is the usual monomorphic field selector. Updating a field has the usual Haskell syntax `r{ a = e }`, but the type is overloaded so this can update the `a` field in any record. The type classes `HasField` and `SetField` capture this. `HasField "name" rec ty` expresses that the record type `rec` has a field `name` with type `ty` that can be extracted with `getField`. `SetField "name" rec ty` expresses that the record type `rec` has a field `name` with type `ty` that can be set with `setField`.

Record updates can also update nested fields, e.g., `r{ a.b.c = e }`.  Note that this will not easily work in GHC, since GHC does not fully implement `OverloadedRecordUpdate`.  When GHC decides how to do it, MicroHs will follow suit.

Record updates cannot change the type of polymorphic fields.

## Qualified strings

A string literal can be qualified with a module name, `M."..."`. This means `M.fromString "..."`, using whatever `fromString` is in scope as `M.fromString`. It gives a lightweight way to have string literals of different types without turning on `OverloadedStrings` globally.

## String interpolation

A string literal prefixed by `s` can contain interpolated expressions, written `${expr}`:
```haskell
greet :: String -> Int -> String
greet name n = s"Hello ${name}, you are number ${n + 1}"
```
Interpolation also works with multi-line strings, `s"""..."""`, and interpolated strings can be nested.

The string `s"foo ${e} bar"` is translated into
```haskell
interpolateFinalize (interpolateRaw "foo " `interpolateAppend`
                     interpolateValue e    `interpolateAppend`
                     interpolateRaw " bar" `interpolateAppend`
                     interpolateEmpty)
```
The unqualified form uses the definitions in `Data.String.Interpolate`, where `interpolateValue` uses the `Interpolate` class (similar to `Show`, but strings and characters are not quoted) and the result can be any `IsString` type. A qualified form, `M.s"..."`, uses `M.interpolateRaw` etc. instead, so you can provide your own interpretation. `Data.String.Interpolate.Basic` is a simple alternative where all interpolated values must already have the result type.

## Libraries

The `Prelude` contains the functions from the Haskell Report and a few extensions. The `base` package contains most of what GHC has in `base`.

To completely avoid importing the `Prelude` (which can be needed in `base` to avoid circular imports) use the incantation `import qualified Prelude()`. This will not even try to open `Prelude.hs`.  It also avoids the automagic (qualified) import of `Mhs.Builtin` that normally happens.

### Report compliance

| Module                 | Report compliant          | Comment                                                 |
| ---------------------- | ------------------------: | --------------------------------------------------------|
| Prelude                | yes<sup>[1](#note1)</sup> | missing `catch` (as in GHC)                             |
| Control.Monad          | yes<sup>[1](#note1)</sup> | `fail` not in `Monad` (as in GHC)                       |
| Data.Array             |                       yes |                                                         |
| Data.Bits              |                       yes |                                                         |
| Data.Char              |                        no | missing `lexLitChar`, `readLitChar`                     |
| Data.Complex           | yes<sup>[1](#note1)</sup> | no datatype context                                     |
| Data.Int               |                       yes |                                                         |
| Data.Ix                |                       yes |                                                         |
| Data.List              |                       yes |                                                         |
| Data.Maybe             |                       yes |                                                         |
| Data.Ratio             |                       yes |                                                         |
| Data.Word              |                       yes |                                                         |
| Foreign                |                       yes |                                                         |
| Foreign.C              |                       yes |                                                         |
| Foreign.C.Error        |                       yes |                                                         |
| Foreign.C.String       |                        no | missing functions (in particular `CWString` related)    |
| Foreign.C.Types        |                        no | missing/wrong instances                                 |
| Foreign.ForeignPtr     |                        no | missing `FinalizerEnvPtr`, `newForeignPtrEnv`, `addForeignPtrFinalizerEnv`, `finalizeForeignPtr`, `unsafeForeignPtrToPtr` |
| Foreign.Marshal        |                       yes |                                                         |
| Foreign.Marshal.Alloc  |                       yes |                                                         |
| Foreign.Marshal.Array  |                       yes |                                                         |
| Foreign.Marshal.Error  | yes<sup>[1](#note1)</sup> | `void :: Functor f => f a -> f ()`                      |
| Foreign.Marshal.Utils  |                       yes |                                                         |
| Foreign.Ptr            |                       yes | `freeHaskellFunPtr` does nothing                        |
| Foreign.StablePtr      |                       yes |                                                         |
| Foreign.Storable       |                       yes |                                                         |
| Numeric                |                       yes |                                                         |
| System.Environment     |                       yes |                                                         |
| System.Exit            |                       yes |                                                         |
| System.IO              |                        no | some missing functions and types                        |
| System.IO.Error        | yes<sup>[1](#note1)</sup> | renamed `catch`, `try` to `catchIOError`, `tryIOError` (as in GHC) |

<a name="note1">1.</a> not quite, but close enough, see comment

## FFI

MicroHs supports `foreign import ccall`, `foreign import capi`, `foreign export ccall`, and `foreign import javascript` (for the emscripten target). When running a program directly (using `-r`), in the interactive system, or when generating a `.comb` file, only the C functions built into the runtime (plus those from embedded packages) can be called. When generating a `.c` file or an executable, any C function can be called.

As in GHC, the import string can name header files and use `&` to import the address of a C object. In addition, MicroHs has a non-standard form, `"value EXPR"`, which imports the value of an arbitrary C expression. Inside `EXPR`, `$1`, `$2`, … (up to `$9`) are replaced by the function's arguments. For example:
```haskell
foreign import capi "value EINTR" eINTR :: Errno
foreign import ccall "value ($1 + $2) * 2" addTwice :: CInt -> CInt -> IO CInt
```
The expression is pasted verbatim into the generated C code, so it must be a valid C expression for the argument and result types. A type without `IO` gives a pure value.

There is still a lot of missing FFI functionality compared to GHC.

## Preprocessor

Sadly, compiling a lot of Haskell packages needs the C preprocessor. MicroHs uses `cpphs`, and the distribution contains the combinator code for it, so `make` builds `bin/cpphs`. The symbol `__MHS__` is defined when compiling with MicroHs.

---

# Compiler

The compiler takes the name of a module (or a file name) and compiles it. The module should contain the function `main` of type `IO ()`, which is the entry point of the program. If no module is given, the compiler starts in interactive mode.

## Modules

A module `M.N` is looked for as the file `M/N.hs` (or `.lhs`, or `.hsc`, which is run through `hsc2hs`) in each directory of the module search path, which defaults to `.` and can be changed with `-i`. If it is not found there, the installed packages are searched (see [Packages](#packages)). If the name given on the command line ends in `.hs` or `.lhs` it is treated as a file name.

## Compiler flags

| Flag | Meaning |
| ---- | ------- |
| `-h`, `-?` | Print short usage |
| `--help` | Print all flags |
| `--version` | Print the version |
| `--numeric-version` | Print just the version number |
| `-v` | Increase verbosity (can be repeated) |
| `-q` | Be quiet |
| `-oFILE` | Output to `FILE`. If `FILE` ends in `.comb`, produce a combinator file. If it ends in `.c`, produce a C file. Otherwise compile the combinators together with the runtime system to produce an executable. Default is `out.comb`. |
| `-r` | Run the program directly instead of writing a file |
| `-c` | Do not link; just produce an object file |
| `-eEXPR` | Evaluate `EXPR` (in the context of the given modules) |
| `--interactive` | Start interactive mode even with module arguments |
| `-iPATH` | Add `PATH` (colon separated) to the module search path |
| `-i` | Clear the module search path |
| `-aPATH` | Add `PATH` (colon separated) to the package search path |
| `-a` | Clear the package search path |
| `-PPKG` | Build package `PKG` from the given modules |
| `-Q PKG [DIR]` | Install package `PKG`, optionally into `DIR` |
| `-L[FILE\|PKG]` | List all modules of a package |
| `-pFILE` | Preload a package |
| `-tTARGET` | Select a target from `mhs.conf` (see [Targets](#targets)) |
| `-optc OPTION` | Pass an option to the C compiler |
| `-optl OPTION` | Pass an option to the C compiler for linking |
| `-XCPP` | Run `cpphs` on all source files |
| `-Dxxx`, `-Ixxx` | Passed to `cpphs` |
| `-F` | Run a preprocessor on the source files |
| `-pgmF CMD` | Use `CMD` as the `-F` preprocessor |
| `-optF FLAG` | Pass `FLAG` to the `-F` preprocessor |
| `-C[PATH]` | Read and write the compilation cache, optionally at `PATH` (default `.mhscache`) |
| `-CR[PATH]` | Only read the compilation cache |
| `-CW[PATH]` | Only write the compilation cache |
| `-z` | Compress the combinator code (LZMA) |
| `-b64` | Base64 encode the combinator code |
| `-T` | Generate dynamic function usage statistics |
| `-l` | Show every time a module is loaded |
| `-s` | Show compilation speed in lines/s |
| `-fno-code` | Type check only; do not generate code |
| `-ECMD` | Set the editor for the `:edit` command |
| `-interactive-print FUN` | Use `FUN` to print values in interactive mode |
| `--stdin` | Use stdin in the interactive system (no line editing) |
| `--embed-packages PKG:...` | Embed packages (and their FFI stubs) in an `mhs` binary |
| `--embed-ffis PKG:...` | Embed the FFI stubs of packages in an `mhs` binary |
| `-ddump-PASS` | Debug: print the program after `PASS`, one of `preproc`, `parse`, `derive`, `expandInst`, `typecheck`, `desugar`, `linked`, `toplevel`, `combinator`, `all` |
| `--` | End of compiler arguments; the rest are passed to the program (with `-r`) |

Files ending in `.c`, `.o`, or `.a` on the command line are passed to the C compiler, and `.js` files are embedded in the output (for targets that support it).

With the `-v` flag the processing time for each module is reported, e.g.
```
importing done MicroHs.Exp, 284ms (91 + 193)
```
which means that processing `MicroHs.Exp` took 284ms, with parsing taking 91ms and type checking and desugaring taking 193ms.

### Compilation cache
With the `-C` flag the compiler writes its internal cache of compiled modules to `.mhscache` at the end of compilation.  At startup it reads this file if it exists, and validates the contents using an MD5 checksum of each file in the cache. This can make compilation much faster, since a cached module is not parsed and type checked again. Do **not** use `-C` when you are changing the compiler itself; if the cached data types change the compiler will probably just crash.

## Environment variables

* `MHSDIR` the directory where `lib/` and `src/runtime/` are expected to be.  By default this is found relative to the `mhs` binary.
* `MHSCPPHS` command to use for `-XCPP`.  Defaults to `cpphs`.
* `MHSHSC2HS` command to use for `.hsc` files.  Defaults to `hsc2hs`.
* `NO_COLOR` disables colored error messages.

## Interactive mode

If no module name is given, the compiler enters interactive mode.  You can also use `--interactive` to enter it after loading some modules. Normally input is read from the terminal with line editing (see below). With the `--stdin` flag, input is read as plain lines from standard input without line editing, which is useful when input is piped from a file or another program, e.g., `mhs --stdin < script.txt`.

At the prompt you can enter an expression to evaluate it, a top level definition (or an `import`) to add it, or a command starting with `:`. An expression of type `IO a` is run, and its result is printed unless it is `()`.  Any other expression is printed with `show`. If an expression uses a qualified name, e.g., `Data.Map.empty`, and the module is not imported, a qualified import is added automatically.

### Commands

Commands can be abbreviated to any unique prefix, e.g., `:t` for `:type`.

| Command | Meaning |
| ------- | ------- |
| `:quit` | Quit |
| `:{` | Start multi-line input, ended by a line with `:}` |
| `:clear` | Clear all definitions and imports |
| `:reload` | Reload all modules |
| `:delete PRE` | Delete the definitions (and imports) whose lines start with `PRE` |
| `:defs` | Show the current definitions |
| `:type EXPR` | Show the type of `EXPR` |
| `:kind TYPE` | Show the kind of `TYPE` |
| `:main ARGS` | Run `main` with the given arguments |
| `:save [FILE]` | Save the current definitions to a file |
| `:edit [FILE]` | Edit a file, or the location of the last error, and then reload |
| `:find NAME` | Edit the file containing the definition of `NAME` |
| `:! CMD` | Run a shell command |
| `:set [OPT]` | Show the options, or set an option (see below) |
| `:help`, `:?` | Show the commands |

The options for `:set` are:
* `+s`/`-s` show evaluation statistics (time, reductions, and allocated cells)
* `+c`/`-c` show compilation time
* `+t`/`-t` show the type of each evaluated expression
* `+color`/`-color` use color in error messages
* `path=PATH` set the module search path
* `prompt=STR` set the prompt
* `prompt-cont=STR` set the prompt for continuation lines in multi-line input

The editor used by `:edit` and `:find` is set with the `-E` flag, e.g., `'-Eemacs +%d %s'`, where `%d` is replaced by the line number and `%s` by the file name.  The default is `vim +%d %s`.

### Line editing

The input line can be edited with Emacs-like keys:

| Key | Action |
| --- | ------ |
| Ctrl-A, Home | Go to beginning of line |
| Ctrl-E, End | Go to end of line |
| Ctrl-B, Left | Move back one character |
| Ctrl-F, Right | Move forward one character |
| Backspace | Delete the character before the cursor |
| Ctrl-D, Delete | Delete the character under the cursor |
| Ctrl-K | Delete to the end of the line |
| Ctrl-U | Erase the whole line |
| Ctrl-P, Up | Previous line in the history |
| Ctrl-N, Down | Next line in the history |
| Ctrl-R | Reverse incremental search in the history |
| TAB | Complete an identifier, module name, or keyword |
| Return | Submit the line |
| Ctrl-C | Interrupt a running evaluation |

During an incremental search (Ctrl-R), typed characters extend the search string, Ctrl-R finds the next older match, Backspace undoes the last step, and Ctrl-G aborts the search. Any other key accepts the found line and then performs its usual action.

All input lines are saved in `~/.mhsi`, which is read at startup so the history persists between sessions.
## Targets

`mhs` compiles the combinators together with the runtime system using a C compiler. How this is done is controlled by the configuration file `mhs.conf` (installed in the `mhs` data directory), and the `-t` flag selects a target.  A target section looks like this:
```ini
[unix]
cc = "cc"
ccflags = "-w -Wall -O3"
cclibs = "-lm"
conf = "unix"
```
The keys are `cc` (the C compiler), `ccflags` (flags at the start of the command), `cclibs` (flags at the end), `conf` (which `src/runtime/` platform directory to use), `cout` (the flag for the output file, default `-o`), and `js` (the flag to embed a JavaScript file).

The distributed targets are:
* `unix` the default on non-Windows systems
* `unix_lto`, `unix_x86` with more aggressive optimization
* `debug` compile with `-g` instead of optimization
* `emscripten` generate JavaScript/WASM using `emcc`
* `emscripten_web` JavaScript/WASM for use in a web page with xterm.js
* `tcc` use the Tiny C Compiler
* `windows` the default on Windows, using `cl`
* `environment` take everything from the environment variables `CC`, `MHSCCFLAGS`, `MHSCCLIBS`, and `MHSCONF`

You can add your own targets to `mhs.conf`.

For example, with `emcc` and `node` installed:
```
mhs -temscripten Example -oout.js
node out.js
```
The generated JavaScript file contains the WASM code embedded as a blob. Running via JavaScript/WASM is almost as fast as running natively.

The `[mhs]` section of `mhs.conf` has the key `packageDbPath`, which is the default package search path.

---

# Packages

To avoid compiling everything from source all the time, there is a notion of a precompiled package. A package is a set of modules that are compiled together and can then be installed in a known place. Packages can depend on other installed packages.

There is no need for any extra flags to `mhs` to use installed packages; they are all visible at all times. When compiling to an executable, only the parts of a package that are actually used are included.

## Creating and installing a package with `mhs`

To build a package, use `mhs -Ppackage-name -opackage-name.pkg modules...`, where `modules...` are the modules to expose from the package.  Other modules that are needed are included automatically.

To install a package, use `mhs -Q package-name.pkg [install-dir]`. If `install-dir` is left out, the package is installed in the default place.

To list the modules in a package, use `mhs -Lpackage-name.pkg`.

## MicroCabal (`mcabal`)

Normally you don't call `mhs` directly to build packages. Instead you use `mcabal`, a reimplementation of a subset of Cabal, which is included with MicroHs. It reads the package's `.cabal` file and does the right thing.

* `mcabal install` builds and installs the package in the current directory.
* `mcabal install PKG` downloads `PKG` from Hackage, then builds and installs it.  Add `-r` to also install missing dependencies.
* `mcabal install --git=URL [--dir=DIR] PKG` gets the package from a git repository instead.
* `mcabal build`, `mcabal test`, and `mcabal clean` do the obvious things.
* `mcabal update` gets a new set of consistent package versions from Stackage.

To get a consistent set of packages, MicroCabal uses Stackage (nightly by default) to pick package versions. It assumes a Unix-like system with `curl` (or `wget`, if `MICROCABAL_USE_WGET` is set) and `tar`.

## Installed package organization

Packages are installed in `~/.mcabal/` (or `$CABALDIR`) with the following layout:
* `bin/` installed binaries, initially `mhs`, `cpphs`, and `mcabal`
* `mhs-VER/` installed files for a given `mhs` version
* `mhs-VER/packages/` a `PKG.pkg` file for each library package and (maybe) a directory `PKG/` that contains `data/`, `include/`, and `cbits/` for the package
* `mhs-VER/.../MODULE.txt` a file for each installed module, containing the name of the package that has that module

There can only be one version of a package installed in a directory. If you need multiple versions, use different directories and the `-a` flag. There are no checks for consistency among packages, and packages compiled with different versions of the compiler are not compatible.

## Porting a package

Hackage is full of great packages; unfortunately most of them contain some GHC specific code. Some compile as is, many need small changes.  Here are some tips:
* Use CPP to make MicroHs specific changes, testing `#ifdef __MHS__`.
* In the `.cabal` file you can use `if impl(mhs)` conditionals, and `mhs-options:` for MicroHs specific flags.
* GHC specific modules (e.g., `GHC.Exts`) are often only partially available.  The `ghc-compat` package provides some of this.
* Things that commonly need fixing are unboxed types and primitives, Template Haskell, and unsupported extensions.

When you have a working port, please try to get the changes upstream, and report the status.

## Available packages

Included in `base` (and installed with the compiler): `array`, `bytestring`, `deepseq`, `directory`, `hashable`, `process`, `stm`, `text` (with varying degrees of completeness).

Some important packages that are known to work: `containers`, `transformers`, `mtl`, `parsec`, `filepath`, `time`, `random`, `QuickCheck`, `hspec`, `HUnit`, `pretty`, `prettyprinter`, `optparse-applicative`, `unordered-containers`, `async`, `split`, `fgl`, `cpphs`.

`make -f Makefile.packages` installs a large set of packages that are known to compile. There is also a [spreadsheet with the status for many packages](https://docs.google.com/spreadsheets/d/1e0dbUg5uuFKNwgMpwtBnYRldPCYYyBqYfsbyhEjf5bU/edit?usp=sharing). If a package is not listed it just means that nobody has reported success or failure with it.  Please do so.

---

# Implementation

## Compiler

The compiler is written in MicroHs and lives in `src/MicroHs/`. It can also be compiled with GHC (`make bin/gmhs`) and with a slightly patched Hugs (see below). When compiling with GHC, the `ghc/` directory provides some compatibility modules.

The compiler pipeline is:
1. Lexing and layout processing (`Lex`), and parsing to an abstract syntax tree (`Parse`, `Expr`).
2. Deriving instances (`Deriving`).
3. Fixity resolution (`Fixity`) and type checking (`TypeCheck`, `TCMonad`), which also translates type classes to dictionary passing.
4. Desugaring to a simple lambda calculus (`Desugar`, `Exp`), where data types are encoded as functions (`EncodeData`). Data types with at most 6 constructors use Scott encoding; larger types use a constructor number and a tuple of the arguments.
5. Translation to combinators by bracket abstraction, with optimizations (`Abstract`).
6. Serialization of the combinator graph (`ExpPrint`), optionally compressed and turned into a C array (`MakeCArray`).

Other important modules:
* `Main` decodes flags and drives everything.
* `Compile` finds, compiles, and caches modules, and builds packages.
* `CompileCache`, `Package` the compilation cache and the package format.
* `Interactive` the REPL.  It uses `Translate`, which converts combinators directly to runnable values.
* `FFI` generates C wrappers for foreign imports and exports.
* `Builtin`, `SymTab`, `Ident`, `IdentMap`, `Graph` (strongly connected components), `StateIO`, and `Config`.

### Bootstrapping

The repository contains the compiler itself compiled to combinators (as C code) in `generated/mhs.c`. This is how `mhs` is built from just the C compiler.

To rebuild the compiler after changing it, use `make newmhs` (or `make newmhsz` to compress). `make bootstrap` recompiles the compiler twice and compares the outputs to make sure the new compiler still works.

MicroHs can also be built from scratch in the sense of [bootstrappable.org](https://bootstrappable.org/), using Hugs (`make bin/hmhs`).  This needs [a slightly patched version of Hugs](https://github.com/augustss/hugs98-plus-Sep2006), to work around undefined behavior with arithmetic overflow.

## Runtime

The runtime system is written in C and is mainly in `src/runtime/eval.c`. It is a graph reduction machine for combinators, with primitive operations for built in types and for executing IO operations. The C code is reasonably portable and has no dependencies beyond a C compiler and (optionally) a C library.

The runtime also contains:
* A mark-scan garbage collector.
* Lightweight threads (for `Control.Concurrent`, `MVar`, and STM), with non-blocking IO where available.
* Bignums for `Integer`, using [imath](https://github.com/creachadair/imath) (included); alternatively [GMP](https://gmplib.org/).
* Serialization and deserialization of arbitrary expressions, preserving sharing and cycles. The only exceptions are C pointers (e.g., file handles) other than `stdin`, `stdout`, and `stderr`.
* LZMA/LZ77 decompression and Base64 decoding, for compressed combinator code.
* Buffered file I/O with UTF-8 encoding.

### Types
Some types are primitive and known by the runtime, e.g., `Int`, `Word`, `Double`, `Float`, `Ptr`, `IO`, and arrays. The function type, `->`, is (of course) also built in. All other types are defined in Haskell and encoded as lambda terms by the compiler. The runtime knows how lists, booleans, and tuples are encoded.

### Memory layout
Memory is allocated in cells.  Each cell has room for two pointers (two words, typically 16 bytes), so it can represent an application node.  A few bits indicate if the cell is an application or something else. If it is something else, one word is a tag, e.g., a combinator or an integer, and the other word stores the payload, e.g., the integer value.

There is a bitmap with one bit per cell. Allocating a cell consists of finding the next free cell using the bitmap and marking it as used; in practice whole runs of free cells are claimed at once. The garbage collector clears the bitmap and then marks every reachable cell. There is no explicit scan phase, since that is baked into the allocation. Allocation is fast, assuming the CPU has some kind of find-first-set instruction.

### Runtime flags
Runtime flags are given between `+RTS` and `-RTS`. Everything else is available to the running program.

* `-HSIZE` set heap size to `SIZE` cells, can be suffixed by `k`, `M`, or `G`; default is `50M`
* `-KSIZE` set stack size to `SIZE` entries, can be suffixed by `k`, `M`, or `G`; default is `250k`
* `-rFILE` read combinators from `FILE` (for `mhseval`), instead of `out.comb`
* `-v` be more verbose, can be repeated
* `-T` generate profiling statistics (if compiled with `-T` as well)
* `-B` ring the bell on every GC
* `-oFILE` read the input, run a garbage collection, and write the resulting graph to `FILE`

For example, `./prog +RTS -H1M -v -RTS hello` runs `prog` with a heap of 1M cells, verbose runtime output, and the program gets the single argument `hello`.

### Configuration
The runtime is configured for each platform by a `config.h` in a subdirectory of `src/runtime/`. It contains a number of `WANT_` macros that select functionality, e.g., `WANT_STDIO` (file I/O), `WANT_FLOAT32`/`WANT_FLOAT64` (floating point), `WANT_INT64` (64 bit integers on 32 bit platforms), `WANT_GMP` (use GMP for `Integer`), `WANT_DIR`, `WANT_TIME`, `WANT_SOCKET`, etc. It also defines how to do platform specific things, such as finding the first set bit. Platform specific code goes in `extra.c` in the same directory.

### Using GMP for `Integer`
By default, `Integer` uses imath, which is included. To use GMP instead, uncomment the first few lines in the `Makefile`, and adjust where the C compiler finds GMP. To switch between using and not using GMP you need to do `make clean`.

The combinator file format is described in [`COMB.md`](../COMB.md).

---

# Platforms

MicroHs needs a C compiler (C99 or so) and not much else. The runtime works on 32 and 64 bit platforms, both little- and big-endian. The cell size is two words, so 8 bytes on 32 bit platforms (12 bytes if `Double` is enabled) and 16 bytes on 64 bit platforms.

The configurations in `src/runtime/` are:

| Directory  | Platform | Notes |
| ---------- | -------- | ----- |
| `unix`     | Linux, macOS, BSD, and other Unix-like systems | Full functionality. Tested with `gcc`, `clang`, and `tcc`. |
| `windows`  | Windows with MSVC (`cl`) | Build with `nmake -f Makefile.windows`.  32 and 64 bit. |
| `mingw`    | Windows with MinGW | |
| `micro`    | Generic micro-controller | No file system; the program is read from memory. |
| `stm32f4`  | STM32F4 micro-controllers | Example in `boards/STM32F407G-DISC1`. |
| `esp32`    | ESP32 micro-controllers | Example in `boards/ESP32-C3-DevKitM-1`. |
| `c64`      | Commodore 64, using `cc65` | Example in `boards/C64`.  Very limited. |

Also:
* **JavaScript/WASM**: using emscripten with the `emscripten` target. There is also a [version of MicroHs running in the browser](https://augustss.github.io/MicroHs/web-mhs/).
* The CI tests Linux (64 bit, and 32 bit x86), macOS, Windows (MSVC), and emscripten.

Requirements:
* Building and running `mhs`: a C compiler and `make` (`nmake` on Windows).  No Haskell compiler is needed.
* `mcabal` downloading packages: `curl` (or `wget`), `tar`, and `git` for `--git`.
* On small platforms, the compiler runs on a host and only the runtime and compiled program run on the target. You can turn off functionality (e.g., `WANT_STDIO`, `WANT_FLOAT64`) to make the runtime smaller.

---

# Contributing

Contributions are very welcome!  Bug reports, missing library functions, ported packages, and compiler improvements are all appreciated. Open an issue or a pull request at [github.com/augustss/MicroHs](https://github.com/augustss/MicroHs). See also the [AI policy](../AI-POLICY.md).

## Building
* `make` builds `bin/mhs`, `bin/cpphs`, and `bin/mcabal` from the C files in `generated/`.
* `make bin/gmhs` builds the compiler with GHC, which is convenient while developing.
* `make newmhs` (or `make newmhsz`, which compresses the combinators) rebuilds `bin/mhs` and `generated/mhs.c` to include changes to the compiler.
* `make bootstrap` checks that the compiler can still compile itself.

## Libraries
The libraries live in the `lib/` directory. Adding missing functions, instances, and types from the Report or from GHC's `base` is a welcome contribution. So are improvements to the bundled versions of packages that are heavily GHC specific, such as `array`, `bytestring`, and `text`.

## Tests
The test suite is in the `tests/` directory. To run it, do
* `make runtest` to use the GHC compiled compiler
* `make runtestmhs` to use the MicroHs compiled compiler
* `make runtestemscripten` to use the MicroHs compiled compiler targeting JavaScript
* `make everytest` to run everything, including bootstrapping

To add a test, create `MyTest.hs` and `MyTest.ref`, where the latter contains the expected output, and add a line `MyTest` to `tests/all.tests`. The file explains how to give input, expected error output, and compiler flags. Tests that check compiler error messages go in `tests/err.tests` in the same way, with the expected message in the `.ref` file.

## Adding a primitive
To add a new primitive:
* in `src/runtime/eval.c`
  - add a variant to `enum node_tag`
  - add an entry to the `primops` table (mapping the name of the primitive to the tag)
  - add a `case` in `printrec`
  - implement the primitive by adding a `case` in `evali` (use the other primitives as a guide)
* in `src/MicroHs/Translate.hs` add an entry to `primTable`, to make the primitive available in interactive mode
* in `ghc/PrimTable.hs` and `hugs/PrimTable.hs` add an entry to `primOps`, for the GHC and Hugs compiled interactive mode

Then the primitive can be used with `_primitive "myPrimitive"`.

## Thank you
A big thanks goes to the people who have contributed to MicroHs: @amigalemming, @arossato, @benclifford, @claudeha, @dmjio, gay@disroot.org, @gergoerdi, @ikervagyok, @jmaessen, @juhp, @kolmodin, @konsumlamm, @liolin, @lortabac, @MaximilianAlgehed, @meooow25, @philderbeast, @rewbert, @sol, @Superstar64, @thielema, @thimc, @thma, @treeowl, @TristanCacqueray, @tvmaaren, @yobson.
