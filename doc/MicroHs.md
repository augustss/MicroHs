# MicroHs

MicroHs (`mhs`) is a compiler for an extended subset of Haskell. It compiles Haskell to combinators, which are then executed by a small runtime system written in C. The compiler is written in MicroHs and can compile itself. All you need to build and run MicroHs is a C compiler.

Contents:
* [Language](#language)
* [Compiler](#compiler)
* [Runtime flags](#runtime-flags)
* [Packages](#packages)
* [Implementation](#implementation)
* [Platforms](#platforms)
* [Contributing](#contributing)

---

# Language

The language is an extended subset of [Haskell 2010](https://www.haskell.org/onlinereport/haskell2010/).

Differences from Haskell 2010 and GHC:
* All supported extensions are always enabled. The only exception is `CPP`, which must be turned on with `{-# LANGUAGE CPP #-}` or the `-XCPP` flag. Other `LANGUAGE` pragmas are ignored, so a pragma for an unsupported extension (e.g., `StrictData`), or one that turns something off (e.g., `NoImplicitPrelude`), has no effect.
* Kind variables need an explicit `forall`.
* Data type contexts are not supported.
* `main` in the top module given to `mhs` is the program entry point, whatever the module is called.
* Many things that should be errors (but are mostly harmless) are not reported.
* Source code and text file I/O always use UTF-8.
* `BangPatterns` are parsed, but only have an effect in a top level `let`/`where`.
* `Int8`, `Int16`, and `Int32` arithmetic raises an exception on overflow instead of wrapping. `Int` and `Int64` wrap, as in GHC, unless the runtime is compiled with `-DWANT_OVERFLOW=1`.
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
* [DeriveGeneric](https://downloads.haskell.org/ghc/latest/docs/users_guide/exts/generics.html#extension-DeriveGeneric)
* [DeriveLift](https://downloads.haskell.org/ghc/latest/docs/users_guide/exts/deriving_extra.html#extension-DeriveLift)
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
* [ExtraCommas](#extra-commas)
* [ExtraInstanceBindings](#extra-instance-bindings)
* [FlexibleContexts](https://downloads.haskell.org/ghc/latest/docs/users_guide/exts/flexible_contexts.html#extension-FlexibleContexts)
* [FlexibleInstances](https://downloads.haskell.org/ghc/latest/docs/users_guide/exts/instances.html#extension-FlexibleInstances)
* [ForeignFunctionInterface](https://downloads.haskell.org/ghc/latest/docs/users_guide/exts/ffi.html#extension-ForeignFunctionInterface)
* [FunctionalDependencies](https://downloads.haskell.org/ghc/latest/docs/users_guide/exts/functional_dependencies.html#extension-FunctionalDependencies)
* [GADTs](https://downloads.haskell.org/ghc/latest/docs/users_guide/exts/gadt.html#extension-GADTs)
* [GADTSyntax](https://downloads.haskell.org/ghc/latest/docs/users_guide/exts/gadt_syntax.html#extension-GADTSyntax)
* [GeneralisedNewtypeDeriving](https://downloads.haskell.org/ghc/latest/docs/users_guide/exts/newtype_deriving.html#extension-GeneralisedNewtypeDeriving)
* [HexFloatLiterals](https://downloads.haskell.org/ghc/latest/docs/users_guide/exts/hex_float_literals.html#extension-HexFloatLiterals)
* [ImportQualifiedPost](https://downloads.haskell.org/ghc/latest/docs/users_guide/exts/import_qualified_post.html#extension-ImportQualifiedPost)
* [ImportShadowing](#import-shadowing)
* [IncoherentInstances](https://downloads.haskell.org/ghc/latest/docs/users_guide/exts/instances.html#extension-IncoherentInstances)
* [InstanceSigs](https://downloads.haskell.org/ghc/latest/docs/users_guide/exts/instances.html#extension-InstanceSigs)
* [KindSignatures](https://downloads.haskell.org/ghc/latest/docs/users_guide/exts/kind_signatures.html#extension-KindSignatures)
* [LambdaCase](https://downloads.haskell.org/ghc/latest/docs/users_guide/exts/lambda_case.html#extension-LambdaCase)
* [LiberalTypeSynonyms](https://downloads.haskell.org/ghc/latest/docs/users_guide/exts/liberal_type_synonyms.html#extension-LiberalTypeSynonyms)
* [MagicHash](https://downloads.haskell.org/ghc/latest/docs/users_guide/exts/magic_hash.html#extension-MagicHash)
* [MonomorphismRestriction](https://downloads.haskell.org/ghc/latest/docs/users_guide/exts/monomorphism.html#extension-MonomorphismRestriction)
* [MultiClassInstances](#multi-class-instances)
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
* [PolymorphicComponents](https://downloads.haskell.org/ghc/latest/docs/users_guide/exts/rank_polymorphism.html#extension-RankNTypes) (an old name for `RankNTypes`)
* [PostfixOperators](https://downloads.haskell.org/ghc/latest/docs/users_guide/exts/rebindable_syntax.html#extension-PostfixOperators)
* [QualifiedDo](https://downloads.haskell.org/ghc/latest/docs/users_guide/exts/qualified_do.html#extension-QualifiedDo)
* [QualifiedStrings](#qualified-strings)
* [QuantifiedConstraints](https://downloads.haskell.org/ghc/latest/docs/users_guide/exts/quantified_constraints.html#extension-QuantifiedConstraints) (only in superclass contexts)
* [RankNTypes](https://downloads.haskell.org/ghc/latest/docs/users_guide/exts/rank_polymorphism.html#extension-RankNTypes)
* [RecordWildCards](https://downloads.haskell.org/ghc/latest/docs/users_guide/exts/record_wildcards.html#extension-RecordWildCards)
* [RecursiveDo](https://downloads.haskell.org/ghc/latest/docs/users_guide/exts/recursive_do.html#extension-RecursiveDo) (without optimal `mdo` desugaring)
* [RequiredTypeArguments](https://downloads.haskell.org/ghc/latest/docs/users_guide/exts/required_type_arguments.html#extension-RequiredTypeArguments)
* [ScopedTypeVariables](https://downloads.haskell.org/ghc/latest/docs/users_guide/exts/scoped_type_variables.html#extension-ScopedTypeVariables)
* [StandaloneDeriving](https://downloads.haskell.org/ghc/latest/docs/users_guide/exts/standalone_deriving.html#extension-StandaloneDeriving)
* [StandaloneKindSignatures](https://downloads.haskell.org/ghc/latest/docs/users_guide/exts/poly_kinds.html#extension-StandaloneKindSignatures)
* [StringInterpolation](#string-interpolation)
* [TupleSections](https://downloads.haskell.org/ghc/latest/docs/users_guide/exts/tuple_sections.html#extension-TupleSections) (only pairs right now)
* [TypeApplications](https://downloads.haskell.org/ghc/latest/docs/users_guide/exts/type_applications.html#extension-TypeApplications)
* [TypeLits](#type-level-literals)
* [TypeOperators](https://downloads.haskell.org/ghc/latest/docs/users_guide/exts/type_operators.html#extension-TypeOperators)
* [TypeSynonymInstances](https://downloads.haskell.org/ghc/latest/docs/users_guide/exts/instances.html#extension-TypeSynonymInstances)
* [UndecidableInstances](https://downloads.haskell.org/ghc/latest/docs/users_guide/exts/instances.html#extension-UndecidableInstances)
* [UndecidableSuperClasses](https://downloads.haskell.org/ghc/latest/docs/users_guide/exts/undecidable_super_classes.html#extension-UndecidableSuperClasses)
* [UnicodeSyntax](https://downloads.haskell.org/ghc/latest/docs/users_guide/exts/unicode_syntax.html#extension-UnicodeSyntax)
* [ViewPatterns](https://downloads.haskell.org/ghc/latest/docs/users_guide/exts/view_patterns.html#extension-ViewPatterns)

### Deriving

Deriving is supported for `Bounded`, `Data`, `Enum`, `Eq`, `Foldable`, `Functor`, `Generic`, `Generic1`, `Ix`, `Lift`, `Ord`, `Read`, `Show`, and `Traversable`.

`Typeable` is automatic: every `data` type, `newtype`, and class gets a `Typeable` instance. This needs the class `Data.Typeable.Typeable`, which is known in any module that imports the `Prelude`. To avoid the instance for a `data` type or `newtype`, e.g., in a module that does not import the `Prelude`, add an empty `deriving ()` clause. This turns off all deriving for that type, including the `HasField` instances for its record fields, so any other deriving clauses on the same declaration are ignored. There is no way to avoid the instance for a class.

### Extra commas

A comma separated list can have an extra comma at the beginning or at the end. This makes it easier to add, remove, and reorder items, and to make items conditional with CPP. For example:
```haskell
module M(
    Color(Red, Green,),
    main,
  ) where

data Color = Red | Green
  deriving (
    Eq,
    Show,
  )

colors :: [Color]
colors = [
    Red,
    Green,
  ]
```
A leading or trailing comma is allowed in list literals and list comprehensions, export and import lists (and the lists of constructors and methods in them), deriving and `default` clauses, record declarations, record construction and update, guards, and functional dependencies. Only a trailing comma is allowed in a list of names before `::` and in a fixity declaration, e.g., `x, y, :: Int`. Tuples cannot have extra commas, since that would conflict with tuple sections.

GHC will also get this extension; see the [accepted proposal](https://github.com/ghc-proposals/ghc-proposals/blob/master/proposals/0748-extra-commas.rst).

### Extra instance bindings

An instance declaration can have a second `where` with bindings that are not methods. These bindings can be used in the methods, and they can use the type variables and the context of the instance, but they are not visible outside the instance. This extension is MicroHs only. For example:
```haskell
instance Show a => Show (Tree a) where
    showsPrec _ t = go 0 t
  where
    go :: Int -> Tree a -> ShowS
    go n (Leaf x)   = indent n . shows x . showString "\n"
    go n (Node l r) = indent n . showString "Node\n" . go (n+2) l . go (n+2) r
    indent n = showString (replicate n ' ')
```
The second `where` must be indented less than the methods, so that it ends the block of methods.

### Import shadowing

A top level definition in a module shadows an imported definition with the same name. In Haskell 2010, using such a name is an error, since it is ambiguous. The imported definition can still be used with a qualified name:
```haskell
import Data.List

insert :: Int -> [Int] -> [Int]     -- shadows Data.List.insert
insert x xs = x : xs

test = (insert 1 [2, 3], Data.List.insert 2 [1, 3])
```

### Multi-class instances

An instance declaration can define instances of several classes at once, by giving a tuple of class constraints instead of a single one. The methods of all the classes go in the same `where`, and the instance context applies to all of them. This extension is MicroHs only. For example:
```haskell
data Color = Red | Green

instance (Eq Color, Show Color) where
  Red   == Red   = True
  Green == Green = True
  _     == _     = False
  show Red   = "Red"
  show Green = "Green"

class Describe a where
  describe :: a -> String
  describe _ = "something"

class Name a where
  name :: a -> String
  name _ = "nameless"

instance Show a => (Describe [a], Name [a]) where
  describe xs = "a list of length " ++ show (length xs)
  name = show
```
The classes can also be given by a constraint synonym. An instance declaration without a `where` gives instances where all methods have their default definitions:
```haskell
type Both a = (Describe a, Name a)

instance Both Bool
```

### Qualified strings

A string literal can be qualified with a module name, `M."..."`. This means `M.fromString "..."`, using whatever `fromString` is in scope as `M.fromString`. It gives a lightweight way to have string literals of different types without turning on `OverloadedStrings` for the whole module.

### String interpolation

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

### Type-level literals

Natural numbers and strings can be used as types, with the kinds `Nat` and `Symbol`. The classes `KnownNat` and `KnownSymbol`, with the methods `natVal` and `symbolVal`, turn them into values. They are all exported from `Data.TypeLits`, which in GHC is called `GHC.TypeLits`. For example:
```haskell
import Data.TypeLits
import Data.Proxy

newtype Vec (n :: Nat) = Vec [Int]

vlen :: forall n . KnownNat n => Vec n -> Integer
vlen _ = natVal (Proxy :: Proxy n)      -- vlen (Vec [] :: Vec 3) is 3
```
In GHC, type-level literals are part of [DataKinds](https://downloads.haskell.org/ghc/latest/docs/users_guide/exts/data_kinds.html#extension-DataKinds) (see [type-level literals](https://downloads.haskell.org/ghc/latest/docs/users_guide/exts/type_literals.html)). MicroHs only has the literals; it cannot use data types as kinds, and there are no type-level operations on numbers or strings.

## Records

MicroHs implements the overloaded record extensions. A field `a` of a record `r` can be accessed as `r.a`, as well as with the usual `a r`. The former is overloaded and works for any record type with a field `a`, whereas the latter uses the usual monomorphic field selector. A field update has the usual syntax, `r{ a = e }`, but it is also overloaded, so it can update the field `a` of any record type. The type classes `HasField` and `SetField` capture this: `HasField "name" rec ty` means that the record type `rec` has a field `name` of type `ty`, which can be extracted with `getField`, and `SetField "name" rec ty` means that the field can be set with `setField`.

Record updates can also update nested fields, e.g., `r{ a.b.c = e }`. This does not easily work in GHC, since GHC does not fully implement `OverloadedRecordUpdate`. When GHC decides how to do it, MicroHs will follow suit.

Record updates cannot change the type of polymorphic fields.

## Libraries

The `Prelude` contains the functions from the Haskell Report and a few extensions. The `base` package contains most of what GHC has in `base`.

To avoid the `Prelude` completely (which is needed in parts of `base` to avoid circular imports), use `import qualified Prelude()`. The compiler then does not even open `Prelude.hs`, and it also skips the automatic qualified import of `Mhs.Builtin`. Note that the `NoImplicitPrelude` pragma has no effect.

### MicroHs specific modules

Besides the usual modules, `base` has some modules that are specific to MicroHs:
* `Data.String.Interpolate` and `Data.String.Interpolate.Basic`, for [string interpolation](#string-interpolation).
* `Data.TypeLits`, for [type-level literals](#type-level-literals).
* `Mhs.Builtin` exports what the compiler refers to in the code it generates, e.g., in derived instances. It is imported automatically, qualified with a name that cannot be written in a program.
* `Mhs.Print` has `cuprint` and `cprint`, which print the combinator graph of a value (see [Debugging](#debugging)).
* `System.Compress` compresses and decompresses strings of bytes, e.g., `compressLZMA` and `decompressLZMA`.
* `System.IO.MD5` computes MD5 checksums of strings, files, and handles.
* `System.IO.Serialize` writes and reads values, see [below](#serialization).
* `System.IO.StringHandle` makes a `Handle` that reads from a string (`stringToHandle`), or collects what is written to a handle in a string (`handleWriteToString`). There are also `ByteString` versions.
* `System.IO.TimeMilli` gives the time in milliseconds or microseconds.
* `System.IO.Transducers` adds a layer to a `Handle` that transforms what is read or written, e.g., compression (`addLZMA`), Base64 encoding (`addBase64`), UTF-8 encoding (`addUTF8`), or buffering (`addBuffer`).
* `System.RTS` gives the number of reductions and allocated cells so far (`getStats`), and runs a garbage collection (`gc`).

### Serialization

`System.IO.Serialize` can write any value to a handle and read it back, even functions and values with sharing or cycles. The exceptions are values that refer to C data, such as `Ptr`, `FunPtr`, and file handles (except `stdin`, `stdout`, and `stderr`), and `MVar`s, `ThreadId`s, and weak pointers. The value is written in the [combinator file format](COMB.md), so it can only be read by a program with the same version of the runtime.

Use `hSerializeDescr` and `hDeserializeDescr` when you can, since they are safer. They need a `Data` instance for the type. `hSerializeDescr` writes a checksum of the complete type (all constructors and the types of their fields, recursively) before the value, and `hDeserializeDescr` returns `Nothing` if it does not match the type that it is expected to read. Functions have a `Data` instance too, and the checksum of a function type includes its argument and result types, so this also works for functions and for data types with function fields:
```haskell
import Data.Data
import System.IO
import System.IO.Serialize

data Shape = Circle Double | Rect Double Double
  deriving (Show, Data)

main :: IO ()
main = do
  withBinaryFile "shapes.ser" WriteMode $ \ h -> hSerializeDescr h [Circle 1, Rect 2 3]
  r <- withBinaryFile "shapes.ser" ReadMode hDeserializeDescr
  print (r :: Maybe [Shape])      -- Just [Circle 1.0,Rect 2.0 3.0]
```
`hSerialize` and `hDeserialize` do no check at all, and reading a value as the wrong type will probably crash the program. Use them only for values whose type has no `Data` instance. There are also `writeSerialized` and `readSerialized`, which work on files, and `writeSerializedCompressed`, which compresses the data (`readSerialized` handles both).

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

MicroHs supports `foreign import ccall`, `foreign import capi`, `foreign export ccall`, `foreign import javascript`, and `foreign export javascript` (the last two for the emscripten target, see [JavaScript](#javascript)). When running a program directly (using `-r`), in the interactive system, or when generating a `.comb` file, only the C functions built into the runtime (plus those from embedded packages) can be called. When generating a `.c` file or an executable, any C function can be called.

As in GHC, the import string can name header files and use `&` to import the address of a C object. In addition, MicroHs has a non-standard form, `"value EXPR"`, which imports the value of an arbitrary C expression. Inside `EXPR`, `$0`, `$1`, … (up to `$9`) are replaced by the function's arguments, numbered from 0 as in a JavaScript import. For example:
```haskell
foreign import capi "value EINTR" eINTR :: Errno
foreign import ccall "value ($0 + $1) * 2" addTwice :: CInt -> CInt -> IO CInt
```
The expression is pasted verbatim into the generated C code, so it must be a valid C expression for the argument and result types. A type without `IO` gives a pure value.

Some of the FFI is still missing compared to GHC. In particular, `foreign import ccall "wrapper"` and `foreign import ccall "dynamic"` are not implemented.

### Calling Haskell from C

A function exported with `foreign export ccall` can be called from C. Compile the module with `-c`, which leaves out the C `main` of the runtime, so the module does not need a `main` either. `mhs` also writes the header file `M_stub.h` (for module `M`, next to the output file) with prototypes for `mhs_init` and the exported functions. The C code must call `mhs_init()` once, before it calls any exported function. For example, with
```haskell
module ForExp where

foreign export ccall funcName :: Int -> IO Int
foreign export ccall "other" (*) :: Int -> Int -> Int    -- exported as other

funcName :: Int -> IO Int
funcName x = do
  putStrLn "funcName called"
  pure (x + 1)
```
and the C program `main.c`
```c
#include <stdio.h>
#include "ForExp_stub.h"

int main(void) {
  mhs_init();
  printf("%d %d\n", (int)funcName(5), (int)other(6, 7));
  return 0;
}
```
the Haskell code can be made into a shared library:
```
mhs -c -optc --shared -optc -fPIC ForExp -olibforexp.so
cc main.c -L. -lforexp -o main
```
On Linux, also add `-Wl,-rpath,'$ORIGIN'` to the `cc` command (or set `LD_LIBRARY_PATH`), so that the library is found when the program runs.

Alternatively, `mhs -c ForExp -oForExpHs.c` writes the C code for the Haskell part. Compile it together with the runtime, `src/runtime/eval.c`, and the C program, with the directories `src/runtime` and `src/runtime/unix` (or another platform) as include directories.

### JavaScript

With the emscripten targets (`-temscripten`, `-tbrowser`, ...) Haskell code can call JavaScript,
hold JavaScript values, and be called back from JavaScript.
The module `GHC.Wasm.Prim` follows GHC's WebAssembly backend, so code written for it needs few changes.
See `tests/JSVal.hs` and `tests/JSCallback.hs` for examples.

#### Calling JavaScript
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

#### JavaScript values
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

#### Callbacks: calling Haskell from JavaScript
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
* A callback that returns a `JSVal` it has already freed gives its JavaScript caller an error; the runtime goes on.

`foreign export javascript` makes a Haskell function callable from JavaScript, where its name gets a `_` prefix, as usual with emscripten. Such a function can also have `Bool` arguments and results, which become 0 and 1. As for C, compile the module with `-c`, and call `_mhs_init()` before any exported function. The `testforexpjs` target in `tests/Makefile` shows how to compile such a module with `emcc` and call it from JavaScript.

#### The program and the event loop
Haskell threads keep their state in the heap, so no C stack has to be preserved and emscripten's ASYNCIFY is not needed.
* Whenever all Haskell threads are blocked (`main` included) the runtime returns to the JavaScript event loop;
  a callback, a timer (for `threadDelay`), or a settling Promise resumes the threads.
* While a program has live callbacks the runtime stays alive after `main` returns, so JavaScript events can
  still call them. The program ends when `main` has finished and no callback is left.
* A fatal runtime error, a JavaScript exception from an `unsafe` import, or a WebAssembly trap (also one while `main`
  is still running) stops the runtime.
  In the browser the page lives on, so every later call into Haskell throws an `Error` saying that the runtime
  has stopped, instead of running on a corrupted runtime.

#### Targets
| target | output | notes |
|---|---|---|
| `emscripten` | WebAssembly + JavaScript, for node | |
| `emscripten_js` | JavaScript only (`-sWASM=0`), for node | |
| `browser` | WebAssembly + JavaScript for the browser | no file system; `-Oz -sSUPPORT_LONGJMP=wasm`; 4M cell heap |
| `browser_rts` | as `browser`, but the program is fetched at run time | see below |
| `browser_js` | JavaScript only for the browser | `-O3`; 4M cell heap |

(`emscripten_web`, which also exists, is not for programs: it runs `mhs` itself in a web page, see `web-mhs/`.)

The browser targets use a 4M cell heap (about 60MB) instead of the default 50M, so that garbage collection,
which also releases the JavaScript values referenced from Haskell, happens regularly;
change it with `-optc -DHEAP_CELLS=N`.

The `browser` target compiles with `-Oz -sSUPPORT_LONGJMP=wasm`.
The evaluator uses `setjmp`, and with emscripten's default JavaScript implementation of `longjmp` every call
out of a function that uses `setjmp` goes through a JavaScript trampoline, which makes `-Oz` (which does not
inline) several times slower than `-O3`.
With WebAssembly exception handling (browsers since 2022) `-Oz` is as fast as `-O3` and about 12% smaller compressed.
`browser_js` has no WebAssembly and stays at `-O3`.

#### The browser runtime (`browser_rts`)
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

#### Embedded JavaScript files
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

## Preprocessor

Sadly, many Haskell packages need the C preprocessor. MicroHs uses `cpphs`, and the distribution contains the combinator code for it, so `make` builds `bin/cpphs`. The symbol `__MHS__` is defined when compiling with MicroHs.

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
| `-c` | Leave out the runtime's C `main`, e.g., to build a library (see [Calling Haskell from C](#calling-haskell-from-c)) |
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
| `-ddump-PASS` | Debug: print the program after `PASS`, one of `preproc`, `parse`, `derive`, `expandInst`, `typecheck`, `desugar`, `linked`, `toplevel`, `combinator`, `all` (see [Dump flags](#dump-flags)) |
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

## Profiling

Compiling with `-T` adds a counter to every top level function. Running the program with `+RTS -T -RTS` then prints, when the program exits, how many times each function was called. Functions that were never called are left out. For example, for a module `Fib` whose `main` computes `fib 20`:
```
$ mhs -T Fib -ofib
$ ./fib +RTS -T -RTS
...
Fib.fib                                                           21891
```
Only code compiled with `-T` has counters, so, e.g., functions from installed packages are not counted.

For overall numbers, use `+RTS -v -RTS`, which prints the number of reductions, allocated cells, garbage collections, etc. In interactive mode, `:set +s` shows the time, reductions, and allocated cells for each evaluation.

## Debugging

* `Debug.Trace` has the usual `trace` functions.
* `-ddump-PASS` prints the program after a compiler pass, e.g., `-ddump-combinator` prints the combinator code of each definition (see [Dump flags](#dump-flags)).
* `+RTS -v -v -v -RTS` prints the whole program after it has been loaded, in the readable form of the [combinator file format](COMB.md#human-readable-format).
* `cuprint` from `Mhs.Print` prints the combinator graph of a value as it is at the moment, in the same form. `cprint` first evaluates the value fully and simplifies it, but this is fragile.

---

# Runtime flags

Programs compiled with MicroHs, including `mhs` itself, take runtime flags between `+RTS` and `-RTS`. All other arguments are passed to the running program.

* `-HSIZE` set the heap size to `SIZE` cells. `SIZE` can be suffixed by `k`, `M`, or `G`. The default is `50M`.
* `-KSIZE` set the stack size to `SIZE` entries. `SIZE` can be suffixed by `k`, `M`, or `G`. The default is `300k`.
* `-v` print statistics when the program exits, e.g., the number of reductions, allocated cells, and garbage collections. Can be repeated for more output.
* `-T` print how many times each function was called, for a program compiled with `-T` (see [Profiling](#profiling)).
* `-B` ring the bell on every garbage collection.
* `-rFILE` read the combinators from `FILE` instead of `out.comb` (for `mhseval`).
* `-oFILE` read the program, run a garbage collection, and write the resulting graph to `FILE`.

For example, `./prog +RTS -H1M -v -RTS hello` runs `prog` with a heap of 1M cells and prints statistics at the end, and the program gets the single argument `hello`.

The heap and the stack have a fixed size; they do not grow. A program that runs out of heap stops with the error `heap exhausted`, and one that runs out of stack stops with `stack overflow`. Run it again with a bigger `-H` or `-K`.

---

# Packages

To avoid compiling everything from source every time, modules can be precompiled into packages. A package is a set of modules that are compiled together and then installed in a known place. Packages can depend on other installed packages.

`mhs` needs no extra flags to use installed packages; all installed packages are always visible. When compiling to an executable, only the parts of a package that are actually used are included.

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

## Embedding packages in `mhs`

Normally `mhs` finds packages in the package search path, but packages can also be built into the `mhs` binary itself, e.g., to get a compiler that does not need any installed packages. When compiling the compiler (the module `MicroHs.Main`), the flag `--embed-packages PKG1:PKG2:...` puts the given packages into the binary. They are loaded when `mhs` starts, as if they were installed. For example:
```
mhs -i -imhs -isrc MicroHs.Main --embed-packages base -omhs-base
```
When running a program directly (`-r`), in the interactive mode, or when generating a `.comb` file, a foreign import can only call C functions that are in the `mhs` binary. `--embed-ffis PKG1:PKG2:...` includes the C wrappers for the foreign imports of the given packages, without embedding the packages themselves. `--embed-packages` does this for the packages it embeds.

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
4. Desugaring to a simple lambda calculus (`Desugar`, `Exp`), where constructors and `case` become special combinators (`EncodeData`), see [Data type encoding](#data-type-encoding).
5. Translation to combinators by bracket abstraction, with optimizations (`Abstract`), see [Combinators](#combinators).
6. Serialization of the combinator graph (`ExpPrint`), optionally compressed and turned into a C array (`MakeCArray`), see [Combinator file format](COMB.md).

Other important modules:
* `Main` decodes flags and drives everything.
* `Compile` finds, compiles, and caches modules, and builds packages.
* `CompileCache`, `Package` the compilation cache and the package format.
* `Interactive` the REPL.  It uses `Translate`, which converts combinators directly to runnable values.
* `FFI` generates C wrappers for foreign imports and exports.
* `Builtin`, `SymTab`, `Ident`, `IdentMap`, `Graph` (strongly connected components), `StateIO`, and `Config`.

### Dump flags

The `-ddump-PASS` flags print the program at different stages of compilation, which is useful when working on the compiler. Several flags can be given at once, and `-ddump-all` turns on all of them.

The following flags print each module as it is compiled. Modules that are loaded from the compilation cache or from an installed package are not compiled, so they are not printed.
* `-ddump-preproc` the source text, after preprocessing with `cpphs` or the `-F` preprocessor, if any.
* `-ddump-parse` the syntax tree after parsing. Fixities have not been resolved yet, so all operators are shown as left associative, e.g., `a + b * c` is shown as `(a + b) * c`. Numeric literals are shown with a `%`.
* `-ddump-derive` the module after the class declarations have been expanded and instances have been derived. Each class method has got a default definition, `m$dflt`, and each type has got its derived instances, including the automatic `Typeable` instance and the `HasField` instances for record fields.
* `-ddump-expandInst` as above, and each instance has also been turned into a dictionary, `inst$C@T`. It is the class's dictionary constructor, `C$C`, applied to the superclass dictionaries and the methods.
* `-ddump-typecheck` the module after type checking. All names are fully qualified, and type class constraints have become explicit dictionary arguments.
* `-ddump-desugar` the module after desugaring to a simple lambda calculus. Pattern matching has been turned into the `case` combinators, and constructors into `C_k_n` (see [Data type encoding](#data-type-encoding)).

The following flags print the whole program, including the library, so the output is large.
* `-ddump-combinator` every definition of every module after translation to combinators, in readable form.
* `-ddump-linked` the definitions that are used by `main`, which are the ones that end up in the output. They are shown in the postfix syntax of the [combinator file format](COMB.md), but with the original names.
* `-ddump-toplevel` all definitions, including those from packages embedded with `--embed-packages`, in the same syntax as `-ddump-linked`.

### Bootstrapping

The repository contains the compiler itself compiled to combinators (as C code) in `generated/mhs.c`. This is how `mhs` is built from just the C compiler.

To rebuild the compiler after changing it, use `make newmhs` (or `make newmhsz` to compress). `make bootstrap` recompiles the compiler twice and compares the outputs to make sure the new compiler still works.

MicroHs can also be built from scratch in the sense of [bootstrappable.org](https://bootstrappable.org/), using Hugs (`make bin/hmhs`).  This needs [a slightly patched version of Hugs](https://github.com/augustss/hugs98-plus-Sep2006), to work around undefined behavior with arithmetic overflow.

## Combinators

Bracket abstraction (`Abstract`) removes all lambdas using `S`, `K`, and `I`. A number of rewrites then introduce the other combinators, which give smaller code and fewer reductions. Some rewrites remove combinators altogether, e.g., `S K x` → `I`, `S (K x) (K y)` → `K (x y)`, `S (K x) I` → `x`, `B I x` → `x`, and `Y (K x)` → `x`.

| Combinator | Reduction | Introduced by |
| ---------- | --------- | ------------- |
| `S`   | `S x y z = x z (y z)` | bracket abstraction |
| `K`   | `K x y = x` | bracket abstraction |
| `I`   | `I x = x` | bracket abstraction |
| `B`   | `B x y z = x (y z)` | `S (K x) y` → `B x y` |
| `C`   | `C x y z = x z y` | `S x (K y)` → `C x y` |
| `S'`  | `S' x y z w = x (y w) (z w)` | `S (B x y) z` → `S' x y z` |
| `C'`  | `C' x y z w = x (y w) z` | `C (B x y) z` → `C' x y z` |
| `B'`  | `B' x y z w = x y (z w)` | `B B` → `B'` |
| `C'B` | `C'B x y z w = x z (y w)` | `C' B` → `C'B` |
| `A`   | `A x y = y` | `K I` → `A` |
| `U`   | `U x y = y x` | `C I` → `U` |
| `Z`   | `Z x y z = x y` | `B K` → `Z` |
| `K2`  | `K2 x y z = x` | `Z K` → `K2`, `K (K x)` → `K2 x` |
| `K3`  | `K3 x y z w = x` | `Z K2` → `K3`, `K (K2 x)` → `K3 x` |
| `K4`  | `K4 x y z w v = x` | `Z K3` → `K4`, `K (K3 x)` → `K4 x` |
| `P`   | `P x y z = z x y` | `C (C I x) y` → `P x y`, `B C (C I)` → `P` |
| `R`   | `R x y z = y z x` | `C C` → `R` |
| `O`   | `O x y z w = w x y` | `B (B K) P` → `O` |
| `Y`   | `Y x = x (Y x)` | recursive `let` |

The runtime implements `Y x` by making the redex point to itself, so it builds a cyclic graph. A combinator that returns one of its arguments (e.g., `I`, `K`, and `A`) overwrites the redex with an indirection; the others overwrite it with the result, allocating cells for the new inner applications. The rewrites can increase the arity of an expression, so a few combinators also reduce when they have too few arguments, e.g., `B' x y = B (x y)` and `Z x y = K (x y)`. This keeps the sharing (full laziness) of the original code.

When the program has been loaded, the garbage collector also does some reductions that make the graph smaller, e.g., `I x` → `x` and `K x y` → `x`. A program can ask for this with `performGCWithReduction` from `System.Mem`.

Besides combinators, the code contains literals, the data type combinators described below, and primitive operations such as `+`, `==`, `seq`, and the IO operations. The runtime lists them all in the `primops` table in `src/runtime/eval.c`.

## Data type encoding

Data types are not encoded as functions; the runtime handles constructors and `case` directly. The encoding is generated by `EncodeData`.

### Constructors
Constructor number `k` with `n` fields is the combinator `C_k_n`, where the constructors are numbered from 0 in the order they appear in the `data` declaration. A value is the constructor applied to its fields. For example, with `data Maybe a = Nothing | Just a`, `Nothing` is `C_0_0` and `Just 42` is `C_1_1 42`. A constructor applied to at most `n` arguments is in weak head normal form. A constructor with strict fields is a function that evaluates those fields before building the value, e.g., `\ x y -> seq x (C_0_2 x y)` when the first field is strict. A `newtype` constructor is just `I`.

The runtime knows the encoding of booleans, lists, and tuples:

| Type | Constructors |
| ---- | ------------ |
| `Bool` | `False` = `C_0_0`, `True` = `C_1_0` |
| `[a]` | `[]` = `C_0_0`, `(:)` = `C_1_2` |
| tuples | `()` = `C_0_0`, `(,)` = `C_0_2`, `(,,)` = `C_0_3`, ... |

### Case
A `case` on a type with `m` constructors normally becomes
```
D_a0_..._a{m-1} f0 ... f{m-1} e
```
where `e` is the scrutinee, `ak` is the number of fields of constructor `k`, and the arm `fk` is a function of those fields. Arms that are not in the `case` are filled in with the default alternative, which ignores the fields. For example:
```
case xs of { [] -> a; y:ys -> b }    becomes    D_0_2 a (\ y ys -> b) xs
if c then t else e                   becomes    D_0_0 e t c
```
Mutually recursive local definitions are combined into a tuple `v`, and the `i`th of `n` components is selected with `D_n (\ x1 ... xn -> xi) v`.

The runtime does not need the arities `ak`, since a constructor knows its number of fields. They are there so that the combinator code can be turned back into lambda expressions, e.g., by `Mhs.Reflect`: after bracket abstraction the number of fields cannot be read from an arm (`\ x xs -> x` is just `K`). Below, `D_m` stands for a dense `case` with `m` arms.

If the type has more than 4 constructors and fewer than 3/4 of them have an arm, a sparse `case` is used instead:
```
E_m d A_k1_n1 f1 ... A_km_nm fm e
```
Here `m` is the number of arms, `d` is the default, and each arm `fi` is preceded by `A_ki_ni`, which gives its constructor number and number of fields. The arms are sorted by constructor number.

The scrutinee comes last so that `\ x -> case x of ...` becomes just `D_m f0 ... f{m-1}` after bracket abstraction, without any extra combinators.

### Evaluation
The constructor and `case` combinators take no space in the heap. Their tag, the constructor number, and the number of fields (or arms) are stored in the pointer itself (see [Memory layout](#memory-layout)). There are 11 bits for the constructor number and 8 bits for the number of fields. A `D_m` uses those 19 bits for an index into a table of case arities, where each entry holds `m` and the arities. Equal entries are shared, and the table is never freed.

To reduce `D_m f0 ... f{m-1} e`, the runtime leaves the arms on the stack, pushes a marker, and evaluates `e`. When the evaluation of `e` reaches a constructor `C_k_n` applied to its `n` fields, the marker is right below it on the stack. The runtime then selects the arm `fk` and overwrites the case redex with `fk e1 ... en`, which needs `n-1` new cells (or with an indirection to `fk` when `n` is 0). For `E_m`, the arm is found by binary search on the constructor numbers, and if there is no match the result is `d`. A constructor that is not directly above a marker is a value.

### Class dictionaries

Type classes are translated to dictionary passing, but dictionaries do not use the constructor encoding. A dictionary is a function that applies its argument to the superclass dictionaries and the methods, in the order they are declared. The dictionary constructor for a class `C` with `n` components is
```
C$C = \ x1 ... xn f -> f x1 ... xn
```
and the component number `i` is selected by
```
\ d -> d (\ x1 ... xn -> xi)
```
After bracket abstraction a selector is just a few combinators. For example, `pure`, which is the second of the six components of `Applicative`, is `U (K K4)`.

## Runtime

The runtime system is written in C and is mainly in `src/runtime/eval.c`. It is a graph reduction machine for combinators, with primitive operations for built in types and for executing IO operations. The C code is reasonably portable and has no dependencies beyond a C compiler and (optionally) a C library.

The runtime also contains:
* A mark-scan garbage collector.
* Lightweight threads (for `Control.Concurrent`, `MVar`, and STM), with non-blocking IO where available. Threads are preemptive: the running thread is switched out after a fixed number of reductions, 100000 by default. This can be changed by compiling the runtime with `-DSLICE=N`, e.g., `mhs -optc -DSLICE=10000 ...`.
* Weak pointers with finalizers. When the key of a weak pointer dies, its finalizer runs in a new thread, so output from a finalizer can be interleaved with output from other threads.
* Bignums for `Integer`, using [imath](https://github.com/creachadair/imath) (included) or [GMP](https://gmplib.org/). There is also an implementation in Haskell (see [Implementations of `Integer`](#implementations-of-integer)).
* Serialization and deserialization of arbitrary expressions, preserving sharing and cycles, using the [combinator file format](COMB.md). The only exceptions are C pointers (e.g., file handles) other than `stdin`, `stdout`, and `stderr`.
* LZMA/LZ77 decompression and Base64 decoding, for compressed combinator code.
* Buffered file I/O with UTF-8 encoding.

### Types
Some types are primitive and known by the runtime, e.g., `Int`, `Word`, `Double`, `Float`, `Ptr`, `IO`, and arrays. The function type, `->`, is (of course) also built in. All other types are defined in Haskell, and the compiler encodes them with constructor and `case` combinators. The runtime knows how lists, booleans, and tuples are encoded (see [Data type encoding](#data-type-encoding)).

### Memory layout
Memory is allocated in cells.  Each cell has room for two pointers (two words, typically 16 bytes), so it can represent an application node.  A few bits indicate if the cell is an application or something else. If it is something else, one word is a tag, e.g., an integer or an indirection, and the other word stores the payload, e.g., the integer value.

Cells are aligned, so the two lowest bits of a real pointer are always 0. A pointer whose lowest bits are `10` does not point anywhere; instead it holds a tag, e.g., a combinator, a primitive, or a constructor. Such values take no space in the heap.

There is a bitmap with one bit per cell. Allocating a cell consists of finding the next free cell using the bitmap and marking it as used; in practice whole runs of free cells are claimed at once. The garbage collector clears the bitmap and then marks every reachable cell. There is no explicit scan phase, since that is baked into the allocation. Allocation is fast, assuming the CPU has some kind of find-first-set instruction.

### The evaluator

The evaluator, `evali`, reduces an expression to weak head normal form. It walks down the left spine of applications, pushing each application node on the stack, until it finds something that is not an application, e.g., a combinator. If the stack has enough arguments, the combinator is reduced by overwriting the topmost application node of the redex with the result, so the result is shared. Otherwise the expression is already in weak head normal form, and the evaluator returns. Indirection nodes are followed and short-circuited on the way. A reduction that allocates cells first makes sure that there are enough free cells, and runs a garbage collection if needed. The roots for the garbage collector are the stack, the threads, and some global tables.

There is a single stack. A recursive call of the evaluator uses the part above its caller's part, but recursive calls are avoided where possible, since they also use C stack. A strict primitive, e.g., `+` on `Int`, pushes a marker and evaluates one argument. When the evaluator returns to the marker, it changes the marker and evaluates the other argument, and when it returns to the changed marker, it performs the operation. `case` works in a similar way (see [Data type encoding](#data-type-encoding)).

An IO action is a function that takes a world token and returns a pair of the result and a new world token. The pair is Scott encoded with `P` (`P r w k = k r w`), so `return` is just `P`, and `>>=` is just `C`: `C m f w = m w f`, and when `m w` has been reduced to `P r w'`, this is `f r w'`. `unsafePerformIO` applies the action to a world token, and selects the result with `K`. The primitive IO operations return their results in the same way.

`catch` sets up a handler with `setjmp`, and evaluates the protected action with a recursive call of the evaluator. Throwing an exception does a `longjmp` to the innermost handler, which runs the handler action. An exception that is not caught kills the thread, and the program if it is the main thread.

Each thread has a root node, its IO action applied to a world token, and the scheduler evaluates the root of the thread at the front of the run queue. When the thread's time slice runs out, or the thread blocks, e.g., on an `MVar`, the thread's stack is simply thrown away, and the next thread runs. All reductions have been done in place in the graph, so evaluating the root again later resumes the computation where it left off. Exception handlers are set up again when the evaluation passes the `catch`.

### Configuration
The runtime is configured for each platform by a `config.h` in a subdirectory of `src/runtime/`. It contains a number of `WANT_` macros that select functionality, e.g., `WANT_STDIO` (file I/O), `WANT_FLOAT32`/`WANT_FLOAT64` (floating point), `WANT_INT64` (64 bit integers on 32 bit platforms), `WANT_IMATH` and `WANT_GMP` (which implementation of `Integer` to use), `WANT_DIR`, `WANT_TIME`, `WANT_SOCKET`, etc. It also defines how to do platform specific things, such as finding the first set bit. Platform specific code goes in `extra.c` in the same directory.

Some other macros control how the runtime works. These are set in `config.h`:
* `GCRED` do some reductions during garbage collection (see [Combinators](#combinators)).
* `INTTABLE` share the cells for small integers (from -10 to 255).
* `SANITY` do some sanity checks during evaluation.
* `STACKOVL` check for stack overflow.
* `WANT_KPERF` make `+RTS -v` also print the number of instructions executed, using the CPU's performance counters. This only works on some CPUs, see the comment in `unix/config.h`.

These have defaults, and can be set with `-D`, e.g., `mhs -optc -DMARK_PFQ=0 ...`:
* `BUMP_ALLOC` (default 1) allocate cells by moving a pointer through a run of free cells, instead of searching the bitmap for each cell.
* `MARK_PFQ` (default 16) the number of cells the garbage collector prefetches while marking; 0 turns prefetching off.
* `SLICE` (default 100000) the number of reductions in a thread's time slice.
* `HEAP_CELLS` and `STACK_SIZE` the default heap and stack sizes (see [Runtime flags](#runtime-flags)).

### Implementations of `Integer`
There are three implementations of `Integer`:
* [imath](https://github.com/creachadair/imath), a small bignum library in C, which is included in the runtime. This is the default.
* [GMP](https://gmplib.org/), which must be installed separately. To use it, uncomment the first few lines in the `Makefile`, and adjust where the C compiler finds GMP. To switch between using and not using GMP you need to do `make clean`.
* An implementation in Haskell, in `lib/no-gmp`. It is slower, but the runtime does not need any bignum code. It is used when the runtime has neither imath nor GMP, i.e., when both `WANT_IMATH` and `WANT_GMP` are 0. The `micro` configuration sets them so, and with the `unix` configuration you can compile the runtime with `-DWANT_IMATH=0`.

imath and GMP have the same interface, so the Haskell code in `lib` is the same for both. The Haskell implementation replaces two modules, `Data.Integer_Type` and `Data.Integer.Internal`, which are in `lib/no-gmp`. The library code and the runtime must agree: code compiled for imath or GMP calls C functions that a runtime without them does not have, and it fails with an error like `FFI unknown mpz_cmp`. To compile a program that uses the Haskell implementation, put `lib/no-gmp` before `lib` in the module search path, and compile the runtime without imath, e.g., with `$MHSDIR` being the MicroHs source directory:
```
mhs -i -i. -i$MHSDIR/lib/no-gmp -i$MHSDIR/lib -optc -DWANT_IMATH=0 Prog -oprog
```

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
* `make bin/gmhs` builds the compiler with GHC, which is convenient while developing. The interactive mode only works in a compiler built by MicroHs, though.
* `make newmhs` (or `make newmhsz`, which compresses the combinators) rebuilds `bin/mhs` and `generated/mhs.c` to include changes to the compiler. It uses `bin/gmhs`, so it needs GHC.
* `make bootstrap` checks that the compiler can still compile itself.

### Changing the combinator format
The combinator file format has a version, which must be the same in `src/runtime/eval.c` (`VERSION`) and `src/MicroHs/ExpPrint.hs` (`combVersion`). Change it whenever existing combinator files would no longer work, e.g., when the data type encoding changes, or a primitive is removed or changes meaning. Then old files are rejected, instead of misbehaving.

After such a change all the combinator code in `generated/` must be regenerated, not just the compiler, i.e., also `cpphs.c`, `mcabal.c`, `install.c`, `tester.c`, and the `base` package, `base.pkg`. `make preparedist` does all of this. It needs GHC, a checkout of [MicroCabal](https://github.com/augustss/MicroCabal) next to the MicroHs directory, the `cpphs` sources (it fetches them as a git submodule), and emscripten (for `generated/mhseval.js`).

## Libraries
The libraries live in the `lib/` directory. Adding missing functions, instances, and types from the Report or from GHC's `base` is a welcome contribution. So are improvements to the bundled versions of packages that are heavily GHC specific, such as `array`, `bytestring`, and `text`.

## Tests
The test suite is in the `tests/` directory. To run it, do
* `make runtest` to use the GHC compiled compiler
* `make runtestmhs` to use the MicroHs compiled compiler
* `make runtestemscripten` to use the MicroHs compiled compiler targeting JavaScript
* `make everytest` to run everything, including bootstrapping

To add a test, create `MyTest.hs` and `MyTest.ref`, where the latter contains the expected output, and add a line `MyTest` to `tests/all.tests`. The file explains how to give input, expected error output, and compiler flags. Tests that check compiler error messages go in `tests/err.tests` in the same way, with the expected message in the `.ref` file.

## Measuring performance
* `make timecompile` times the compiler compiling itself, and prints the statistics from `+RTS -v`. Give a better timing command with `TIME`, e.g., `make timecompile TIME='/usr/bin/time -l'` on macOS.
* The `benchmarks/` directory has a set of benchmark programs, see its [README](../benchmarks/README.md).
* The number of reductions and allocated cells from `+RTS -v` are deterministic, but the number of reductions is not a good measure of speed, since different combinators do very different amounts of work. The number of instructions executed is a better measure, and it varies much less than the time. Get it with `/usr/bin/time -l` on macOS, `perf stat` on Linux, or by compiling the runtime with `WANT_KPERF`.
* Wall clock times vary between runs. When comparing two versions, run them alternately, several times.

## Adding a primitive
To add a new primitive, change `src/runtime/eval.c`:
* add a variant to `enum node_tag`, after `T_FIRST_REAL_TAG`
* add an entry to the `primops` table, mapping the name of the primitive to the tag
* implement the primitive by adding a `case` in `evali` (use the other primitives as a guide)

Then the primitive can be used with `_primitive "myPrimitive"`. The interactive mode reads its code the same way as the runtime, so the primitive works there too.

## Thank you
A big thanks goes to the people who have contributed to MicroHs: @amigalemming, @arossato, @benclifford, @claudeha, @dmjio, gay@disroot.org, @gergoerdi, @ikervagyok, @jmaessen, @juhp, @kolmodin, @konsumlamm, @liolin, @lortabac, @MaximilianAlgehed, @meooow25, @philderbeast, @rewbert, @sol, @Superstar64, @thielema, @thimc, @thma, @treeowl, @TristanCacqueray, @tvmaaren, @yobson.
