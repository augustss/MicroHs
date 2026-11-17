# Combinator file format

The compiler outputs a program as a combinator graph in a simple text format. The same format is used by the runtime for serializing any value, e.g., by `hSerialize` and `hDeserialize` in `System.IO.Serialize`. Since the code is a graph rather than a tree, the format can express sharing and cycles.

A file has three parts:
* a header line with the format version, e.g., `v9.1`, which must match the runtime's version exactly
* a line with the number of labels used, which the runtime uses to size its label table
* the graph, followed by an end marker `}`

## Producing combinator files

* `mhs -oFILE.comb` writes a combinator file. With no `-o` flag the output goes to `out.comb`.
* `mhs -oFILE.c` writes the combinator file as a C array of bytes. Compiling it together with the runtime gives an executable.
* With `-z` the combinator code is compressed (LZMA), and with `-b64` it is Base64 encoded.
* `mhseval +RTS -rFILE -RTS` runs a combinator file.
* `+RTS -oFILE -RTS` makes a program write itself, after loading and a garbage collection, as a combinator file.

When reading, the runtime looks at the first character: `v` (from the version) means plain text, and `z` means that the rest is compressed. Anything else is assumed to be Base64 encoded.

## The graph
The graph is written in postfix form. Every item pushes an expression on a stack, except `@`, which pops a function and an argument and pushes the application of the function to the argument. So an application `f a` is written by writing `f`, then `a`, and then `@`. Items are separated by white space when needed.

| Item | Meaning |
| ---- | ------- |
| `@` | application |
| `:label` | give the label `label` (a number) to the expression on top of the stack |
| `_label` | reference to the expression with label `label` |
| `[n]` | an array of the `n` topmost expressions, so an array is written `e1 ... en [n]` |
| `}` | end of the graph |

A label can be referenced before (textually) it is defined.

## Atoms
Atoms are distinguished by the first (or first two) characters. An atom ends at the next space or newline.

| Atom | Meaning |
| ---- | ------- |
| `#123` | `Int` (or `Word`, `Char`, etc.) |
| `##123` | `Int64` (or `Word64`) |
| `&1.5` | `Double` |
| `&&1.5` | `Float` |
| `%123"` | `Integer`, terminated by `"` |
| `"chars"` | a short `ByteString` (see the escape convention below) |
| `$n bytes` | a `ByteString` given by its length `n`, a space, and then `n` raw bytes |
| `!"chars"` | a tick, for profiling with `-T` |
| `^name` | a foreign function imported with the FFI |
| `;name` | a C function pointer; only `;0` and `;closeb` are allowed |
| `C_k_n` | constructor number `k` with `n` fields |
| `D_m` | a `case` with an arm for each of the `m` constructors |
| `E_m` | a sparse `case` with `m` arms |
| `A_k_n` | the tag for an arm of a sparse `case`, for constructor `k` with `n` fields |
| anything else | a combinator or primitive, e.g., `S`, `C'B`, `+`, `IO.>>=`, or `fromUTF8` |

The data type atoms (`C_k_n`, `D_m`, `E_m`, and `A_k_n`) are described in [Data type encoding](MicroHs.md#data-type-encoding), and the combinators in [Combinators](MicroHs.md#combinators). The runtime also has the atoms `L_m` and `M_m` for the markers that `case` evaluation pushes on the stack, but they do not occur in files written by the compiler.

### Strings
A Haskell `String` literal with at least two characters is written as `fromUTF8` applied to its UTF-8 encoding as a `ByteString`, e.g., `fromUTF8 "hello" @`. A shorter string is written as a list, and a `Char` literal as its code, e.g., `#120` for `'x'`. A `ByteString` of more than 100 bytes uses the `$` form.

Inside quotes, the bytes are encoded so that most printable ASCII takes one byte and other bytes take two. Use the first rule that applies:

| Byte `c` | Encoding |
| -------- | -------- |
| `c < 0x20` | `^` followed by `c+0x20` |
| `"`, `^`, `\|`, `\` | `\` followed by `c` |
| `c < 0x7f` | `c` |
| `c == 0x7f` | `\?` |
| `c < 0xa0` | `^` followed by `c-0x40` |
| `c < 0xff` | `\|` followed by `c-0x80` |
| `c == 0xff` | `\_` |

## Example
This module
```
module M where
import qualified Prelude()

f :: a -> a
f x = x

g :: a -> (a, a)
g x = (f x, f x)

main = (f, g)
```

produces
```
v9.1
3
A C_0_2 _0 @_1 @:2 @
A S' C_0_2 @_0 @_0 @:1 @
A I :0 @
_2 @@@ }
```
- The format version is 9.1, and the file uses 3 labels.
- The label 2 is defined as `C_0_2 _0 _1`, i.e., the pair `(f, g)`.  It is the first argument of `A`, which throws it away.
- The label 1 is defined as `S' C_0_2 _0 _0`, i.e., `g`. `A` throws this away too.
- The label 0 is defined as `I`, i.e., `f`. `A` throws this away too.
- Finally, the reference `_2` is the value of the whole graph.

So each top level definition `e` with label `l` becomes `A e :l @`, and the main expression is last.

## Human readable format
There is an alternative, more readable, format. It is used by `cprint` and `cuprint` from `Mhs.Print`, and when the runtime prints the program after loading it (with `+RTS -v -v -v -RTS`). It uses the same atoms, but
* `(f a)` for application
* `:label e` for definition
* `[n] e1 ... en` for arrays

This format cannot be read back.
