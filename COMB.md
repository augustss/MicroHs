# Combinator file format

The format is quite simple, there are only a few types of nodes the format:

 * atom
 * application
 * label definition
 * array

If the combinator expression had been a tree we would only need applications
(for interior nodes) and atoms (for leaves).  But since we want to represent
graphs rather than trees we can also give a name to a node and refer to it
(with another atom).  We also have a special encoding for arrays for efficiency.

Before a serialized graph there is a header: file-format version, number of labels used.
After a serialized graph there is an end marker '}'.

To generate a combinator file simply use '-ofile.comb' to mhs.
Using '-ofile.c' will output the string as a C array of bytes.
Additionally, using the '-z' flags to mhs will compress the C byte array with LZMA.
With no '-o' flag the result will be in 'out.comb'

## Atoms
Atoms are distinguished by the first (or first two) characters.  An atom ends at the next white-space or ')'.

 * #digits   INT
 * ##digits  INT64
 * &chars    DOUBLE
 * &&chars   FLOAT
 * %digits"  INTEGER
 * "chars"   STRING (see escape convention)
 * $digits bytes  BYTESTRING
 * !chars    TICK
 * ^chars    FFI name
 * ;chars    C function pointer
 * chars     primitive (combinators, arithmetic etc)

Strings use the following escape conventions (use in order)

 * c < 0x20    encodes as '^' c+0x20
 * c=='"' || c=='^' || c=='|' || c=='\\'   encodes as '\\' c
 * c < 0x7f    encodes as c
 * c==0x7f     encodes as '\\' '?'
 * c<0xa0      encodes as '^' c-0x40
 * c<0xff      encodes as '|' c-0x80
 * c==0xff     encodes as '\\' '_'

Bytestrings are coded with '$' followed by the length in decimal, followed by a space, and then the raw bytes
of the bytestring.

## Application
An application `f a` is encoded bye encoding `f` and `a` and followed by a '@'.

## Definitions
To give a name to a definition `e`, encode `e` and follow by ':label'.
To refer to a definition use '_label'.
Labels may be references before (textually) they are defined.

## Arrays
An n elements array is serialized as 'e1 ... en [n]', where n is the length in decimal.

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
v8.4
3
A P _0 @_1 @:2 @
A S' P @_0 @_0 @:1 @
A I :0 @
_2 @@@ }
```
- Version 8.4, using 3 labels.
- The label 2 is defined as (P _0 _1), and A simply throws away this expression.
- The label 1 is defined as (S' P _0 _0), and A throws this away.
- The label 0 is defined as I, and A throws this away.
- But finally the reference to _2 is kept.

## Human readable format
There is an alternate format used by `cprint` that is more human readable.
It uses the same atoms, but

 * '(f a)' for application
 * ':label e' for definition
 * '[n] e1 ... en' for array
