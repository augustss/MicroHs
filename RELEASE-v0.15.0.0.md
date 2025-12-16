# New in MicroHs 0.15.0.0

## Features
 * New language extensions
   * EmptyDataDeriving
   * NondecreasingIndentation
   * MonoLocalBinds removed
   * MonomorphismRestriction
 * Evaluation stats in interactive system: `set +s`
 * STM (implementation from David Sabel)
 * GHC compatible exception handling (`mask` etc.)
 * `System.Mem.Weak`
 * `Foreign.StablePtr`
 * `out.comb` can be generated compressed and base64 encoded
 * Improved `Data.ByteString
 * FFI wrapper for `eval.c`
 * New `BFILE` stuff: file descriptor, buffering, base64 coding
 * `Double` and `Float` are now different types
 * `Int64`/`Word64` on 32 bit platforms
 * Instruction counts on supported platforms (MacOS&M4, Linux&x86_64)
 * Proper `array` package
 * Unboxed tuple and sum syntax accepted (but no unboxing)
 * Deriving Data, Foldable, Functor, Ix, Read, Traversable

## Performance
 * Evaluation in the interactive system is much faster

## FFI
 * `foreign export ccall` implemented
 * `foreign import javascript` implemented
 * JavaScript version of the MicroHs runtime is part of the distribution: `generated/mhseval.js`
 
## Bug fixes
 * Lots
 
## Packages
 * Many more packages compile, e.g., QuickCheck, filepath
 * See https://docs.google.com/spreadsheets/d/1e0dbUg5uuFKNwgMpwtBnYRldPCYYyBqYfsbyhEjf5bU/edit?usp=sharing
 * Use `make Makefile.packages` to install some of them

## Contributors
 * Ben Clifford <benc@hawaga.org.uk>
 * Heinrich Apfelmus <apfelmus@quantentunnel.de>
 * Henning Thielemann <git@henning-thielemann.de>
 * konsumlamm <konsumlamm@gmail.com>
 * Lorenzo Tabacchini <lortabac@gmx.com>
 * Thomas Mahler <thma32@web.de>
 * Thomas van Maaren <74723259+tvmaaren@users.noreply.github.com>
 * Tristan de Cacqueray <tristan@midirus.com>
