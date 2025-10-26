# New in MicroHs 0.15.0.0

## Features
 * New language extensions
   * EmptyDataDeriving
   * NondecreasingIndentation
 * Evaluation stats in interactive system: `set +s`
 * STM (implementation from David Sabel)
 * GHC compatible exception handling (`mask` etc.)
 * `System.Mem.Weak`
 * `Foreign.StablePtr`
 * `out.comb` can be generated compressed and base64 encoded
 * FFI wrapper for `eval.c`
 * New `BFILE` stuff: file descriptor, buffering, base64 coding
 * `Double` and `Float` are now different types
 * `Int64`/`Word64` on 32 bit platforms
 * instruction counts on supported platforms (MacOS&M4, Linux&x86_64)
 * Proper `array` package
 * Unboxed tuple and sum syntax accepted (but no unboxing)

## Performance
 * Evaluation in the interactive system is much faster

## FFI
 * `foreign export ccall` implemented
 * `foreign import javascript` implemented
 * JavaScript version of the MicroHs runtime is part of the distribution: `generated/mhseval.js`
 
## Bug fixes
 * Pattern binding with overloaded expression.
 * Windows build
 
## Packages
 * Many more packages compile, e.g., QuickCheck, filepath

## Contributors
 * Ben Clifford <benc@hawaga.org.uk>
 * Heinrich Apfelmus <apfelmus@quantentunnel.de>
 * Henning Thielemann <git@henning-thielemann.de>
 * konsumlamm <konsumlamm@gmail.com>
 * Lorenzo Tabacchini <lortabac@gmx.com>
 * Thomas Mahler <thma32@web.de>
 * Thomas van Maaren <74723259+tvmaaren@users.noreply.github.com>
 * Tristan de Cacqueray <tristan@midirus.com>
