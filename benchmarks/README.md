Simple benchmarking machinery to compare how a set of benchmarks performs when changes have been made to the runtime.

Run `make build` to build the benchmarks. Run `make bench` to run them and collect their results. The results are written to `results/`, into a time-stamped JSON file.

Each benchmark's executable is run 5 times by default (`make bench RUNS=10` to override). The report keeps every sample plus the min and median for the wall-clock fields (`total_time_secs`, `gc_time_secs`). The actuall wall-clock times can be a bit noise, but other things like reductions and allocations numbers appear to be deterministic.

## Comparing versions

`compare.sh` compares the benchmarks between versions of MicroHs, e.g., a
release and the working tree:

    benchmarks/compare.sh v0.16.9.0 .
    make compare REVS="v0.16.9.0 ." RUNS=10      # the same, from benchmarks/

Each argument is a git revision (a tag, a branch, or a commit), or `.` for
the working tree. For each one the script exports the revision to
`build/src-HASH`, builds its `bin/mhs` with `make bin/mhs`, and compiles the
benchmarks of the working tree with it, with the libraries and the runtime
of that version. With `-s` it builds the compiler from the sources first,
to include compiler changes that are not in `generated/mhs.c` yet.

It checks that every version prints the same output, and then runs each
benchmark `RUNS` times (`-n`, default 5) for every version, interleaving
the versions, so that a change in the load of the machine affects them
alike. It collects:
- the wall clock time, the maximum RSS, and (on macOS) the instructions
  retired, from `/usr/bin/time`;
- the GC time, reductions, cells allocated, GCs, and max cells used, from
  the `+RTS -v` output.

All the samples are written to `results/compare-TIME.csv`. A table with the
median of each value for every benchmark and version, the change from the
first version, and the geometric mean of the changes, is written to
`results/compare-TIME.md` and printed. Instructions and reductions are
deterministic, so they show small changes that the wall clock time does
not.

## Files

- `compare.sh` - compares versions, see above
- `Util/` - contains drivers and helpers
  - `BenchmarkHarness.hs` -
  - `Json.hs` - Defines a datatype for JSON, and a renderer.
  - `RunStats.hs` - Defines a datatype to represent the output from `+RTS -v -RTS`, and implements a parser to construct it from said output.
- `Loads/` - contains the actual programs that will be executed
  - `ExprEval.hs` - Generates and evaluates a tree-shaped AST
  - `FibInteger.hs` - Double-call fibonacco with `Integer`s.
  - `FibInt.hs` -  Double-call fibonacci with `Int`, to compare with the `Integer` version.
  - `FibTail.hs` - Fibonacci with tail-recursion, to see if it is optimized.
  - `ForkJoin.hs` - Forks some green threads and joins them, to exercise green thread performance.
  - `ListFusion.hs` - Builds a list via `[1..n]`, filters, maps, and sums it, to check how well producer/transformer/consumer list fusion (deforestation) works. (spoiler, there is no deforestation in mhs).
  - `MergeSort.hs` - Classic mergesort of lists of numbers.
  - `NFib.hs` - Counts the number of calls. I include this as it is a 'classic' benchmark.
  - `NumericIntegration.hs` - Strict Double accumulation in a tight loop.
  - `PrimeSieve.hs` - Prime counter, using nested lazy list filters.
  - `TightIntLoop.hs` - Int accumulator, tail recursive.
