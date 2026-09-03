Simple benchmarking machinery to compare how a set of benchmarks performs when changes have been made to the runtime.

Run `make build` to build the benchmarks. Run `make bench` to run them and collect their results. The results are written to `results/`, into a time-stamped JSON file.

Each benchmark's executable is run 5 times by default (`make bench RUNS=10` to override). The report keeps every sample plus the min and median for the wall-clock fields (`total_time_secs`, `gc_time_secs`). The actuall wall-clock times can be a bit noise, but other things like reductions and allocations numbers appear to be deterministic.

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
