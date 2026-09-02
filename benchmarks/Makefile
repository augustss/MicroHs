# Build and run the benchmark suite.
#
#   make build          -- compile every benchmark + the harness
#   make bench          -- build, then run the harness (writes benchmarks/results/run-<ts>.json)
#   make bench RUNS=10  -- same, but run each benchmark's executable 10 times instead of the default 5
#   make clean          -- remove compiled executables (keeps .hs/.out/results)
#
MHS      = bin/mhs
MHSFLAGS = -ilib -ibenchmarks -CR
RUNS     = 5

BENCHMARKS = NFib TightIntLoop NumericIntegration FibInteger FibInt FibTail PrimesSieve MergeSort ExprEval ForkJoin ListFusion

.PHONY: build bench clean cachelib

# Each benchmark below is a separate mhs invocation, and every one of them
# pulls in the base library. Priming ../.mhscache once up front
# means the -CR in MHSFLAGS lets them all reuse that compiled base instead
# of each recompiling it from scratch.
cachelib:
	cd .. && rm -f .mhscache && $(MHS) -ilib -CW AllOfLib

build: cachelib $(BENCHMARKS:%=%.exe) BenchHarness.exe

%.exe: Loads/%.hs
	cd .. && $(MHS) $(MHSFLAGS) Loads.$* -o benchmarks/$@

BenchHarness.exe: Util/BenchHarness.hs Util/RunStats.hs Util/Json.hs
	cd .. && $(MHS) $(MHSFLAGS) Util.BenchHarness -o benchmarks/BenchHarness.exe

bench: build
	cd .. && benchmarks/BenchHarness.exe $(RUNS)

clean:
	rm -f *.exe
