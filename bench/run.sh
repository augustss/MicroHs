#!/bin/sh
# Benchmark harness. Usage: bench/run.sh <eval-binary> [runs]
# Pins to CPU 2, runs micro-benchmarks N times, prints best (min) per-bench time
# and the runtime's reduction-rate / GC stats from a verbose run.
EVAL=${1:-bin/mhseval}
RUNS=${2:-5}
PIN="taskset -c 2"

# Warm up
$PIN $EVAL +RTS -rBench.comb -RTS >/dev/null 2>&1

# Collect best (min) ms per benchmark across RUNS
awkprog='
/nfib32:|tak:|sumTo:|churn:/ {
  name=$1; gsub(/:/,"",name);
  for(i=1;i<=NF;i++) if($i ~ /^\(/){ms=$i; gsub(/[()]/,"",ms)}
  if(!(name in best) || ms+0 < best[name]) best[name]=ms+0
}
END{ for(n in best) printf "%-8s %6d ms\n", n, best[n] }
'
{
  for r in $(seq 1 $RUNS); do
    $PIN $EVAL +RTS -rBench.comb -RTS 2>/dev/null
  done
} | sort | awk "$awkprog" | sort

# One verbose run for reduction rate + GC stats
echo "--- stats ---"
$PIN $EVAL +RTS -rBench.comb -v -RTS 2>&1 | \
  grep -E 'reductions|gc expired|total expired|max cells' | sed 's/^ *//'
