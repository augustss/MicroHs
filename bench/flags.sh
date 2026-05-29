#!/bin/sh
# Build mhseval with several compiler-flag variants and benchmark each.
# Correctness of the eval.c *source* is checked separately by validate.sh;
# these flags are optimization-only (no fast-math) so they preserve semantics.
# Usage: bench/flags.sh [runs]
set -u
cd "$(dirname "$0")/.."
RUNS=${1:-8}
PIN="taskset -c 2"
SRC="src/runtime/main.c src/runtime/eval.c src/runtime/comb.c"
INC="-Isrc/runtime -Isrc/runtime/unix"
BASEW="-Wall"

bench_bin() {
  bin=$1
  # warm
  $PIN "$bin" +RTS -rBench.comb -RTS >/dev/null 2>&1
  {
    for r in $(seq 1 $RUNS); do
      $PIN "$bin" +RTS -rBench.comb -RTS 2>/dev/null
    done
  } | awk '
    /nfib32:|tak:|sumTo:|churn:/ {
      name=$1; gsub(/:/,"",name);
      for(i=1;i<=NF;i++) if($i ~ /^\(/){ms=$i; gsub(/[()]/,"",ms)}
      if(!(name in best) || ms+0 < best[name]) best[name]=ms+0
    }
    END{ t=0; for(n in best) t+=best[n];
         printf "  nfib32=%d tak=%d sumTo=%d churn=%d  SUM=%d\n", best["nfib32"],best["tak"],best["sumTo"],best["churn"],t }'
}

build_and_bench() {
  name=$1; flags=$2
  bin="/tmp/mhseval_$name"
  if cc $BASEW $flags $INC $SRC -lm -o "$bin" 2>/tmp/cc_$name.err; then
    printf "%-22s" "$name:"
    bench_bin "$bin"
  else
    printf "%-22s BUILD FAILED (see /tmp/cc_%s.err)\n" "$name:" "$name"
  fi
}

echo "### compiler-flag sweep (best-of-$RUNS, taskset -c 2), times in ms, lower=better"
build_and_bench "O3"            "-O3"
build_and_bench "O3-march"      "-O3 -march=native"
build_and_bench "O3-nossp"      "-O3 -fno-stack-protector"
build_and_bench "O3-march-nossp" "-O3 -march=native -fno-stack-protector"
build_and_bench "O3-lto"        "-O3 -flto"
build_and_bench "O3-all"        "-O3 -march=native -fno-stack-protector -flto -fno-semantic-interposition"
build_and_bench "O2"            "-O2"
build_and_bench "Ofast-nofm"    "-O3 -march=native -fno-stack-protector -funroll-loops"
