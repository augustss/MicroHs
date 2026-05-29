#!/bin/sh
# Profile-Guided Optimization build of mhseval + benchmark.
# PGO targets exactly the giant-switch interpreter pattern: it lays out the
# dispatch branches and the hot/cold code by measured frequency.
# Stage 1: instrumented build.  Stage 2: train on representative workloads
# (the micro-benchmark AND a self-compile, so both interpreter and compiler
# paths are profiled).  Stage 3: optimized rebuild using the profile.
# Optimization-only: preserves semantics (no fast-math) and runtime memory.
# Usage: bench/pgo.sh [runs] [extra-flags]
set -u
cd "$(dirname "$0")/.."
RUNS=${1:-8}
EXTRA=${2:-"-march=native -fno-stack-protector"}
PIN="taskset -c 2"
SRC="src/runtime/main.c src/runtime/eval.c src/runtime/comb.c"
INC="-Isrc/runtime -Isrc/runtime/unix"
PROFDIR=/tmp/mhs_pgo
rm -rf "$PROFDIR"; mkdir -p "$PROFDIR"

echo "### PGO stage 1: instrumented build"
cc -Wall -O3 $EXTRA -fprofile-generate="$PROFDIR" $INC $SRC -lm -o /tmp/mhseval_pgogen \
  || { echo "STAGE1 BUILD FAILED"; exit 1; }

echo "### PGO stage 2: training run (bench x2 + self-compile)"
$PIN /tmp/mhseval_pgogen +RTS -rBench.comb -RTS >/dev/null 2>&1
$PIN /tmp/mhseval_pgogen +RTS -rBench.comb -RTS >/dev/null 2>&1
# Self-compile training: need an mhs built with the instrumented runtime.
cc -Wall -O3 $EXTRA -fprofile-generate="$PROFDIR" $INC src/runtime/main.c src/runtime/eval.c generated/mhs.c -lm -o /tmp/mhs_pgogen 2>/dev/null \
  && /tmp/mhs_pgogen -imhs -isrc MicroHs.Main -o/tmp/pgo_self.comb >/dev/null 2>&1 \
  && echo "  (self-compile training done)" || echo "  (self-compile training skipped)"

echo "### PGO stage 3: optimized rebuild with profile"
cc -Wall -O3 $EXTRA -fprofile-use="$PROFDIR" -fprofile-correction -Wno-missing-profile $INC $SRC -lm -o /tmp/mhseval_pgo \
  || { echo "STAGE3 BUILD FAILED"; exit 1; }

echo "### benchmark PGO binary (best-of-$RUNS)"
$PIN /tmp/mhseval_pgo +RTS -rBench.comb -RTS >/dev/null 2>&1
{
  for r in $(seq 1 $RUNS); do $PIN /tmp/mhseval_pgo +RTS -rBench.comb -RTS 2>/dev/null; done
} | awk '
  /nfib32:|tak:|sumTo:|churn:/ {
    name=$1; gsub(/:/,"",name);
    for(i=1;i<=NF;i++) if($i ~ /^\(/){ms=$i; gsub(/[()]/,"",ms)}
    if(!(name in best) || ms+0 < best[name]) best[name]=ms+0
  }
  END{ t=0; for(n in best) t+=best[n];
       printf "PGO: nfib32=%d tak=%d sumTo=%d churn=%d  SUM=%d\n", best["nfib32"],best["tak"],best["sumTo"],best["churn"],t }'
