#!/bin/sh
# Final headline measurement: the PORTABLE runtime-source speedup (plain -O3,
# no CPU-specific or security-reducing flags), base vs OPT2+OPT4.
# Also reports the (small, non-portable) extra from -march=native -fno-stack-protector.
# Usage: bench/final.sh [runs]
set -u
cd "$(dirname "$0")/.."
RUNS=${1:-12}
PIN="taskset -c 2"
INC="-Isrc/runtime -Isrc/runtime/unix"
SRC="src/runtime/main.c src/runtime/eval.c src/runtime/comb.c"

echo "### building binaries"
# base = pristine HEAD eval.c at -O3
git show HEAD:src/runtime/eval.c > /tmp/eval_base.c
cc -Wall -O3 $INC src/runtime/main.c /tmp/eval_base.c src/runtime/comb.c -lm -o /tmp/mhseval_base_O3 2>/dev/null && echo "  base  -O3            ok"
# current = OPT2+OPT4 eval.c at -O3 (portable) and at -O3-march-nossp (max)
cc -Wall -O3 $INC $SRC -lm -o /tmp/mhseval_cur_O3 2>/dev/null && echo "  OPT4  -O3            ok"
cc -Wall -O3 -march=native -fno-stack-protector $INC $SRC -lm -o /tmp/mhseval_cur_max 2>/dev/null && echo "  OPT4  -O3-march-nossp ok"

bench_one() {
  label=$1; bin=$2
  $PIN "$bin" +RTS -rBench.comb -RTS >/dev/null 2>&1
  { for r in $(seq 1 $RUNS); do $PIN "$bin" +RTS -rBench.comb -RTS 2>/dev/null; done; } | awk -v L="$label" '
    /nfib32:|tak:|sumTo:|churn:/ { name=$1; gsub(/:/,"",name);
      for(i=1;i<=NF;i++) if($i ~ /^\(/){ms=$i; gsub(/[()]/,"",ms)}
      if(!(name in best)||ms+0<best[name]) best[name]=ms+0 }
    END{ t=0; for(n in best) t+=best[n];
      printf "%-24s nfib32=%4d tak=%3d sumTo=%4d churn=%4d  SUM=%d\n", L, best["nfib32"],best["tak"],best["sumTo"],best["churn"],t }'
}
echo "### micro-benchmark (best-of-$RUNS, taskset -c 2), ms, lower=better"
bench_one "base  -O3:"            /tmp/mhseval_base_O3
bench_one "OPT2+OPT4 -O3:"        /tmp/mhseval_cur_O3
bench_one "OPT2+OPT4 -O3-m-nossp:" /tmp/mhseval_cur_max

echo "### peak RSS (KB) on the micro-suite"
for b in /tmp/mhseval_base_O3 /tmp/mhseval_cur_O3; do
  rss=$(\time -v "$b" +RTS -rBench.comb -RTS 2>&1 | awk -F': ' '/Maximum resident/{print $2}')
  printf "  %-22s %s KB\n" "$b" "$rss"
done
