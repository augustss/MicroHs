# MicroHs performance work — notes & timing

Goal: >=2x runtime speedup, no increase in memory use.

## Timeline
- 2026-05-28 23:42 MDT: session start, exploration begins.

## Baselines (taskset -c 2, best-of-N)
- Micro (bench/Bench.hs via bench/run.sh):
    churn 965ms | nfib32 999ms | sumTo 1031ms | tak 366ms ; aggregate 218.5 Mred/s, GC 4.5%
- Canonical nfib 35 (tests/Nfib.hs): ~8.7-10.6M nfib/s, 229 Mred/s, RSS 8MB
- Self-compile (bin/mhs ... MicroHs.Main): 56.98s wall, RSS 812MB, 162 Mred/s, GC 16.8%

## Optimizations (each measured A/B vs bin/mhseval.base)

### Micro-benchmark SUM (nfib32+tak+sumTo+churn), best-of-8, ms, lower=better
- base (-O3, pre-opt):                     SUM 3357   (nfib 1000 / tak 365 / sumTo 1029 / churn 963)
- OPT2 dispatch (-O3):                      SUM 3050   = 1.10x   (AP fast-path + read-uutag-once + split IND/tagged)
- OPT2 + -march=native:                     SUM 3070   (no help: workload not compute/SIMD bound)
- OPT2 + -fno-stack-protector:              SUM 3023   (~1%: removes %fs canary per evali call)
- OPT2 + -march + -nossp:                   SUM 2946   = 1.14x   (best simple flag combo)
- OPT2 + -flto:                             SUM 3042   (~0: eval.c already one TU)
- OPT2 + all flags + no-semantic-interpos:  SUM 2942   = 1.14x
- O2:                                       SUM 3066
- PGO (train on bench, self-compile partial): SUM 3006  (WORSE than -O3-all; PGO not a win here) -> dropped
- OPT4 unwind-loop localization (-O3-m-n):  SUM <pending>

### Findings
- Workload is REDUCTION-BOUND, not GC/alloc-bound: micro-suite GC=4.7%, max 6810 live cells
  (109 KB) in an 800 MB (50M-cell) default heap. Pure per-reduction execution cost dominates.
- Build flags are nearly useless here (~3.7% total): march=0, lto=0, nossp~1%, PGO negative.
  Strong evidence the hot loop is memory-latency / branch bound, already well-compiled.
- Dispatch (OPT2) ~10%. OPT3 (whole-function stack-ptr localization) REGRESSED -> reverted:
  GCC spilled the locals (3.4 KB frame, register-starved) -> worst of both worlds.
- OPT4 = localize ONLY the spine-unwind loop's live range. Confirmed in asm: loop body is now
  register-only; the 4 globals (stack/stack_ptr/stack_size/n) load once in pre-header, stack_ptr
  stored once on exit (was 4 global mem-ops + n-spill PER push). Unwind ~62% of dispatch iters.
- Computed-goto: GCC fragments the 284-case switch into nested range-checked sub-tables
  (~5 compares for a common combinator). A flat goto-table removes the predicted compares but
  NOT the shared indirect-branch misprediction (cases funnel to one `goto top`), so est. ~3-5%
  for a large, byte-identical-risking edit. Deferred unless needed.

### Self-compile (real-world: mhs compiling MicroHs.Main -> .comb), best wall of 3, -O3
- base  (pristine eval.c):   55.54 s
- OPT2+OPT4 dispatch:        49.27 s   = 1.127x  (~12.7% faster), byte-identical to gold
- OPT2-only:                 <pending bbo9tdkng>  (isolates whether OPT4 helps the real workload)
- Self-compile responds BETTER to the dispatch opt than the synthetic micro-suite (it is more
  reduction/dispatch-bound). RSS unchanged (~789 MB, dominated by the 800 MB heap reservation).

### OPT4 verdict
- Micro: net wash (nfib -2%, tak -2%, sumTo +2%, churn 0). Reason: spine unwind is LATENCY-bound
  on the dependent pointer-chase `ln = FUN(ln)`; the OoO core already hides the L1-cached global
  loads OPT4 removed, so cutting instruction count does not shorten the critical path.
- Decision pending OPT2-only self-compile isolation.

### Fundamental 2x analysis
- nfib/tak: cache-resident, reduction-overhead bound. Arithmetic already updates the redex node
  in place (no alloc). Combinator reductions already minimal-alloc (1 new_ap for B/C).
- 2x => halve per-reduction cost. Unwind already ~9 instr; head-dispatch ~15; reduction body
  (alloc) hard to cut. No single runtime-only lever halves this without changing the boxed-graph
  representation (unboxing/JIT) or the compiler's emitted combinators (breaks self-compile).

### Unboxing-ceiling measurement (instrumented throwaway build /tmp/eval_instr.c)
- Counted binary-int ops (counter at binint1: chokepoint) / total reductions:
    suite (Bench.comb):  52,543,192 / 281,341,769 = 18.7% binint
    nfib  (Nfib.comb) : 179,164,494 / 537,497,679 = 33.3% binint  (most arithmetic-heavy)
- Amdahl impossible-best (arithmetic made FREE): suite -> 1.23x max ; nfib -> 1.50x max.
- => Tagged-int unboxing, the textbook lever, is mathematically < 2x here. The other 67-81% of
  reductions are combinator-structural (S/B/C/AP routing): count fixed by the compiler, cost is
  the same latency-bound pointer-chase. Confirms 2x needs native code/JIT (kill the dispatch on
  the structural majority too), not just a value-representation tweak. GC-rewrite & special-comb
  counters both ~0 -> not levers. Instrumentation NOT in shipped eval.c (throwaway in /tmp).
