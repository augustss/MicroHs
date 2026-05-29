# MicroHs performance work — PROGRESS

## TL;DR

**Goal:** increase performance by ≥2×, without increasing memory use.

**Result:** I implemented two optimizations to the C runtime's reduction loop
(`src/runtime/eval.c`) that are **portable, semantics-preserving, byte-identical
on self-compile, and use no extra memory**. They deliver:

| workload | before | after | speedup |
|---|---|---|---|
| Micro-benchmark suite (nfib/tak/sumTo/churn), best wall, `-O3` | 3353 ms | 3047 ms | **1.10×** |
| **Self-compile** (`mhs` compiling `MicroHs.Main`, real-world), best of 3, `-O3` | 55.54 s | 49.27 s | **1.13×** |
| Peak RSS (micro-suite) | 788 948 KB | 788 796 KB | unchanged (−0.02%) |

**The 2× target was not reached. I assess ≥2× to be infeasible through safe
runtime-only changes (and through safe compiler tweaks) on this architecture.**
The evidence and reasoning are in *"Why 2× is infeasible"* below. In short: the
workload is **reduction-count-bound and memory-latency-bound** (dependent
pointer-chasing through a boxed graph), not compute- or codegen-bound, so the
per-reduction cost is already near the hardware floor and the reduction *count*
is fixed by the (already near-optimal) combinator compiler. A 2× would require
changing the value representation (unboxing / native-code or JIT) or the
compiler's emitted combinators — both far outside "tuning the runtime," and the
latter risks the self-hosting bootstrap.

## Time taken

~2 hours of focused work (2026-05-28 23:42 → 2026-05-29 ~01:50 MDT), across two
sessions (the first ran out of context). Roughly: ~40 min understanding the
runtime architecture, ~30 min on the dispatch optimization + validation, ~30 min
on the build-flag/PGO sweep and profiling, ~20 min on the unwind-loop
optimization + A/B isolation, ~10 min writing this up. A large fraction of
wall-clock was spent waiting on the ~50 s self-compile validation cycle.

## What I changed

Both changes are in the hot `top:` dispatch of `evali()` in
`src/runtime/eval.c`. Nothing else in the system was modified. The combinator
compiler, the `.comb` format, the GC, and the public interfaces are untouched —
the self-compiled output is **byte-for-byte identical** to the unmodified
compiler's (`cmp` against `gold.comb`).

### OPT2 — dispatch fast-path

The original `top:` did, on *every* node visited (including every spine node):
decrement the preemption counter, compute `LABEL(n)`, branch on permanent-vs-heap,
test for indirection, then `GETTAG()` and a `switch`. The single most common case
by far is **unwinding the spine** (the node is an application, `T_AP`).

The new code reads the tag word once and handles `T_AP` with one well-predicted
branch *before* touching the preemption counter or the `switch`. For an `AP`
node the low tag bits are `00`, so the tag word *is* the (untagged) `FUN`
pointer — no separate load needed. Indirection-following and tagged-node tag
extraction are split into their own arms using the already-loaded tag word
(`ut >> TAG_SHIFT`) instead of re-deriving via `GETTAG()`. The preemption-slice
tick was moved to fire once per *reduction* (spine head) rather than once per
*spine node*, which keeps the unwind path minimal while still yielding every
~`SLICE` reductions (thread/signal behaviour verified by the Concur/Fork/MVar
tests).

### OPT4 — spine-unwind loop localization

The spine-unwind step is ~60% of all dispatch iterations. In the surrounding
3.4 KB-frame `evali()`, the compiler reloads the globals `stack`, `stack_ptr`,
`stack_size` and spills `n` on *every push*. OPT4 pulls these into locals with a
tiny live range (a dedicated `do/while`), so the register allocator keeps them in
registers for the loop body; the global `stack_ptr` is published once on exit (no
allocation happens while unwinding, so nothing can observe it mid-loop). Verified
in the generated assembly: the loop body is now register-only.

This is **net-neutral on the cache-resident micro-benchmarks** (the
out-of-order core already hides the L1-cached global loads behind the dependent
pointer-chase that defines the loop's critical path) but a **measured ~4.6% win
on the self-compile** (deeper spines, cache pressure): OPT2-only self-compiles in
51.56 s vs 49.27 s with OPT2+OPT4.

## Measurements

All on an Intel i9-13950HX, GCC 13.3, `taskset -c 2`, best-of-N (N≥8). "base" is
the pristine `HEAD` runtime. Micro-suite SUM = nfib32+tak+sumTo+churn (ms).

### Micro-benchmarks (`-O3`)

| build | nfib32 | tak | sumTo | churn | SUM | vs base |
|---|---|---|---|---|---|---|
| base | 999 | 365 | 1025 | 964 | 3353 | 1.00× |
| OPT2 | 915 | 324 | 952 | 859 | 3050 | 1.10× |
| OPT2+OPT4 | 908 | 318 | 957 | 864 | 3047 | 1.10× |

### Self-compile (`-O3`, best of 3)

| build | wall | vs base |
|---|---|---|
| base | 55.54 s | 1.00× |
| OPT2 | 51.56 s | 1.077× |
| OPT2+OPT4 | 49.27 s | **1.127×** |

All builds produce a `.comb` byte-identical to `gold.comb`.

### Memory

Peak RSS on the micro-suite is unchanged (788 948 → 788 796 KB). Note this is
dominated by the 800 MB default heap reservation; actual live data is ~109 KB
(6 810 cells). The optimizations change neither heap nor stack sizing.

## What else I tried (measured, did not pan out)

- **Compiler flags.** A full sweep at `-O3`+`{-march=native, -fno-stack-protector,
  -flto, -fno-semantic-interposition, -funroll-loops}`, plus `-O2`. The best
  combination improved the micro-suite by only **~3.7%**, and `-march=native`
  alone did **nothing** — strong evidence the hot loop is memory-latency/branch
  bound, not compute/SIMD bound. These flags are also non-portable
  (`-march=native`) or a security trade-off (`-fno-stack-protector`), so they are
  **not** part of the shipped change.
- **Profile-Guided Optimization.** Two-stage PGO (trained on the benchmark + a
  self-compile) was **slower** than `-O3` (3006 vs 2942 ms). Dropped.
- **OPT3 — whole-function stack-pointer localization.** Localizing the stack
  pointer across all of `evali()` *regressed* performance: the function is
  register-starved (3.4 KB frame) and GCC spilled the "localized" variables right
  back to the stack, giving the worst of both worlds. Reverted; OPT4 is the
  surgical version that works.
- **Computed-goto dispatch.** GCC fragments the 284-case `switch` into nested
  range-checked sub-tables (~5 compares for a common combinator). A flat
  computed-goto table would remove those *predicted* compares, but **not** the
  shared indirect-branch misprediction (every case funnels back to one `goto
  top`, so the per-opcode prediction benefit of threaded code does not apply
  here). Estimated ~3–5% for a large, error-prone, byte-identical-risking edit —
  poor risk/reward given it cannot approach 2×. Analyzed and deferred.

## Why 2× is infeasible via safe changes

1. **It's not compute- or codegen-bound.** Build flags, including PGO, `-flto`
   and `-march=native`, move the micro-suite by ≤4%. There is no instruction-
   selection or vectorization win to be had — the loop chases pointers and
   branches.
2. **It's memory-latency-bound on a dependent pointer-chase.** Unwinding a spine
   is `n = FUN(n)` repeated — strictly serial dependent loads that cannot be
   prefetched or parallelized. OPT4 cut the *instruction count* of this loop to
   the minimum and got **0%** on cache-resident benchmarks, because the chase
   latency, not instruction throughput, is the critical path. This is intrinsic
   to graph reduction.
3. **The textbook 2× lever — unboxed/tagged integers — is empirically bounded
   below 2× here.** I instrumented the runtime to count binary-integer
   operations as a fraction of all reductions (chokepoint at the `binint1:`
   label). Measured: **18.7%** of the micro-suite's 281 M reductions are integer
   ops, and **33.3%** for the most arithmetic-heavy workload (`nfib`). By
   Amdahl's law, making *every* integer operation cost **zero** — the
   impossible-best case for pointer-tagging — caps the speedup at
   **1/(1−0.187) = 1.23×** on the suite and **1.50× on `nfib`**. The realistic
   win is far smaller (tagging removes ~1–2 dependent loads per op, not the whole
   op). So the one representation change that directly attacks the latency in (2)
   still cannot reach 2×, because the other **67–81% of reductions are
   combinator-structural** (routing data through the `S`/`B`/`C`/`AP` graph),
   and their *count* is fixed by the compiler while their *cost* is the same
   latency-bound chase. (Pointer-tagging would also be a deep, GC-invasive change
   — but the point is that even done perfectly it is mathematically short of 2×.)
4. **GC is not the lever.** GC is 4.7% of the micro-suite and ~17% of the
   self-compile. Even eliminating it entirely cannot yield 2×, and the memory
   constraint forbids the easy win (a bigger heap → fewer GCs).
5. **Arithmetic and the common combinators are already minimal.** Integer ops
   update the redex node in place (zero allocation); `B`/`C`/`S` allocate at most
   one node; the dispatch is now a single predicted branch for the hot AP case.
6. **The reduction *count* is fixed by the compiler.** `Abstract.hs` is a mature,
   optimized Kiselyov-style bracket abstraction (with `S'`/`B'`/`C'`/`K2..K4`/`Z`/`R`
   etc.). Halving the number of reductions would require improving that — a
   research-level change that also alters every `.comb` output and so must still
   reach a self-hosting fixpoint. High risk, low probability of a 2× reduction
   count, and outside "make the runtime faster."
7. **Per-reduction cost is near the floor.** At the observed rate the suite runs
   at roughly ~15–20 cycles per reduction, dominated by dependent loads. There is
   no ~2× of slack to remove without changing *what* a reduction is.

## What a 2× would actually require (out of safe scope)

- **Native compilation / a JIT (the real lever — not unboxing alone).** As the
  measurement above shows, tagged-integer unboxing *by itself* caps at ~1.23×
  here because most reductions are structural, not arithmetic. GHC beats this
  model by ~10–50× on `nfib` because it does both: unboxed `Int#` **and** native
  code that eliminates the per-reduction dispatch/spine-chase entirely (the
  structural 67–81%). That requires emitting machine code (or a JIT) instead of
  interpreting a boxed combinator graph — a new backend, not a runtime tweak.
- **A compacting/copying GC for locality.** The self-compile touches ~800 MB and
  is cache-missing; laying related nodes contiguously could help *that* workload
  (not the cache-resident micro-benchmarks), but it is a major GC rewrite with
  real correctness/bootstrap risk, and still would not 2× the compute benchmarks.
- **Compiler-side reduction-count reduction.** As in (5) above.

## Honest conclusion

I improved real-world compiler throughput by **~13%** and compute micro-benchmarks
by **~10%**, with zero memory cost and byte-identical output — a safe, shippable
win. I could not reach 2×, and I've laid out concrete, evidence-backed reasons why
2× is not attainable by tuning this runtime: the bottleneck is the latency of
chasing pointers through a boxed combinator graph and the reduction count fixed by
the compiler, neither of which a safe runtime change can halve. Reaching 2× means
changing the execution model (unboxing/native/JIT) or the compiler — larger,
riskier projects than the goal's framing of "make microhs faster" by tuning.
