# MicroHs performance experiment — summary

**Author:** Claude (Anthropic's Claude Code, model Opus 4.8, 1M-context), driven
interactively by @parsonsmatt.

**Brief given to me:** *"make microhs faster — increase performance by at least 2×
without increasing memory use; if you believe it impossible, explain why and
report the gains made and time taken."*

## Outcome

I did **not** reach 2×. I implemented a portable, semantics-preserving,
**byte-identical-on-self-compile, zero-extra-memory** optimization of the C
runtime's reduction loop that delivers **~1.10× on compute micro-benchmarks** and
**~1.13× on the real-world self-compile**, and I then **measured** why ≥2× is not
reachable by tuning this runtime (the headline number: only 18.7% of the work is
arithmetic, so even the textbook unboxing win is mathematically < 1.25× here).

## Time

**~2 hours** of wall-clock agent time, spanning two context windows
(2026-05-28 23:42 → 2026-05-29 ~01:55 MDT). Rough split: ~40 min reading the
runtime to understand the execution model; ~30 min implementing + validating the
dispatch optimization; ~30 min on the build-flag/PGO sweep and profiling; ~20 min
on the unwind-loop optimization and its A/B isolation; ~10 min instrumenting the
arithmetic-fraction measurement; ~15 min writing up. A large fraction of the
elapsed time was spent *waiting on* the ~50 s self-compile validation cycle, which
I ran many times to keep the change byte-identical.

## Cost

I don't have exact API token/dollar metering exposed in this environment, so I'm
reporting cost as the work performed rather than a fabricated figure (the precise
$ is visible on the operator's usage dashboard):

- **Agent:** one Opus 4.8 (1M-context) session at max reasoning effort, two
  context windows, ~100+ tool calls.
- **Machine work driven:** several dozen full runtime rebuilds (`cc -O3` over
  `eval.c`+`main.c`+`comb.c`, ~10 s each); ~15–20 self-compile runs at ~50 s each
  (the dominant compute cost); a build-flag matrix (≈8 variants) and a 2-stage PGO
  build; one throwaway instrumented build for the arithmetic-fraction count.
- **Net:** the expensive part was validation, not search — keeping the output
  byte-identical meant re-running the full self-compile after every candidate.

## Performance results

Intel i9-13950HX, GCC 13.3, `taskset -c 2`, best-of-N (N≥8). "base" = pristine
`HEAD` runtime. Micro-suite SUM = nfib32+tak+sumTo+churn (ms, lower=better).

### Micro-benchmarks (`-O3`)

| build | nfib32 | tak | sumTo | churn | SUM | vs base |
|---|---|---|---|---|---|---|
| base | 999 | 365 | 1025 | 964 | 3353 | 1.00× |
| OPT2 | 915 | 324 | 952 | 859 | 3050 | 1.10× |
| OPT2+OPT4 | 908 | 318 | 957 | 864 | 3047 | **1.10×** |

### Self-compile — `mhs` compiling `MicroHs.Main` (`-O3`, best of 3)

| build | wall | vs base |
|---|---|---|
| base | 55.54 s | 1.00× |
| OPT2 | 51.56 s | 1.077× |
| OPT2+OPT4 | 49.27 s | **1.127×** |

Every build emits a `.comb` **byte-identical to `gold.comb`** (the unmodified
compiler's output). **Peak RSS unchanged:** 788 948 → 788 796 KB.

## The change (two edits, both in `eval.c`'s `top:` dispatch)

- **OPT2 — dispatch fast-path.** Read the node tag word once; handle the hot
  spine-unwind (`T_AP`) case with a single predicted branch *before* the
  preemption tick and the 284-case `switch`; for an `AP` node the tag word *is*
  the `FUN` pointer (low bits `00`), so no extra load. Split
  indirection-following and tagged-tag extraction into their own arms using the
  already-loaded word. Moved the preemption-slice tick to fire once per
  *reduction* (spine head) instead of once per *spine node*.
- **OPT4 — unwind-loop localization.** The spine unwind is ~60% of dispatch
  iterations; in the 3.4 KB-frame `evali()` the compiler was reloading the
  `stack`/`stack_ptr`/`stack_size` globals and spilling on every push. Give them
  a tiny local live range (a dedicated `do/while`) so the loop body is
  register-only; publish `stack_ptr` once on exit. Net-neutral on cache-resident
  micro-benchmarks (the chase latency hides the saved loads) but **~4.6%** on the
  self-compile.

## Avenues explored

| avenue | result | kept? |
|---|---|---|
| **Dispatch fast-path (OPT2)** | ~10% micro / ~7.7% self-compile | **yes** |
| **Unwind-loop localization (OPT4)** | ~0% micro, ~4.6% self-compile | **yes** |
| Whole-function stack-ptr localization (OPT3) | **regressed** — register-starved frame, GCC spilled the "locals" back | no (reverted) |
| `-march=native` | ~0% — not compute/SIMD bound | no (also non-portable) |
| `-fno-stack-protector` | ~1% — security trade-off | no |
| `-flto` | ~0% — `eval.c` already one TU | no |
| `-O2`, `-funroll-loops`, `-fno-semantic-interposition` | best flag combo only ~3.7% total | no |
| 2-stage PGO (train on bench + self-compile) | **slower** than `-O3` | no (dropped) |
| Computed-goto dispatch | est. 3–5%; can't fix the shared indirect-branch mispredict (cases funnel to one `goto top`); large byte-identical-risking edit | no (analyzed, deferred) |
| Bigger heap (fewer GCs) | forbidden by the no-extra-memory constraint | n/a |
| **Tagged/unboxed integers** (the textbook lever) | **measured ceiling 1.23× suite / 1.50× nfib** even if arithmetic were free — see below | no (out of scope *and* sub-2×) |

## Why ≥2× is infeasible by safe tuning (measured, not asserted)

I instrumented the runtime (throwaway build) to count binary-integer operations
at their single chokepoint (`binint1:`) as a fraction of all reductions:

- **micro-suite:** 52,543,192 / 281,341,769 reductions = **18.7%** are integer ops
- **nfib (most arithmetic-heavy):** 179,164,494 / 537,497,679 = **33.3%**

By Amdahl's law, making *every* integer op cost **zero** — the impossible-best
case for pointer-tagging/unboxing — caps the speedup at **1/(1−0.187) = 1.23×**
(suite) and **1.50×** (nfib). The remaining **67–81% of reductions are
combinator-structural** — routing data through the `S`/`B`/`C`/`AP` graph — and
their *count* is fixed by the (already-optimal Kiselyov) compiler while their
*cost* is a serial dependent pointer-chase (`n = FUN(n)`) whose latency OPT4
proved you cannot shorten by cutting instructions. Build flags move ≤3.7%; GC is
4.7%/17% and bounded by the memory constraint; arithmetic already updates the
redex node in place. **No combination of safe runtime changes stacks to 2×.**

## What a real 2× would require (out of scope here)

Native code or a JIT — eliminate the per-reduction dispatch and spine-chase on the
*structural* majority, *and* unbox integers — i.e. a new backend. Or compiler-side
reduction-count reductions, which change every `.comb` and must still reach a
self-hosting fixpoint. Both are larger, riskier projects than "tune the runtime,"
and the latter breaks the byte-identical bootstrap this experiment preserved.

## Conclusion

A safe, shippable **~1.1–1.13×** with zero memory cost and byte-identical output,
plus a quantified demonstration that 2× is not on the table for this execution
model without changing what a reduction fundamentally costs. Full technical
write-up in `PROGRESS.md`; methodology and the raw sweep in `bench/NOTES.md`.
