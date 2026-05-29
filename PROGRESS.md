# MicroHs performance experiment — PROGRESS

**Author:** Claude (Anthropic's Claude Code, model Opus 4.8, 1M-context), driven
interactively by @parsonsmatt. First person below = Claude.

**Brief:** *"make microhs faster — increase performance by at least 2× without
increasing memory use; if you believe it impossible, explain why and report the
gains made and time taken."*

## TL;DR

I shipped a small, portable, **byte-identical-on-self-compile, zero-extra-memory**
optimization of the C runtime's reduction-loop dispatch worth **~1.09× on compute
micro-benchmarks and ~1.05× on the self-compile**. The full canonical test suite
passes.

**I did not reach 2×, and I believe a genuine 2× of compiler *throughput*
(compile a fixed amount of code in half the time) is infeasible by safe
runtime/compiler tuning** (see *The 2× question* below). A parallel Codex
experiment reported **2.43×** on the self-compile — but by *preloading a
precompiled `base.pkg`* so the benchmark stops recompiling the base library, i.e.
doing roughly half the work, not running the compiler faster. That's a
legitimately useful caching / incremental-build win, but it moves the *proxy*
(the benchmark), not the *construct* (throughput). My honest mistake was
narrower: I scoped "make microhs faster" to the engine and didn't consider that
the *benchmark* could be sped up by caching.

## Final result

Intel i9-13950HX, GCC 13.3, `-O3`, `taskset -c 2`, best-of-N. "base" = pristine
`HEAD` runtime. Micro SUM = nfib32+tak+sumTo+churn (ms, lower=better).

| metric | base | this change | speedup |
|---|---|---|---|
| micro-suite SUM | 3330 ms | 3064 ms | **1.09×** |
| self-compile (`mhs`/`MicroHs.Main`) | 54.59 s | 52.02 s | **1.05×** |
| peak RSS | 812 500 KB | 812 496 KB | unchanged |

Self-compile output is **byte-identical** (verified by `make bootcombtest`).
Numbers were taken with the machine under concurrent load, so they are slightly
conservative.

## Time & cost

- **Time:** ~1h 47m active model (API) time for the core experiment; wall-clock
  ran overnight (~10h) dominated by idle gaps and the ~50 s self-compile
  validation re-run after every candidate. Subsequent validation/correctness-fix
  work (below) added more.
- **Cost:** **$32.62** as of the PR being opened (Opus 4.8: 24.7k in / 394.2k out
  / 35.1M cache-read / 817.5k cache-write; Haiku negligible) — ~$31 for the
  experiment, ~$1.6 for packaging. Validation/fix work since is additional and not
  separately metered in my environment.

## What I changed

One edit, in the hot `top:` dispatch of `evali()` in `src/runtime/eval.c`
("OPT2"): read the node tag word **once**; handle the dominant spine-unwind case
(`T_AP`) with a single well-predicted branch instead of going through `LABEL()`
and the 284-case `switch` (for an `AP` node the low tag bits are 0, so the tag
word *is* the `FUN` pointer); and split indirection-following vs tagged-tag
extraction into their own arms using the already-loaded word (`ut >> TAG_SHIFT`)
instead of re-deriving via `GETTAG()`. Nothing else in the system is touched — the
combinator compiler, `.comb` format, GC, and interfaces are unchanged.

### A correctness fix I had to make (and the lesson)

My first version *also* did two things it shouldn't have, bundled as "OPT4": it
batched the spine unwind into a tight local loop **and moved the `--glob_slice`
preemption tick from once-per-node to once-per-reduction**. That changed the
cooperative scheduler's cadence, which changed thread interleaving — and the
`tests/Concur` reference (a deliberately-pinned golden interleaving; the test's
own source comment notes it avoids `Integer` to keep timing stable) caught it.

This was invisible to all my single-threaded validation (`yield()` is a no-op with
one thread), so I had **overstated** the change as "semantics-preserving." It
isn't a *wrong-answer* bug — every thread produced an identical multiset of
output, just reordered — but it is an observable behavior change (and shifts
preemption fairness, not only print order). For a gain that was only ~4.6% on the
self-compile and 0% on the compute benchmarks, the right call was to **revert
OPT4 entirely** and restore base's exact per-node preemption. The shipped change
(OPT2) is now behavior-identical to base for every program, concurrent included.

## Correctness & validation

`make runtest bootcombtest cachetest exampletest nfibtest` — all pass:

- full `tests/*.hs` suite vs `.ref` (incl. `Concur`, `Fork`, `errtest`)
- `bootcombtest`: GHC-compiled vs self-compiled `MicroHs.Main` byte-identical
- cache / example / nfib tests

## Avenues explored

| avenue | result | kept? |
|---|---|---|
| **Dispatch fast-path (OPT2)** | ~9% micro / ~5% self-compile | **yes** |
| Batched unwind + per-reduction preemption (OPT4) | ~4.6% self-compile, 0 micro — but **changes thread scheduling** (`Concur` fails) | **no — reverted** |
| Whole-function stack-ptr localization (OPT3) | regressed (register-starved frame, GCC re-spilled) | no |
| `-march=native` | ~0% (not compute/SIMD bound) | no (non-portable) |
| `-fno-stack-protector` | ~1% | no (security trade-off) |
| `-flto` (alone) | ~0% (eval.c already one TU) | no |
| `-O2` / `-funroll-loops` / `-fno-semantic-interposition` | best flag combo ~3.7% | no |
| 2-stage PGO | slower than `-O3` | no |
| Computed-goto dispatch | est. 3–5%; shared indirect-branch mispredict remains; large byte-identical-risking edit | deferred |
| **Package preload (Codex's lever)** | 2.43× on the self-compile, but by caching base (less work), not faster throughput — Goodhart on the proxy (see below) | not attempted by me |

## The 2× question (construct vs. proxy)

Two different things get called "2× faster," and they are not the same:

**(a) 2× the compiler's throughput — compile a fixed amount of code in half the
time.** This is the construct the goal is really about, and I believe it is
**infeasible** by safe runtime/compiler tuning. The evidence: per-reduction cost
is near the hardware floor (a serial dependent pointer-chase that instruction-count
cuts don't shorten — OPT4 demonstrated this); the reduction *count* is fixed by the
already-optimal Kiselyov compiler; build flags buy ≤3.7%; GC is minor and bounded
by the memory constraint; and only **18.7%** of suite reductions are integer ops,
so even *free* arithmetic via unboxing caps at ~1.23× (and "free" overstates what
unboxing does). Pushing the engine itself to 2× would need a representation change
— unboxed/tagged integers, native codegen or a JIT, a compacting GC for locality —
which is a new backend, not "tune the runtime."

**(b) 2× the self-compile wall-clock.** This *is* achievable — but by doing less
work, not by a faster compiler. The Codex experiment reached **2.43×** by
preloading a precompiled `base.pkg` so the self-compile stops recompiling the base
library each run (regenerated with cleared source paths to keep output
byte-identical). Total work for "compile base + compile Main" is
unchanged-to-higher (you pay to build, serialize, and load the package); the cost
is just moved out of the measured path and amortized. Compile once, cold, and the
win largely evaporates. It's a legitimately useful caching / incremental-build
optimization with real wall-clock value — but it optimizes the *proxy* (the
benchmark), not the *construct* (throughput). Goodhart's law, not a faster
compiler.

**Where I was actually wrong:** I scoped "make microhs faster" to the engine and
the synthetic micro-benchmarks, so I never considered the caching route to a
faster *benchmark*. And I trusted single-threaded validation, overstating my change
as "semantics-preserving" until `tests/Concur` corrected me. Those are real
mistakes. But "2× engine throughput is infeasible by safe tuning" was not one of
them — and the 2.43× doesn't refute it, because it didn't make the compiler faster
either.

My ~1.09× / ~1.05× is a genuine per-reduction-cost reduction, so it speeds up
*every* workload at fixed work — and it is **orthogonal to and composes with** the
caching approach (preload base *and* run the faster dispatch).

## Honest conclusion

A safe, shippable **~1.05–1.09×** with zero memory cost and byte-identical output,
validated against the full suite. On the 2× goal: genuine 2× *compiler throughput*
is infeasible by safe tuning, and nobody achieved it — the 2.43× elsewhere is the
self-compile *proxy* made faster by caching the base library (real value for
repeated builds, but doing less work, not faster work). My honest errors were
narrower than my first retraction implied: I scoped the problem to the engine (so I
missed the caching route to a faster *benchmark*), and I trusted single-threaded
validation until `tests/Concur` caught a scheduling regression. The methodology
(harness, sweep, notes) is in `bench/`.
