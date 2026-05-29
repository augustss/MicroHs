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

**I did not reach 2×, and — importantly — my original conclusion that 2× was
"infeasible" was wrong.** A parallel Codex experiment reached **2.43×** on the
self-compile by *preloading a precompiled `base.pkg`* so the compiler stops
recompiling the base library every run. That's a workload/algorithmic win I
scoped myself out of by tunnel-visioning on the reduction engine. See
[The 2× question](#the-2-question-where-i-was-wrong).

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
| **Package preload (Codex's lever)** | **the real 2× — I missed it** (see below) | not attempted by me |

## The 2× question (where I was wrong)

My engine-level analysis still holds *as far as it goes*: per-reduction cost is
near the hardware floor (dependent pointer-chasing), the reduction *count* is
fixed by the already-optimal Kiselyov compiler, build flags buy ≤3.7%, GC is
minor, and I measured that only **18.7%** of suite reductions are integer ops
(so even free arithmetic via unboxing caps at ~1.23× — and "free" overstates what
unboxing does). All true. **But I let that define the whole problem and concluded
"microhs can't go 2×," which doesn't follow.**

The brief was "make microhs faster," and the real workload is the compiler. Its
dominant cost is **recompiling the entire base library from source on every run** —
redundant work, not engine speed. Codex attacked *that*: load a precompiled
`base.pkg` (a feature microhs already has) and skip it, regenerating the package
with cleared source paths so output stays byte-identical. Result: **2.43×** on the
self-compile. I had the clues (microhs has a package system; `generated/base.pkg`
exists; my own early bug was a package-path problem) and dismissed the
compiler/workload axis as out of scope. That was the miss.

For completeness, the things that *would* push the **engine** itself toward 2×
(unboxed/tagged integers, native codegen or a JIT, a compacting GC for locality)
remain out of "tune the runtime" scope and are larger, riskier projects — but they
are no longer the only path to a 2× *compiler*, which is what the goal asked for.

## Honest conclusion

A safe, shippable **~1.05–1.09×** with zero memory cost and byte-identical output,
validated against the full suite — plus a corrected understanding: 2× *is*
attainable for the self-compile, via not recompiling the base library, and I
reached the wrong conclusion by scoping the problem to the reduction engine and
trusting single-threaded validation. The methodology (the harness, sweep, and
notes) is in `bench/`.
