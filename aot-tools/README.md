# Tools for the AOT experiment

These are the scripts and data behind the "Third result" in `jit-plan.md`.
They put compilers, the workload and outputs in `$AOTWORK`, which
defaults to `aot-work` next to this checkout.

| file | |
|---|---|
| `build.sh` | builds the code generator `mhs-aot`, the compilers `mhs-0` (hooks, nothing compiled), `mhs-8`, `mhs-25` and `mhs-78`, a frozen copy of the sources as the workload, and the reference output `ref.c`.  `build.sh all` also builds `mhs-all` (every function), which takes 2 minutes and 9.5 GB.  With `BASE=` a jit-sampler checkout it also builds `mhs-base`, whose runtime has no AOT hooks. |
| `bench.sh [RUNS [VERSION...]]` | times the compilers on the workload, interleaved, with `/usr/bin/time -l` and `+RTS -v`, and checks each output against `ref.c`.  Writes `$AOTWORK/meas.csv`. |
| `aottests.sh MODE OUTDIR [TESTS...]` | builds each short-form test in `tests/all.tests` without and with `--aot` and compares the output with its `.ref`.  MODE `all` compiles every function; any other MODE compiles only the test's module.  Serdes, SerdesDescr, Reflect and PartRed are expected to fail with `--aot`, because they serialize or print code. |
| `bisect.sh LIST` | bisects a list of definitions for one that breaks the self-compile when compiled. |
| `ranked.txt` | the definitions in order of the reductions they cost in a self-compile, from the per-node profile on the `profile` branch.  The top 8, 25 and 78 cover 25%, 50% and 80% of the reductions. |
| `alldefs.txt` | every definition `--aot=*` compiles, the input to `bisect.sh` for the everything build. |
| `results.csv` | the measurements in `jit-plan.md`: 3 interleaved runs of each version. |

Typical use:

```
aot-tools/build.sh all
aot-tools/bench.sh 3 0 8 25 78 all
aot-tools/aottests.sh mod /tmp/aottests
```
