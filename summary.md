# Codex: "MicroHs self-compile performance experiment"

## Goal

Make the MicroHs self-hosted compiler substantially faster without increasing memory use, using CPU/user time as the primary timing metric because wall time was affected by machine contention.

## Final result

The original baseline run was later remeasured after CPU contention was reduced. The corrected comparison is:

| Run | User CPU | Elapsed | Max RSS |
| --- | ---: | ---: | ---: |
| Clean HEAD baseline rerun | 56.72s | 57.04s | 813424 KB |
| Optimized rerun | 23.34s | 23.55s | 666992 KB |

That is a 2.43x speedup by user CPU with about 18% lower max RSS.

For context, an earlier contested baseline measured 79.34s user CPU and 813272 KB max RSS. Against that run the optimized path measured as a 3.37x speedup, but the corrected 2.43x comparison above is the fair number.

The optimized output was byte-identical to the clean baseline combinator output.

## Changes in this experiment

- Enabled C link-time optimization for runtime builds with `-flto=auto -fwhole-program`.
- Made self-hosted MicroHs compilation preload the tracked `generated/base.pkg` when available.
- Kept GHC-hosted compiler generation package-free, because `bin/gmhs` does not read MicroHs serialized packages.
- Regenerated `generated/base.pkg` from the repository root with cleared source paths so package-preloaded output remains byte-identical to normal source compilation output.
- Reduced the default RTS heap from 50M cells to 40M cells to lower RSS while preserving the speedup.
- Optimized several C runtime hot paths:
  - use compiler count-trailing-zero builtins for free-bit scanning;
  - restart allocation scans after the allocated bit instead of at it;
  - combine GC mark test and mark update into one helper;
  - use `memcmp` for bytestring lexicographic comparison.

## Verification

- `make bootcombtest` passed.
- Fresh `bin/gmhs` combinator output compared byte-for-byte equal with package-preloaded `bin/mhs` output.
- Clean HEAD baseline rerun combinator output compared byte-for-byte equal with optimized rerun output.
- `git diff --check` passed.

## Avenues explored

- C runtime and Makefile changes:
  - LTO was high impact and kept.
  - `-fwhole-program` with LTO was kept.
  - `-march=native` was tested and rejected as too small and non-portable.
  - Extra GCC options such as `-fno-semantic-interposition` and `-fipa-pta` were tested and rejected.
  - A `memcmp` bytestring compare replaced a byte loop and was kept.
  - A bytestring equality fast path was tested and reverted.
  - Heap sizes of 20M, 30M, 40M, and 49M cells were tested; 40M was selected as the best default tradeoff.
- Package and compiler changes:
  - Preloading `generated/base.pkg` was the main algorithmic speedup and was kept.
  - Package source-location differences were investigated; the package is now generated from root paths to keep strict output comparisons valid.
  - A lazy package-cache rewrite was attempted and reverted because package instance dictionary definitions were not loaded correctly.
  - `ExpPrint.collectDefs` fusion was tested and reverted because it did not improve the package peak.
  - `Data.Text.compare` via direct bytestring compare improved time slightly but increased max-live cells, so it was reverted.
  - Compile-cache package stripping was tested and reverted because it did not fix package peak.

## Memory note

The memory win is measured by OS max RSS. RTS max live cells increase with package preloading because the workload shape changes:

| Run | Max live cells |
| --- | ---: |
| Clean HEAD baseline rerun | 4,312,949 |
| Optimized rerun | 5,255,870 |

Despite that, max RSS drops from 813424 KB to 666992 KB because the default heap is smaller.

## Time and cost

- Agent-tracked elapsed time: 12,943 seconds, about 3h 35m 43s.
- Agent-tracked token usage: 2,590,711 tokens.
- Dollar cost was not available from the local workspace or goal tracker, so no USD estimate is included.
