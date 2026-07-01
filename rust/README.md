# MicroHs Rust Rewrite

This directory is the staged Rust rewrite of the MicroHs runtime.

The compatibility boundary is the existing combinator file format. Tier 0 keeps
the C runtime as the oracle and teaches Rust to parse and reduce small pure
programs from the same `.comb` data. Later tiers grow the runtime surface until
the Rust implementation can replace `src/runtime/eval.c` for native and wasm
targets.

## Tiers

1. Parse `.comb` `v8.4` files and reduce the core pure combinators.
2. Add literals, primitive arithmetic, strings, and arrays.
3. Add IO actions, scheduler behavior, exceptions, and GC-owned data.
4. Add C FFI, foreign exports, stable pointers, and ByteString/ForeignPtr parity.
5. Add wasm builds, including the current JavaScript FFI bridge semantics.
6. Switch selected MicroHs build targets from the C runtime to the Rust runtime.

## Performance Gates

Every tier should carry benchmarks alongside correctness checks. Tier 0 includes
`mhs-rust-bench`, a dependency-free microbenchmark for parsing and reducing
`.comb` programs:

```sh
cargo run --release --bin mhs-rust-bench -- --scenario identity-chain:1000 --iters 1000
cargo run --release --bin mhs-rust-bench -- --scenario arith-chain:200 --iters 1000
cargo run --release --bin mhs-rust-bench -- --scenario int64-chain:200 --iters 1000
cargo run --release --bin mhs-rust-bench -- --scenario float64-chain:200 --iters 1000
cargo run --release --bin mhs-rust-bench -- --scenario float32-chain:200 --iters 1000
cargo run --release --bin mhs-rust-bench -- --scenario bytes-chain:200 --iters 1000
cargo run --release --bin mhs-rust-bench -- --scenario unpack-chain:200 --iters 1000
cargo run --release --bin mhs-rust-bench -- --scenario fromutf8-chain:200 --iters 1000
cargo run --release --bin mhs-rust-bench -- --scenario array-chain:200 --iters 1000
cargo run --release --bin mhs-rust-bench -- --scenario zoo-chain:300 --iters 1000
cargo run --release --bin mhs-rust-bench -- --scenario data-chain:300 --iters 1000
cargo run --release --bin mhs-rust-bench -- --input path/to/file.comb --iters 100
cargo run --release --bin mhs-rust-bench -- --scenario identity-chain:1000 --iters 100 \
  --c-mhseval ./bin/mhseval
```

With `--c-mhseval`, the benchmark runs the C evaluator on the same `.comb`
input using `+RTS -rFILE -o/dev/null -RTS`. That measures C process
parse/load/serialize wall time; it is useful as an early baseline, but it is not
yet an in-process C reduction benchmark. Rust `reduce_steps` currently counts
outer WHNF rewrites; strict primitive argument evaluation is included in elapsed
time but not counted as separate steps. Later tiers should add C-runtime oracle
comparisons on the same `.comb` inputs, then native and wasm timing once those
runtime surfaces exist.
