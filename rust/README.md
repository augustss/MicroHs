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
cargo run --release --bin mhs-rust-bench -- --scenario cstring-pack:200 --iters 1000
cargo run --release --bin mhs-rust-bench -- --scenario unpack-chain:200 --iters 1000
cargo run --release --bin mhs-rust-bench -- --scenario fromutf8-chain:200 --iters 1000
cargo run --release --bin mhs-rust-bench -- --scenario foreignptr-slice:200 --iters 1000
cargo run --release --bin mhs-rust-bench -- --scenario array-chain:200 --iters 1000
cargo run --release --bin mhs-rust-bench -- --scenario io-chain:200 --iters 1000
cargo run --release --bin mhs-rust-bench -- --scenario io-array-chain:200 --iters 1000
cargo run --release --bin mhs-rust-bench -- --scenario io-bytes-chain:200 --iters 1000
cargo run --release --bin mhs-rust-bench -- --scenario io-control-chain:200 --iters 1000
cargo run --release --bin mhs-rust-bench -- --scenario argref-chain:200 --iters 1000
cargo run --release --bin mhs-rust-bench -- --scenario stdio-chain:200 --iters 1000
cargo run --release --bin mhs-rust-bench -- --scenario ffi-chain:200 --iters 1000
cargo run --release --bin mhs-rust-bench -- --scenario mvar-chain:200 --iters 1000
cargo run --release --bin mhs-rust-bench -- --scenario ptr-chain:200 --iters 1000
cargo run --release --bin mhs-rust-bench -- --scenario rnf-chain:200 --iters 1000
cargo run --release --bin mhs-rust-bench -- --scenario stableptr-chain:200 --iters 1000
cargo run --release --bin mhs-rust-bench -- --scenario weak-chain:200 --iters 1000
cargo run --release --bin mhs-rust-bench -- --scenario zoo-chain:300 --iters 1000
cargo run --release --bin mhs-rust-bench -- --scenario data-chain:300 --iters 1000
cargo run --release --bin mhs-rust-bench -- --input path/to/file.comb --iters 100
cargo run --release --bin mhs-rust-bench -- --scenario identity-chain:1000 --iters 100 \
  --c-mhseval ./bin/mhseval
make bin/mhsbench
cargo run --release --bin mhs-rust-bench -- --scenario identity-chain:1000 --iters 100 \
  --c-mhsbench ./bin/mhsbench
```

With `--c-mhseval`, the benchmark runs the C evaluator on the same `.comb`
input using `+RTS -rFILE -o/dev/null -RTS`. That measures C process
parse/load/serialize wall time; keep it only as a legacy baseline. With
`--c-mhsbench`, the benchmark runs `bin/mhsbench` once and the C runtime loops
over parse, eval, and serialize in-process. The Rust runner passes
`--mode whnf` by default, so C evaluates the same expression-shaped benchmark to
WHNF that Rust evaluates. Use `--c-mhsbench-mode main` only for a real MicroHs
main program that expects the runtime `World` argument. The comparable C number
is `c_parse_eval_serialize_ns_per_iter`; the comparable Rust number is
`parse_reduce_render_ns_per_iter`. Rust `whnf_steps` currently counts outer WHNF
rewrites; strict primitive argument evaluation is included in elapsed time but
not counted as separate steps.
