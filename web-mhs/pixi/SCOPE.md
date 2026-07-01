# MicroHs WASM JavaScript FFI — scope, limitations, and roadmap

This documents *exactly* what the `foreign import javascript` bridge does and,
deliberately, what it does **not** do — so it isn't mistaken for GHC's wasm
backend. It is a small, honest, synchronous FFI for the MicroHs combinator
interpreter running under emscripten, plus a `StablePtr` event-pump so JS events
can drive Haskell.

## What it is

- **Program as data, not code.** MicroHs compiles a program to a combinator
  graph (`.comb`), interpreted by the C runtime (`eval.c`) that is itself
  compiled to wasm *once* by emscripten. There is **no per-program wasm codegen
  and no RTS port** — that is the whole reason this was tractable. (MicroHs's
  `-temscripten` target *does* do per-program C→wasm with static `EM_ASM` JS
  FFI, but it needs `emcc` at compile time, so it cannot run in the browser. We
  chose the interpreter + dynamic bridge precisely to get *in-browser*
  compilation.)
- **Dynamic JS imports.** `foreign import javascript "body"` is serialized as a
  self-describing token `~<tags> "<body>"`; at load time the runtime compiles
  the body with `new Function` into an append-only registry
  (`globalThis.__mhs.reg`), and a `T_IO_JSCALL` node dispatches it by tags.
  Marshalling is scalar: `I`nt / `D`ouble / `F`loat / `P`tr / `V`oid(unit).
- **JS→Haskell event-pump.** `mhs_invoke_int` (exposed to JS as
  `globalThis.__mhs.invoke(sp, v)`) runs a Haskell `StablePtr (Int -> IO ())`
  callback as a *fresh* evaluator thread. A page hands a callback to JS by
  `newStablePtr`+`castStablePtrToPtr` and installs a listener whose body calls
  `__mhs.invoke`. See `PixiEvents.hs` — a canvas click rotates the box from
  Haskell, long after `main` returned.
- **GC-managed object handles (`JSVal`).** A `JSVal` (`JSVal.hs`) is a
  `ForeignPtr` wrapping a handle into `globalThis.__mhs.obj`; a JS body allocates
  a handle with `__mhs.intern(value)` and returns it, `newJSVal` attaches a
  finalizer (`mhs_js_obj_free`) that frees the slot when the `JSVal` is collected.
  So JS values are released on GC instead of leaking, and the handle array is
  bounded by the peak number of simultaneously-live `JSVal`s (freed slots are
  reused via a freelist). This reuses MicroHs's existing ForeignPtr/finalizer
  machinery — the same path as `closeb`/`free`.

## What it deliberately does NOT do (vs GHC's wasm JSFFI)

| Capability | GHC wasm | This bridge |
|---|---|---|
| Async / `await` a JS Promise | yes — forcing a thunk suspends the *thread*, other threads + GC continue | **no** — `main` runs to completion synchronously; no yielding to the event loop |
| `foreign export javascript` (general JS→HS) | yes (async default, `sync` opt-in, `"wrapper"`) | only the narrow `Int -> IO ()` event-pump; no general export |
| `JSVal` with GC lifetime | GC-managed, `FinalizationRegistry` frees the JS slot | **done** — `JSVal` (ForeignPtr + finalizer) frees the JS slot on GC (`JSVal.hs`); not yet a first-class *marshallable* FFI type (wrap/unwrap manually via `newJSVal`/`withJSVal`) |
| Marshalling breadth | Bool, Char, all Int/Word incl **Int64→`bigint`**, Ptr/FunPtr/StablePtr, JSVal, ByteArray# | scalars only; strings via manual `Ptr`+UTF8 helpers; no Bool/Char/Int64/JSVal |
| Catchable JS exceptions | async path → `JSException` in Haskell | fatal (sets `__mhs.err`, `ERR`s) — this *matches* GHC's *sync* path |

**Deployment caveat (CSP).** The dynamic bridge uses `new Function` at load
time, which requires Content-Security-Policy `unsafe-eval`. Sites that forbid
`unsafe-eval` will block it. GHC's post-link glue does not runtime-eval.

## Correctness envelope

- **Synchronous scalar calls during `main`** are safe, conditional on: JS does
  not retain a raw Haskell heap `Ptr` past the call (the GC is non-moving, so a
  pointer is stable *during* the call, but a heap object referenced only by JS
  is not a GC root), and JS does not re-enter Haskell mid-call.
- **The event-pump is error-isolated and reentrancy-guarded.** Each callback
  runs via a fresh `start_exec`, so it has a live scheduler and its own
  `setjmp` boundary — an uncaught exception cleanly `EXIT`s the runtime instead
  of long-jumping into `main`'s returned frame. If the evaluator is already
  active (`main_thread != 0`), an invoke is dropped and returns 0 (the
  interpreter is not reentrant). The callback is fire-and-forget `IO ()`; a
  result is side-channelled via `IORef` if needed.
- **Registry is append-only** (never reset): `parse_top` is re-entered by
  `IO.deserialize`, so resetting would wipe a live registry. Every index stays
  valid for the life of the runtime.

## Roadmap (in rough order of value/effort)

1. **Richer marshalling** — `Bool`/`Char`, typed callback ABIs beyond `Int`, and
   making `JSVal` a first-class *marshallable* FFI type (a body can take/return a
   `JSVal` directly, rather than the handle `Int` + manual `newJSVal`/`withJSVal`).
   *(Done: the GC-managed object handle itself — `JSVal` frees the JS slot on
   collection, reusing the ForeignPtr/finalizer machinery — so handles no longer
   leak.)*
2. **General `foreign export javascript`** — a first-class export path (the
   `ffe_*`/`apply_sp` machinery already re-enters the evaluator) rather than the
   single hand-rolled `Int -> IO ()` pump.
3. **Result-returning / multi-arg callbacks** — generalise `mhs_invoke_int`
   beyond a single `Int` and fire-and-forget.
4. **Async** (the hard one) — let `IO` `await` a Promise. Either instrument the
   interpreter with emscripten Asyncify (or target JSPI) so a reduction can
   yield, or make the evaluator continuation-based and yield at `T_IO_JSCALL`,
   plus a JS-side scheduler and a sync/async tag distinction. This changes the
   "`main` runs to completion" contract and inherits continuation capture, async
   exceptions, and GC-while-suspended.
5. **Avoid `unsafe-eval`** — precompile bodies or use a CSP nonce, for sites
   that forbid `new Function`.

## Version note for reviewers

`combVersion` was intentionally **not** bumped for the `~` token: `checkversion`
is exact-match, so a bump invalidates *every* `.comb` (including the checked-in
bootstrap) for one additive, optional token. Left for the maintainer to decide.
