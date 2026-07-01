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
  Marshalling: `I`nt (also `Word`/`Char`) / `D`ouble / `F`loat / `P`tr / `B`ool /
  `J`SVal / `V`oid (unit, return only).
- **JS→Haskell event-pump.** `mhs_invoke_int` (exposed to JS as
  `globalThis.__mhs.invoke(sp, v)`) runs a Haskell `StablePtr (Int -> IO ())`
  callback as a *fresh* evaluator thread. A page hands a callback to JS by
  `newStablePtr`+`castStablePtrToPtr` and installs a listener whose body calls
  `__mhs.invoke`. See `PixiEvents.hs` — a canvas click rotates the box from
  Haskell, long after `main` returned.
- **`foreign import javascript "wrapper"` (typed JS→Haskell functions).** Turns a
  Haskell closure into a JS function value: `mkCB :: (a -> .. -> IO r) -> IO JSVal`
  interns a JS function (returned as a `JSVal`) that marshals its arguments (by the
  Phase A tags), runs the closure, and returns the result — multi-arg and
  result-returning, unlike the fixed `Int -> IO ()` pump.  Synchronous: the call
  runs to completion (on a fresh thread post-`main`, or the current stack if
  re-entered during evaluation).  The closure's `StablePtr` lives until exit (JS
  may retain the function after the `JSVal` is collected), so there is no auto-free
  yet.  This is the interpreter-model analogue of a static `foreign export
  javascript` (which MicroHs can't do without per-program `emcc`).
- **First-class `JSVal` (GC-managed object handles).** A body can take and return
  `JSVal` (`Mhs.JavaScript`) directly: on return the runtime interns the JS value
  into `globalThis.__mhs.obj` and wraps the handle in a `ForeignPtr` whose
  finalizer (`mhs_js_obj_free`) frees the slot on GC; as an argument it derefs the
  handle back to the JS value. So the type is opaque and runtime-owned (no raw
  handle to mismanage, no double-wrap), JS values are released on GC instead of
  leaking, and the handle array is bounded by the peak number of live `JSVal`s
  (freed slots reused via a freelist). This reuses MicroHs's existing
  ForeignPtr/finalizer machinery.

## What it deliberately does NOT do (vs GHC's wasm JSFFI)

| Capability | GHC wasm | This bridge |
|---|---|---|
| Async / `await` a JS Promise | yes — forcing a thunk suspends the *thread*, other threads + GC continue | **not on this runtime** — `main` runs to completion synchronously.  Planned as an opt-in `-sASYNCIFY` runtime on a separate branch (`EM_ASYNC_JS` + an async token); kept off the default because Asyncify measured ~2x FFI slowdown, and the await must unwind the whole eval loop |
| `foreign export javascript` (general JS→HS) | yes (async default, `sync` opt-in, `"wrapper"`) | **`"wrapper"` done** — typed, multi-arg, result-returning closures→JS functions (sync only); static named `foreign export` not done (needs per-program `emcc`) |
| `JSVal` with GC lifetime | GC-managed, `FinalizationRegistry` frees the JS slot | **done** — first-class `JSVal` (`Mhs.JavaScript`), runtime-owned ForeignPtr + finalizer, freed on GC; take/return directly in a body |
| Marshalling breadth | Bool, Char, all Int/Word incl **Int64→`bigint`**, Ptr/FunPtr/StablePtr, JSVal, ByteArray# | Int/Word/Char, Double, Float, Ptr, **Bool**, **JSVal**; strings via manual `Ptr`+UTF8 helpers. No Int64/Word64 (→`bigint`) yet; `Word`≥2³¹ rides the signed `I` path (unsigned `U` tag deferred) |
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

1. **Richer marshalling** — ~~`Bool`/`Char`, first-class `JSVal`~~ *(done: `Bool`
   via the real `K`/`A` combinators, `Char` via the `Word`/`I` path, and
   runtime-owned first-class `JSVal`)*. Remaining: `Int64`/`Word64` → `bigint`,
   an unsigned `U` tag for honest `Word`≥2³¹, and typed callback ABIs beyond `Int`.
2. ~~**General JS→Haskell exports**, **result-returning / multi-arg callbacks**~~
   *(done: `foreign import javascript "wrapper"` — typed, multi-arg,
   result-returning closures→JS functions, via the `ffe_*` machinery).*  Still
   open: static named `foreign export javascript` (needs per-program `emcc`, so
   incompatible with the in-browser interpreter), and freeing a wrapper's
   `StablePtr` (needs JS-side finalization or an explicit `freeJSVal`).
3. **Async** (the hard one, *on a separate branch*) — let `IO` `await` a Promise.
   The plan: an opt-in `-sASYNCIFY` runtime (`mheval-async.js`) *alongside* the
   fast default, with an async JS-FFI token and an `EM_ASYNC_JS` helper that
   suspends the C stack across the `await`.  Kept off the default because Asyncify
   measured ~2x on FFI-heavy work and +6% size, and the await must unwind the
   whole eval loop (so `ASYNCIFY_ONLY` can't spare the hot path).  Needs a
   no-reentry-while-suspended rule (browser events can fire mid-await).  JSPI is a
   future alternative (lower overhead, but still experimental / less portable).
4. **Catchable JS exceptions** — map a JS exception to a catchable Haskell
   exception (at least on the async path; the sync path matches GHC's fatal one).
5. **Avoid `unsafe-eval`** — precompile bodies or use a CSP nonce, for sites
   that forbid `new Function`.

## Version note for reviewers

`combVersion` was intentionally **not** bumped for the `~` token: `checkversion`
is exact-match, so a bump invalidates *every* `.comb` (including the checked-in
bootstrap) for one additive, optional token. Left for the maintainer to decide.
