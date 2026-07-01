# MicroHs WASM JS-FFI — downstream wishlist

Asks from a real downstream integration: **Combinate** (a Pixi.js ι/SKI combinator
sandbox) now runs its functional core as Haskell compiled by MicroHs to combinators and
executed in the WASM interpreter, driving the app over the `foreign import javascript`
bridge. This is what that integration would still like from MicroHs, in priority order.

Complements [`web-mhs/pixi/SCOPE.md`](web-mhs/pixi/SCOPE.md) (what the FFI does / doesn't) —
this is "what a consumer hit and wanted," with the concrete use case for each.

Legend: **Why** = the Combinate use case · **Now** = current state · **Scope** = rough
effort + code pointers.

---

## 1. String / `JSVal`-string marshalling (or a blessed `CString` helper) — HIGH

> **Delivered** (the fuller option): a `ByteString` marshalling tag (`S`) now moves
> a `Data.ByteString.ByteString` to/from a JS string (UTF-8), for both bodies and
> `"wrapper"` callbacks — `reduce :: ByteString -> IO ByteString` with no files.
> Packed (O(1) pointer+length, no `[Char]` cons-list), embedded-`U+0000`-safe. See
> `web-mhs/pixi/SCOPE.md` "ByteString strings". Text bridge; raw bytes (`Uint8Array`)
> deferred.

**Why.** Combinate marshals a serialized term across the boundary. Because the bridge
carries no strings, we use the **WASM filesystem as a side-channel**: JS `FS.writeFile("/in", wire)`,
a Haskell callback `readFile`s it, reduces, `writeFile`s `/out`, JS `FS.readFile`s it back.
It works and is fast, but it's a workaround for "pass a string in, get a string out." The
natural API is a callback `reduce :: JSString -> IO JSString` (or `String -> IO String`),
with no files.

**Now.** SCOPE.md lists strings as "manual `Ptr` + UTF8 helpers." The runtime already
exports `stringToNewUTF8` / `UTF8ToString`, and `JSVal` + `"wrapper"` are in — so most of
the plumbing exists; what's missing is a first-class way to move a Haskell `String`
(a lazy `[Char]` cons-list in the interpreter) to/from a JS string.

**Scope.** Two options:
- *Lighter:* document + smoke-test the `CString` path — a body/callback of type
  `Ptr CChar -> IO (Ptr CChar)` with Haskell-side `peekCString`/`newCString` (via
  `Foreign.C.String`) and JS-side `UTF8ToString`/`stringToNewUTF8`. Mostly docs + an
  example if it already works; verifies the "manual" path is actually usable.
- *Fuller:* a new marshalling tag (say `'S'`) that walks a `[Char]` to build a JS string on
  return, and builds a `[Char]` from a JS string as an argument. Touches `src/MicroHs/ExpPrint.hs`
  (`jsScalarTag`), `src/MicroHs/FFI.hs`, and `src/runtime/eval.c` (the `mhs_js_*` glue near
  the `T_IO_JSCALL` dispatch). The arg direction must allocate `Char` cons-cells in the heap —
  the fiddly bit; the return direction can reuse the string-building path used by `putStr`.

This is the single change that would most improve FFI ergonomics for "real data" consumers.

## 2. A result-returning event-pump for strings — HIGH (folds into #1)

> **Delivered** via #1 + `"wrapper"`: a `"wrapper"` `reduce :: ByteString -> IO
> ByteString` returns its result directly (as a JS string), so both the `/in` and
> `/out` files disappear.

**Why.** Our reduce callback is registered as a `StablePtr (Int -> IO ())` and invoked via
`__mhs.invoke` (great — this is what made the persistent runtime possible, see below). But
`invoke` is fire-and-forget `IO ()`, so the *result* still goes out via the `/out` file.
The `"wrapper"` primitive already returns a value (as `JSVal`) — so the missing piece is
purely #1 (strings). With #1, a `"wrapper"` `reduce :: JSString -> IO JSString` would
replace both the `/in` and `/out` files.

**Now.** `"wrapper"` (closure → JS function, multi-arg, result-returning) is done;
`__mhs.invoke` (fire-and-forget `Int -> IO ()`) is done. Only string I/O is missing.

**Scope.** No new mechanism — this is #1 applied to `"wrapper"`. Listed separately only to
say: once #1 lands, the FS side-channel disappears entirely.

## 3. Mid-evaluation reentrancy — MEDIUM

**Why.** A future "reduce *during* another reduction" design (e.g. a callback that itself
triggers a sub-reduction, or streaming that re-enters) would need it.

**Now.** SCOPE.md documents it: an `invoke` while the evaluator is active (`main_thread != 0`)
is **dropped and returns 0** — "the interpreter is not reentrant." Combinate never hits this
(JS is single-threaded; each `invoke` runs to completion before the next), so it's not
blocking us — but it's a sharp edge worth a louder warning or a queue.

**Scope.** Either (a) document the constraint more prominently (cheap), or (b) queue a
dropped invoke and drain it when the evaluator goes idle (`src/runtime/eval.c`, the
`mhs_invoke_int` path near the `main_thread` guard). (b) is real runtime work.

## 4. Document the runtime's FS / driving contract — MEDIUM (cheap, high leverage)

**Why.** We reverse-engineered the whole driving protocol from the demos:
`FS.writeFile('/prog.comb', bytes)` → `callMain(['+RTS','-r/prog.comb','-RTS'])` (runs
`main`, and — crucially — **returns** because there's no `EXIT_RUNTIME`) → then
`FS.writeFile('/in', …)` + `__mhs.invoke(sp, v)` + `FS.readFile('/out', {encoding:'utf8'})`.
Also non-obvious: `.comb` must be loaded as **binary**; `__mhs.invoke` is only present when
`_mhs_invoke_int` is exported; the callback's `StablePtr` must be published to JS by the
program itself (e.g. `foreign import javascript "globalThis.__x = $0" :: Ptr () -> IO ()`).

**Now.** The demos show it by example; there's no one-page "how to drive the runtime blob
from a host" contract.

**Scope.** A short `web-mhs/DRIVING.md` (or a section in SCOPE.md). Pure docs.

## 5. Document / ship a Node usage of the MODULARIZE blob — LOW (cheap)

**Why.** `web-mhs/pixi/mhseval-web.js` (`MODULARIZE`, `EXPORT_NAME=MhsEval`, `SINGLE_FILE`)
runs **unmodified under Node** via `createRequire(...)` + `await MhsEval({...})` — we use
exactly that for headless CI/oracle/benchmark harnesses (no browser needed). Worth
documenting; `generated/mhseval-node.js` is a non-MODULARIZE auto-run build that's awkward
to drive by comparison.

**Scope.** A one-paragraph note + a 15-line Node snippet. Pure docs.

## 6. Smaller / lazily-loaded runtime blob — LOW

**Why.** The runtime blob is ~670 KB (`SINGLE_FILE` base64-embeds the wasm). Fine for one
persistent instance; it mattered more under the old "fresh instance per call" model (now
obsolete — see below). Still, a slimmer runtime or split wasm would help cold start / mobile.

**Scope.** Build-flag exploration (drop `SINGLE_FILE`, gzip, tree-shake base). Not blocking.

## 7. `gmhs -z` / `.pkg` serialization — LOW (blocks a stretch goal)

**Why.** The GHC-built `gmhs` can't serialize a `.pkg` (`-z`), so the in-browser compiler
build (`mhs-compile.js`) embeds base as source (`--embed-file`). Not on Combinate's current
path (we compile the core host-side), but it's the blocker for a live "compile Haskell in
the browser → run it" experience.

**Scope.** Compiler work; out of scope for us to estimate.

## 8. Marshalling breadth: `Int64`/`Word64` → `bigint` — LOW

**Why.** Completeness. SCOPE.md notes 64-bit ints are boxed on wasm32 and **error at
compile** in a `foreign import javascript`. Combinate doesn't need them (Char codes,
handles, and step counts fit in `Int`), but a consumer doing 64-bit arithmetic would.

**Scope.** Compiler + runtime; a `bigint` marshalling tag.

---

## Already delivered by `wasm-js-ffi` (thank-you notes — these unblocked us)

- **Persistent runtime (no `EXIT_RUNTIME`) + `_mhs_invoke_int` / `__mhs.invoke` +
  `StablePtr` callbacks.** This was *the* win. Combinate's reducer now instantiates the
  runtime **once**, registers a reduce `StablePtr`, and re-enters it per reduction — the
  browser per-reduce floor dropped from **~110 ms to ~0.5 ms** (it was dominated by
  re-instantiating the WASM module + reloading the `.comb` on every call). This turned the
  Haskell core from a batch oracle into something fast enough to drive interactively.
- **First-class `JSVal`** (`Mhs.JavaScript`) and **Bool/Word/Char (`'U'`) marshalling** —
  richer than the original Int/Double-only bridge; the handle-registry + finalizer model is
  exactly right (no raw handles to mismanage).
- **`"wrapper"`** (closure → JS function) — the general JS→Haskell mechanism; once strings
  land (#1) it becomes our whole boundary.
