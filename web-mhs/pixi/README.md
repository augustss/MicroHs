# MicroHs + pixi.js in the browser

Demonstrates the dynamic JavaScript FFI bridge: a Haskell program that uses
`foreign import javascript` is compiled to a combinator file and run by the
MicroHs WASM runtime, driving [pixi.js](https://pixijs.com/) — with no
per-program C compilation.

- `index.html` — runs a program compiled **host-side** by the runtime WASM.
- `compile.html` — compiles the Haskell **in the browser** (compiler WASM),
  then runs it (runtime WASM). Both stages are WASM; nothing is compiled on a
  server.
- `events.html` — **JS events driving Haskell**: a canvas click re-enters a
  Haskell callback (registered as a `StablePtr`, invoked via
  `globalThis.__mhs.invoke`) that rotates the box — long after `main` returned.
- `PixiDemo.hs` — the example program (create a box, add it to the stage, spin it).
- `PixiEvents.hs` — the event-driven example (click → Haskell rotates the box).
A `foreign import javascript` can take/return `Bool`, `Int`/`Word`/`Char`,
`Double`/`Float`, `Ptr`, and `JSVal` (`import Mhs.JavaScript`) — the last a
GC-managed, opaque reference to a JS value whose underlying object is freed when
the `JSVal` is collected. See [`SCOPE.md`](SCOPE.md) for the full tag set.

See [`SCOPE.md`](SCOPE.md) for exactly what this FFI does and, deliberately,
what it does not (vs GHC's wasm JSFFI) — read it before assuming parity.

The `.js`/`.wasm`/`.comb` build products are gitignored. Build them (with
[emscripten](https://emscripten.org/) on `PATH`, e.g. `. ~/emsdk/emsdk_env.sh`)
from the repo root:

```sh
# 1. a working self-hosted compiler with the JS-FFI changes
make newmhs

# 2. the runtime, as a browser WASM module (EXPORT_NAME=MhsEval).
#    _mhs_invoke_int is exported for the JS->Haskell event-pump (events.html);
#    there is no -sEXIT_RUNTIME, so the instance stays alive for post-main callbacks.
emcc -O3 -sEXPORTED_FUNCTIONS=_mhs_invoke_int,_mhs_wrapper_invoke,_main \
     -sEXPORTED_RUNTIME_METHODS=FS,callMain,stringToNewUTF8,UTF8ToString \
     -sFORCE_FILESYSTEM=1 -sALLOW_MEMORY_GROWTH -sTOTAL_STACK=5MB -sSINGLE_FILE \
     -sMODULARIZE=1 -sEXPORT_NAME=MhsEval -Wno-address-of-packed-member \
     -Isrc/runtime -Isrc/runtime/unix \
     src/runtime/main.c src/runtime/eval.c src/runtime/comb.c -lm \
     -o web-mhs/pixi/mhseval-web.js

# 3. the compiler, as a browser WASM module (in-browser compilation)
bin/mhs -temscripten_compile -z -imhs -isrc MicroHs.Main \
        -oweb-mhs/pixi/mhs-compile.js --embed-packages generated/base.pkg

# 4. (for index.html / events.html) the host-compiled programs
bin/mhs -iweb-mhs/pixi PixiDemo   -oweb-mhs/pixi/PixiDemo.comb
bin/mhs -iweb-mhs/pixi PixiEvents -oweb-mhs/pixi/PixiEvents.comb

# serve and open http://localhost:8000/pixi/compile.html
cd web-mhs && python3 -m http.server 8000
```

Note: the compiled `.comb` must be loaded into the WASM filesystem as **binary**
(`fetch(...).arrayBuffer()` / `FS.readFile`), never as text — it contains raw
bytes that a UTF-8 text decode would corrupt.

## Driving the runtime blob from Node (headless)

The runtime blob from step 2 (`mhseval-web.js` — `MODULARIZE`, `EXPORT_NAME=MhsEval`,
`SINGLE_FILE`) runs **unmodified under Node** — no browser — which is how a headless
CI / oracle / benchmark harness drives it. Instantiate it once, load a `.comb` into the
in-memory FS, and `callMain`; because the blob is built with no `-sEXIT_RUNTIME`, the
instance stays alive after `main` returns, so a `StablePtr` callback can be re-entered
via `globalThis.__mhs.invoke(sp, v)` for each later event (instantiate once, reduce
many times). By contrast `generated/mhseval-node.js` is a separate non-`MODULARIZE`,
auto-running `NODERAWFS` build that reads a host-path `.comb` (`+RTS -rFILE -RTS`) —
handy for a one-shot run, awkward to drive repeatedly.

```js
// drive.mjs — run:  node drive.mjs prog.comb
import { createRequire } from 'node:module';
import { readFileSync }  from 'node:fs';
const require = createRequire(import.meta.url);
const MhsEval = require('./mhseval-web.js');            // the MODULARIZE blob (step 2)

const out = [];
const m = await MhsEval({ noInitialRun: true, print: s => out.push(s) });
m.FS.writeFile('/prog.comb', readFileSync(process.argv[2]));  // .comb as raw bytes
m.callMain(['+RTS', '-r/prog.comb', '-RTS']);                 // runs main; then returns
console.log(out.join('\n'));
// the instance is still live here — e.g. globalThis.__mhs.invoke(sp, v) to re-enter.
```
