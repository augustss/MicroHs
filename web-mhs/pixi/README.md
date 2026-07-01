# MicroHs + pixi.js in the browser

Demonstrates the dynamic JavaScript FFI bridge: a Haskell program that uses
`foreign import javascript` is compiled to a combinator file and run by the
MicroHs WASM runtime, driving [pixi.js](https://pixijs.com/) — with no
per-program C compilation.

- `index.html` — runs a program compiled **host-side** by the runtime WASM.
- `compile.html` — compiles the Haskell **in the browser** (compiler WASM),
  then runs it (runtime WASM). Both stages are WASM; nothing is compiled on a
  server.
- `PixiDemo.hs` — the example program (create a box, add it to the stage, spin it).

The `.js`/`.wasm`/`.comb` build products are gitignored. Build them (with
[emscripten](https://emscripten.org/) on `PATH`, e.g. `. ~/emsdk/emsdk_env.sh`)
from the repo root:

```sh
# 1. a working self-hosted compiler with the JS-FFI changes
make newmhs

# 2. the runtime, as a browser WASM module (EXPORT_NAME=MhsEval)
emcc -O3 -sEXPORTED_FUNCTIONS=_apply_sp,_main \
     -sEXPORTED_RUNTIME_METHODS=FS,callMain,stringToNewUTF8,UTF8ToString \
     -sFORCE_FILESYSTEM=1 -sALLOW_MEMORY_GROWTH -sTOTAL_STACK=5MB -sSINGLE_FILE \
     -sMODULARIZE=1 -sEXPORT_NAME=MhsEval -Wno-address-of-packed-member \
     -Isrc/runtime -Isrc/runtime/unix \
     src/runtime/main.c src/runtime/eval.c src/runtime/comb.c -lm \
     -o web-mhs/pixi/mhseval-web.js

# 3. the compiler, as a browser WASM module (in-browser compilation)
bin/mhs -temscripten_compile -z -imhs -isrc MicroHs.Main \
        -oweb-mhs/pixi/mhs-compile.js --embed-packages generated/base.pkg

# 4. (for index.html only) the host-compiled program
bin/mhs -iweb-mhs/pixi PixiDemo -oweb-mhs/pixi/PixiDemo.comb

# serve and open http://localhost:8000/pixi/compile.html
cd web-mhs && python3 -m http.server 8000
```

Note: the compiled `.comb` must be loaded into the WASM filesystem as **binary**
(`fetch(...).arrayBuffer()` / `FS.readFile`), never as text — it contains raw
bytes that a UTF-8 text decode would corrupt.
