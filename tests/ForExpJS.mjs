// Driver for testforexpjs: calls the EMSCRIPTEN_KEEPALIVE exports of the
// emcc-compiled ForExpJS module (fejs.mjs, built by the Makefile rule).
const FEJS = (await import('./fejs.mjs')).default;
const m = await FEJS();
m._mhs_init();
console.log(m._addOne(41));
console.log(m._isPos(3), m._isPos(-3));
console.log(m._scale(2.5, 4));
