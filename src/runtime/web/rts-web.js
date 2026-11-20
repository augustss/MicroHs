// The browser runtime (target browser_rts): instead of running the program
// compiled into it, fetch a combinator program and run that.
//
//   mhs -tbrowser_rts [--embed-ffis PKG:...] -o rts.js Main src/runtime/web/rts-web.js
//   mhs -o app.comb Main
//
// rts.js runs app.comb (or Module.combUrl, the data-comb attribute of its
// script tag, or in node the MHS_COMB environment variable), as long as the foreign imports of the program are in the
// FFI table of rts.js: those of the program it was built with, and of the
// packages of --embed-ffis.  Rebuilding app.comb needs no C compiler.
(function () {
  var script = typeof document !== "undefined" ? document.currentScript : null;
  var env = typeof process !== "undefined" && process.env ? process.env.MHS_COMB : null;
  var url = Module.combUrl || (script && script.getAttribute("data-comb")) || env || "app.comb";
  Module.noInitialRun = true;
  var ready = Module.onRuntimeInitialized;
  Module.onRuntimeInitialized = function () {
    if (ready) ready();
    // In a browser fetch the program; in node (for tests) read the file.
    var load = typeof fetch === "function" && typeof document !== "undefined"
      ? fetch(url, { cache: "no-store" }).then(function (r) {
          if (!r.ok) throw new Error(r.status + " " + r.statusText);
          return r.arrayBuffer();
        })
      : Promise.resolve().then(function () { return require("fs").readFileSync(url); });
    load.then(function (buf) {
      var bytes = new Uint8Array(buf);
      var p = _mhs_js_alloc(bytes.length);
      HEAPU8.set(bytes, p);
      try {
        _mhs_run_web(p, bytes.length);
      } catch (e) {
        // The program exiting is not an error (emscripten unwinds with it).
        if (e !== "unwind" && !(e && e.name === "ExitStatus")) throw e;
      }
    }, function (e) {
      console.error("mhs: cannot load " + url + ": " + e);
    });
  };
})();
