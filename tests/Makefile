MHSTARGET=
MHSOUTPUT=
MHS=../bin/mhs
TMHS=$(MHS) $(MHSTARGET) $(MHSOUTPUT)
EVAL=../bin/mhseval +RTS -H1M -RTS
TESTER=../bin/tester
.PHONY: test nfib clean errtest alltest cache info
.PHONY: testpreprocessorpaths

alltest: info cache test errtest testforimp testforexp testforimpexp testapplysp testpipenonblockio testpreprocessorpaths
# interactivetest

cache:
	rm -f .mhscache
	MHSDIR=.. $(TMHS) -CW AllOfLib

info:
	MHSDIR=.. $(TMHS) -r Info

test:	cache
	MHSDIR=.. $(TESTER) all.tests

errtest: cache
	MHSDIR=.. $(TESTER) err.tests

interactivetest:
	$(TMHS) --stdin < itest.in | sed -e '1,/^mhs>/d' > itest.out && diff itest.ref itest.out


testpreprocessorpaths:
	sh preprocessor-paths.sh $(TMHS)

testforimp:
	$(TMHS) ForeignC -of.exe && ./f.exe > ForeignC.out && diff ForeignC.ref ForeignC.out

testforimpjs:
	$(TMHS) -temscripten ForImpJS -of.js && node f.js > ForImpJS.out && diff ForImpJS.ref ForImpJS.out

# `foreign export javascript`: the exports are EMSCRIPTEN_KEEPALIVE, so only
# mhs_init needs listing; the program is compiled as a library (no main).
testforexpjs:
	$(TMHS) -c ForExpJS -oForExpJS_c.c
	emcc -O1 -I../src/runtime -I../src/runtime/unix -DUSE_SYSTEM_RAW -Wno-address-of-packed-member ForExpJS_c.c ../src/runtime/eval.c -lm -sEXPORTED_FUNCTIONS=_mhs_init -sALLOW_MEMORY_GROWTH -sTOTAL_STACK=5MB -sSINGLE_FILE -sMODULARIZE=1 -sEXPORT_NAME=FEJS -ofejs.mjs
	node ForExpJS.mjs > ForExpJS.out && diff ForExpJS.ref ForExpJS.out && rm -f fejs.mjs ForExpJS_c.c

testforexp:
	$(TMHS) -c -optc --shared -optc -fPIC ForExp -oForExpHs.so && $(CC) -lm ForExp.c ForExpHs.so -Wl,-rpath,'$$ORIGIN' -o ForExp.exe && ./ForExp.exe > ForExp.out && diff ForExp.ref ForExp.out

testforimpexp:
	$(TMHS) -optc -I. Callback hsimp.c -oCallback.exe && ./Callback.exe > Callback.out && diff Callback.ref Callback.out

testapplysp:
	$(TMHS) -optc -I. ApplySP hsasp.c -oApplySP.exe && ./ApplySP.exe > ApplySP.out && diff ApplySP.ref ApplySP.out

testpipenonblockio:
	MHSDIR=.. $(TMHS) -oPipeNonBlockIO.exe PipeNonBlockIO && ./PipeNonBlockIO.exe > PipeNonBlockIO.out && diff PipeNonBlockIO.ref PipeNonBlockIO.out

testpipenonblockioem:
	$(TMHS) PipeNonBlockIO && $(EVAL) > PipeNonBlockIO.out && diff PipeNonBlockIO.ref PipeNonBlockIO.out

# A FILE.js argument embeds a JavaScript file in the output (--pre-js for emscripten)
testjsflag:
	cp JSFlag.pre jsflag.js
	$(TMHS) jsflag.js JSFlag && $(EVAL) > JSFlag.out && diff JSFlag.ref JSFlag.out && rm -f jsflag.js

# The .js files in the jsbits directory of a package are embedded too.
# Install JSBitsLib as package JSBits, with jsbits as MicroCabal installs js-sources.
testjsbits:
	rm -rf jsbitsdb
	$(MHS) -ijsbitspkg -PJSBits-1.0 -oJSBits-1.0.pkg JSBitsLib && $(MHS) -Q JSBits-1.0.pkg jsbitsdb
	mkdir -p jsbitsdb/packages/JSBits-1.0/jsbits && cp jsbitspkg/jsbitslib.pre jsbitsdb/packages/JSBits-1.0/jsbits/jsbitslib.js
	$(TMHS) -ajsbitsdb JSBits && $(EVAL) > JSBits.out && diff JSBits.ref JSBits.out && rm -rf jsbitsdb JSBits-1.0.pkg

nfib:
	$(TMHS) Nfib && $(EVAL)

clean:
	rm -f *.out *.tmp *.js *.exe *.so a.out *_stub.h .mhscache
