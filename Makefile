#
CCWARNS= -Wall -Wno-deprecated-declarations
CCOPTS= -O3
CCLIBS= -lm
#
GHC= ghc
GHCINCS= -ighc -isrc
GHCWARNS= -Wall -Wno-unrecognised-warning-flags -Wno-x-partial
GHCOPTS= -O
GHCEXTS= -XScopedTypeVariables -XTupleSections
GHCPKGS= -package mtl -package pretty
GHCTOOL= -F -pgmF Tools/convertX.sh
GHCOUT= -outputdir ghc-out
GHCPROF= # -prof -fprof-late #-prof -fprof-auto
GHCFLAGS= $(GHCEXTS) $(GHCINCS) $(GHCWARNS) $(GHCOPTS) $(GHCTOOL) $(GHCPKGS) $(GHCOUT) $(GHCPROF)

# Compile mhs from distribution, with C compiler
bin/mhs:	src/runtime/eval.c src/runtime/config*.h #generated/mhs.c
	@mkdir -p bin
	$(CC) $(CCWARNS) $(CCOPTS) src/runtime/eval.c generated/mhs.c $(CCLIBS) -o bin/mhs

# Compile mhs with ghc
bin/gmhs:	src/*/*.hs Tools/convertX.sh
	@mkdir -p bin
	$(GHC) $(GHCFLAGS)  src/MicroHs/Main.hs -main-is MicroHs.Main -o bin/gmhs

generated/mhs.c:	bin/mhs src/*/*.hs
	@mkdir -p generated
	bin/mhs -ilib -isrc -ogenerated/mhs.c MicroHs.Main
