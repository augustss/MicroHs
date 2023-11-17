# installation prefix
PREFIX=/usr/local
#
CCWARNS= -Wall -Wno-deprecated-declarations
CCOPTS= -O3
CCLIBS= -lm
CCEVAL= $(CC) $(CCWARNS) $(CCOPTS) src/runtime/eval.c $(CCLIBS)
#
GHC= ghc
GHCINCS= -ighc -isrc
GHCWARNS= -Wall -Wno-unrecognised-warning-flags -Wno-x-partial
GHCOPTS= -O
GHCEXTS= -XScopedTypeVariables -XPatternGuards -XTupleSections -XTypeSynonymInstances -XFlexibleInstances
GHCPKGS= #-package mtl -package pretty -package temporary -package process
GHCTOOL= -F -pgmF Tools/convertX.sh
GHCOUTDIR= ghc-out
GHCOUT= -outputdir $(GHCOUTDIR)
GHCPROF= # -prof -fprof-late #-prof -fprof-auto
GHCFLAGS= $(GHCEXTS) $(GHCINCS) $(GHCWARNS) $(GHCOPTS) $(GHCTOOL) $(GHCPKGS) $(GHCOUT) $(GHCPROF)
#
.PHONY:	clean bootstrap install ghcgen

all:	bin/gmhs

# Compile mhs from distribution, with C compiler
bin/mhs:	src/runtime/eval.c src/runtime/config*.h #generated/mhs.c
	@mkdir -p bin
	$(CCEVAL) generated/mhs.c -o bin/mhs

# Compile combinator evaluator
bin/mhseval:	src/runtime/eval.c src/runtime/config*.h src/runtime/comb.c
	@mkdir -p bin
	$(CCEVAL) src/runtime/comb.c -o bin/mhseval

# Compile mhs with ghc
bin/gmhs:	src/*/*.hs Tools/convertX.sh
	@mkdir -p bin
	$(GHC) $(GHCFLAGS) src/MicroHs/Main.hs -main-is MicroHs.Main -o bin/gmhs

# Generate distribution C file
generated/mhs.c:	bin/mhs src/*/*.hs
	@mkdir -p generated
	bin/mhs -ilib -isrc MicroHs.Main -ogenerated/mhs.c

# Make sure boottrapping works
bootstrap:	bin/mhs-stage2
	@echo "*** copy stage2 to bin/mhs"
	cp bin/mhs-stage2 bin/mhs
	cp generated/mhs-stage2.c generated/mhs.c 

ghcgen:	bin/gmhs src/*/*.hs lib/*.hs lib/*/*.hs lib/*/*/*.hs
	bin/gmhs -ilib -isrc MicroHs.Main -ogenerated/mhs.c

# Build stage1 compiler with existing compiler
bin/mhs-stage1:	bin/mhs src/*/*.hs
	@mkdir -p generated
	@echo "*** Build stage1 compiler, using bin/mhs"
	bin/mhs -ilib -isrc MicroHs.Main -ogenerated/mhs-stage1.c
	$(CCEVAL) generated/mhs-stage1.c -o bin/mhs-stage1

# Build stage2 compiler with stage1 compiler, and compare
bin/mhs-stage2:	bin/mhs-stage1 src/*/*.hs
	@mkdir -p generated
	@echo "*** Build stage2 compiler, with stage1 compiler"
	bin/mhs-stage1 -ilib -isrc MicroHs.Main -ogenerated/mhs-stage2.c
	cmp generated/mhs-stage1.c generated/mhs-stage2.c
	@echo "*** stage2 equal to stage1"
	$(CCEVAL) generated/mhs-stage2.c -o bin/mhs-stage2

# Run test examples with ghc-compiled compiler
runtest:	bin/mhseval bin/gmhs tests/*.hs
	cd tests; make alltest

# Compress the binary (broken on MacOS)
bin/umhs: bin/mhs
	rm -f bin/umhs
	upx -q -q -obin/umhs bin/mhs
#
clean:
	rm -rf src/*/*.hi src/*/*.o *.comb *.tmp *~ bin/* a.out $(GHCOUTDIR) tmp/* Tools/*.o Tools/*.hi dist-newstyle generated/*-stage*
	cd tests; make clean

install:
	mkdir -p $(PREFIX)/bin
	cp bin/mhs $(PREFIX)/bin
	mkdir -p $(PREFIX)/lib/mhs/src/runtime
	cp -r lib $(PREFIX)/lib/mhs
	cp src/runtime/* $(PREFIX)/lib/mhs/src/runtime
	@echo "***"
	@echo "*** Installation complete"
	@echo "*** Set environment variable MHSDIR to $(PREFIX)/lib/mhs"
	@echo "***"

everytest:	runtest bootcombtest

bootcombtest:	bin/gmhs bin/mhseval
	bin/gmhs -ilib -isrc -ogmhs.comb  MicroHs.Main
	bin/mhseval +RTS -v -rgmhs.comb -RTS -ilib -isrc -omhs.comb MicroHs.Main
	cmp gmhs.comb mhs.comb
