# installation prefix
PREFIX=/usr/local
# Unix-like system, 64 bit words
CONF=unix-64
#
CCWARNS= -Wall
CCOPTS= -O3
CCLIBS= -lm
CCEVAL= $(CC) $(CCWARNS) $(CCOPTS) -Isrc/runtime src/runtime/eval-$(CONF).c $(CCLIBS)
#
GHC= ghc
GHCINCS= -ighc -isrc
GHCWARNS= -Wall -Wno-unrecognised-warning-flags -Wno-x-partial
GHCOPTS= -O
GHCEXTS= -DNOTCABAL -XScopedTypeVariables -XPatternGuards -XTupleSections -XTypeSynonymInstances -XFlexibleInstances -XOverloadedRecordDot -XDisambiguateRecordFields -XOverloadedStrings
# -XOverloadedRecordUpdate
GHCPKGS= -package mtl -package pretty -package haskeline -package process -package time -package ghc-prim -package containers -package deepseq -package directory
GHCTOOL= # -F -pgmF Tools/convertX.sh
GHCOUTDIR= ghc-out
GHCOUT= -outputdir $(GHCOUTDIR)
GHCPROF= # -prof -fprof-late #-prof -fprof-auto
GHCFLAGS= $(GHCEXTS) $(GHCINCS) $(GHCWARNS) $(GHCOPTS) $(GHCTOOL) $(GHCPKGS) $(GHCOUT) $(GHCPROF)
#
.PHONY:	clean bootstrap install ghcgen newmhs cachelib timecompile exampletest cachetest runtest runtestmhs everytest everytestmhs nfibtest info

all:	bin/mhs bin/cpphs

newmhs:	ghcgen
	$(CCEVAL) generated/mhs.c -o bin/mhs
	$(CC) $(CCWARNS) -g -Isrc/runtime src/runtime/eval-$(CONF).c $(CCLIBS) generated/mhs.c -o bin/mhsgdb

# Compile mhs from distribution, with C compiler
bin/mhs:	src/runtime/*.c src/runtime/*.h #generated/mhs.c
	@mkdir -p bin
	$(CCEVAL) generated/mhs.c -o bin/mhs

# Compile cpphs from distribution, with C compiler
bin/cpphs:	src/runtime/*.c src/runtime/config*.h generated/cpphs.c
	@mkdir -p bin
	$(CCEVAL) generated/cpphs.c -o bin/cpphs

# Compile combinator evaluator
bin/mhseval:	src/runtime/*.c src/runtime/config*.h
	@mkdir -p bin
	$(CCEVAL) src/runtime/comb.c -o bin/mhseval
	size bin/mhseval

bin/mhsevalgdb:	src/runtime/*.c src/runtime/config*.h
	@mkdir -p bin
	$(CC) $(CCWARNS) -g src/runtime/eval-$(CONF).c $(CCLIBS) src/runtime/comb.c -o bin/mhsevalgdb

# Compile mhs with ghc
bin/gmhs:	src/*/*.hs ghc/*.hs ghc/*/*.hs ghc/*/*/*.hs
	@mkdir -p bin
	$(GHC) $(GHCFLAGS) src/MicroHs/Main.hs -main-is MicroHs.Main -o bin/gmhs

# Compile mhs with ghc, with code coverage
bin/cmhs:	src/*/*.hs ghc/*.hs ghc/*/*.hs
	@mkdir -p bin
	$(GHC) $(GHCFLAGS) -fhpc src/MicroHs/Main.hs -main-is MicroHs.Main -o bin/cmhs

# Generate distribution C file
generated/mhs.c:	bin/mhs src/*/*.hs
	@mkdir -p generated
	bin/mhs -z -isrc MicroHs.Main -ogenerated/mhs.c

ghcgen:	bin/gmhs src/*/*.hs lib/*.hs lib/*/*.hs lib/*/*/*.hs
	bin/gmhs -isrc MicroHs.Main -ogenerated/mhs.c

# Make sure boottrapping works
bootstrap:	bin/mhs-stage2
	@echo "*** copy stage2 to bin/mhs"
	cp bin/mhs-stage2 bin/mhs
	cp generated/mhs-stage2.c generated/mhs.c 

# Build stage1 compiler with existing compiler
bin/mhs-stage1:	bin/mhs src/*/*.hs
	@mkdir -p generated
	@echo "*** Build stage1 compiler, using bin/mhs"
	bin/mhs -z -isrc MicroHs.Main -ogenerated/mhs-stage1.c
	$(CCEVAL) generated/mhs-stage1.c -o bin/mhs-stage1

# Build stage2 compiler with stage1 compiler, and compare
bin/mhs-stage2:	bin/mhs-stage1 src/*/*.hs
	@mkdir -p generated
	@echo "*** Build stage2 compiler, with stage1 compiler"
	bin/mhs-stage1 -z -isrc MicroHs.Main -ogenerated/mhs-stage2.c
	cmp generated/mhs-stage1.c generated/mhs-stage2.c
	@echo "*** stage2 equal to stage1"
	$(CCEVAL) generated/mhs-stage2.c -o bin/mhs-stage2

cpphssrc/malcolm-wallace-universe:
	mkdir -p cpphssrc
	cd cpphssrc; git clone --branch dot-spaces git@github.com:augustss/malcolm-wallace-universe.git

bootstrapcpphs: cpphssrc/malcolm-wallace-universe #bin/cpphs
	MHSCPPHS=bin/cpphs bin/mhs -z -XCPP -icpphssrc/malcolm-wallace-universe/polyparse-1.12/src -icpphssrc/malcolm-wallace-universe/cpphs-1.20.9 cpphssrc/malcolm-wallace-universe/cpphs-1.20.9/cpphs.hs -ogenerated/cpphs.c

# Run test examples with ghc-compiled compiler
runtest:	bin/mhseval bin/gmhs tests/*.hs
	cd tests; make alltest

# Compress the binary (broken on MacOS)
bin/umhs: bin/mhs
	rm -f bin/umhs
	upx -q -q -obin/umhs bin/mhs

#
timecompile: bin/mhs
	time bin/mhs +RTS -v -RTS -isrc MicroHs.Main

#
timecachecompile: bin/mhs
	@-rm -f .mhscache
	time bin/mhs +RTS -v -RTS -CW AllOfLib
	time bin/mhs +RTS -v -RTS -CR -isrc MicroHs.Main

#
cachelib:
	@-rm -f .mhscache
	bin/mhs -CW AllOfLib

#
clean:
	rm -rf src/*/*.hi src/*/*.o *.comb *.tmp *~ bin/* a.out $(GHCOUTDIR) tmp/* Tools/*.o Tools/*.hi dist-newstyle generated/*-stage* .mhscache
	make clean -f Makefile.emscripten
	cd tests; make clean

install:
	mkdir -p $(PREFIX)/bin
	cp bin/mhs $(PREFIX)/bin
	-cp bin/cpphs $(PREFIX)/bin
	mkdir -p $(PREFIX)/lib/mhs/src/runtime
	cp -r lib $(PREFIX)/lib/mhs
	cp src/runtime/* $(PREFIX)/lib/mhs/src/runtime
	@echo "***"
	@echo "*** Installation complete"
	@echo "*** Set environment variable MHSDIR to $(PREFIX)/lib/mhs"
	@echo "***"

everytest:	newmhs runtest exampletest cachetest bootcombtest nfibtest info

everytestmhs:	bin/mhs bin/mhseval runtestmhs

runtestmhs:
	cd tests; make MHS="../bin/mhs +RTS -H4M -RTS " test

bootcombtest:	bin/gmhs bin/mhseval
	bin/gmhs -isrc -ogmhs.comb  MicroHs.Main
	bin/mhseval +RTS -v -rgmhs.comb -RTS -isrc -omhs.comb MicroHs.Main
	cmp gmhs.comb mhs.comb

exampletest:	bin/mhs bin/mhseval Example.hs
	bin/mhs -r Example
	bin/mhs Example && bin/mhseval
	bin/mhs Example -oEx && ./Ex && rm Ex

info:	bin/mhs
	bin/mhs -r -itests Info

cachetest:	bin/mhs bin/mhseval Example.hs
	rm -f .mhscache
	bin/mhs -CW AllOfLib
	bin/mhs -CR Example && bin/mhseval
	bin/mhs +RTS -v -RTS -isrc -CR MicroHs.Main
	rm -f .mhscache

nfibtest: bin/mhs bin/mhseval
	bin/mhs -itests Nfib && bin/mhseval

emscripten: bin/mhs
	make test -f Makefile.emscripten
