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
GHCEXTS= -XScopedTypeVariables -XTupleSections
GHCPKGS= -package mtl -package pretty
GHCTOOL= -F -pgmF Tools/convertX.sh
GHCOUTDIR= ghc-out
GHCOUT= -outputdir $(GHCOUTDIR)
GHCPROF= # -prof -fprof-late #-prof -fprof-auto
GHCFLAGS= $(GHCEXTS) $(GHCINCS) $(GHCWARNS) $(GHCOPTS) $(GHCTOOL) $(GHCPKGS) $(GHCOUT) $(GHCPROF)
#
MHSCOMP= bin/mhs -ilib -isrc MicroHs.Main
#
.PHONY:	clean bootstrap

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
	$(MHSCOMP) -ogenerated/mhs.c

# Make sure boottrapping works
bootstrap:	bin/mhs-stage2
	cp bin/mhs-stage2 bin/mhs
	cp generated/mhs-stage2.c generated/mhs.c 

# Build stage1 compiler with existing compiler
bin/mhs-stage1:	bin/mhs src/*/*.hs
	@mkdir -p generated
	@echo Build stage1 compiler
	$(MHSCOMP) -ogenerated/mhs-stage1.c
	$(CCEVAL) generated/mhs-stage1.c -o bin/mhs-stage1

# Build stage2 compiler with stage1 compiler, and compare
bin/mhs-stage2:	bin/mhs-stage1 src/*/*.hs
	@mkdir -p generated
	@echo Build stage2 compiler
	bin/mhs-stage1 -ilib -isrc MicroHs.Main -ogenerated/mhs-stage2.c
	cmp generated/mhs-stage1.c generated/mhs-stage2.c
	$(CCEVAL) generated/mhs-stage2.c -o bin/mhs-stage2
#
clean:
	rm -rf src/*/*.hi src/*/*.o *.comb *.tmp *~ bin/* a.out $(GHCOUTDIR) tmp/* Tools/*.o Tools/*.hi dist-newstyle generated/*-stage*
	cd tests; make clean
