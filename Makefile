BIN=bin
BOOTDIR=ghc-boot
OUTDIR=ghc-out
PROF= #-prof -fprof-auto
GHCB=ghc $(PROF) -outputdir $(BOOTDIR)
GHCFLAGS=-i -ighc -ilib -i$(BOOTDIR) -hide-all-packages -XNoImplicitPrelude -F -pgmF $(CURDIR)/convertY.sh 
GHCC=$(GHCB) $(GHCFLAGS)
GHC=ghc
# $(CURDIR) might not be quite right
GHCE=$(GHC) -F -pgmF $(CURDIR)/convertX.sh -outputdir $(OUTDIR)
GCC=gcc
ALLSRC=src/*/*.hs lib/*.hs lib/*/*.hs ghc/*.hs ghc/*/*.hs
.PHONY: all boottest bootboottest bootcombtest uhstest test alltest time example

all:	$(BIN)/eval $(BIN)/uhs

alltest:	test boottest

everytest:	alltest bootboottest bootcombtest

$(BIN)/eval:	src/runtime/eval.c
	@mkdir -p bin
	$(GCC) -Wall -O3 src/runtime/eval.c -o $(BIN)/eval

$(BIN)/uhs:	src/*/*.hs convertX.sh
	$(GHCE) -package mtl -isrc -Wall -O src/MicroHs/Main.hs -main-is MicroHs.Main -o $(BIN)/uhs

$(BIN)/bootuhs:	$(ALLSRC) Main.hs convertY.sh
	rm -rf $(BOOTDIR)
	$(GHCB) -c ghc/Primitives.hs
	$(GHCB) -c ghc/Data/Bool_Type.hs
	$(GHCB) -c ghc/Data/List_Type.hs
	$(GHCC) -c lib/Control/Error.hs
	$(GHCC) -c lib/Data/Bool.hs
	$(GHCC) -c lib/Data/Int.hs
	$(GHCC) -c lib/Data/Char.hs
	$(GHCC) -c lib/Data/Either.hs
	$(GHCC) -c lib/Data/Tuple.hs
	$(GHCC) -c lib/Data/Function.hs
	$(GHCC) -c lib/Data/Maybe.hs
	$(GHCC) -c lib/Data/List.hs
	$(GHCC) -c lib/Text/String.hs
	$(GHCC) -c lib/System/IO.hs
	$(GHCC) -c lib/System/Environment.hs
	$(GHCC) -c lib/Prelude.hs
	$(GHCC) -c lib/PreludeNoIO.hs
	$(GHCC) -c src/Text/ParserComb.hs
	$(GHCC) -c src/MicroHs/Parse.hs
	$(GHCC) -c src/MicroHs/Map.hs
	$(GHCC) -c src/MicroHs/Exp.hs
	$(GHCC) -c src/MicroHs/Desugar.hs
	$(GHCC) -c src/MicroHs/StateIO.hs
	$(GHCC) -c src/MicroHs/Compile.hs
	$(GHCC) -c -main-is MicroHs.Main src/MicroHs/Main.hs
	$(GHC) $(PROF) -hide-all-packages -o $(BIN)/bootuhs $(BOOTDIR)/*.o $(BOOTDIR)/Data/*.o $(BOOTDIR)/System/*.o $(BOOTDIR)/Text/*.o $(BOOTDIR)/Control/*.o $(BOOTDIR)/MicroHs/*.o

# Test Haskell version with local libraries
boottest:	$(BIN)/bootuhs
	$(BIN)/bootuhs -v -v T

# Compare version compiled with normal GHC libraries and uhs libraries
bootboottest:	$(BIN)/uhs $(BIN)/bootuhs
	$(BIN)/uhs     -ilib -isrc -omain-uhs.comb  MicroHs.Main
	$(BIN)/bootuhs -ilib -isrc -omain-boot.comb MicroHs.Main
	cmp main-uhs.comb main-boot.comb

# Compare version compiled with GHC, and bootstrapped combinator version
bootcombtest:	$(BIN)/uhs uhs.comb
	$(BIN)/uhs -ilib -isrc -omain-uhs.comb  MicroHs.Main
	$(BIN)/eval -H10000000 -ruhs.comb --  -ilib -isrc -omain-comb.comb MicroHs.Main
	cmp main-uhs.comb main-comb.comb

# Test normal Haskell version
test:	$(BIN)/eval $(BIN)/uhs tests/*.hs
	cd tests; make test

# Test uhs version
uhstest:	$(BIN)/uhs $(BIN)/eval
	$(BIN)/uhs -ilib -isrc Main
	$(BIN)/eval

uhs.comb:	$(BIN)/uhs $(ALLSRC)
	$(BIN)/uhs -ilib -isrc -ouhs.comb MicroHs.Main

ushcomp:	$(BIN)/eval uhs.comb
	$(BIN)/eval -H1000000 -v -ruhs.comb -- $(ARG)

time:	$(BIN)/eval $(BIN)/uhs tests/*.hs
	cd tests; make time

example:	$(BIN)/eval $(BIN)/uhs Example.hs
	$(BIN)/uhs -ilib Example && $(BIN)/eval

clean:
	rm -rf src/*/*.hi src/*/*.o eval Main *.comb *.tmp *~ $(BIN)/* a.out $(BOOTDIR) $(OUTDIR)
	cd tests; make clean
