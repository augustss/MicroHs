BIN=bin
BOOTDIR=ghc-boot
OUTDIR=ghc-out
PROF= #-prof -fprof-auto
EXTS= -XScopedTypeVariables -XQualifiedDo
GHCB=ghc $(PROF) -outputdir $(BOOTDIR)
GHCFLAGS=-i -ighc -ilib -i$(BOOTDIR) -hide-all-packages -XNoImplicitPrelude $(EXTS) -F -pgmF $(CURDIR)/convertY.sh 
GHCC=$(GHCB) $(GHCFLAGS)
GHC=ghc
# $(CURDIR) might not be quite right
GHCE=$(GHC) $(EXTS) -F -pgmF $(CURDIR)/convertX.sh -outputdir $(OUTDIR)
GCC=gcc
ALLSRC=src/*/*.hs lib/*.hs lib/*/*.hs ghc/*.hs ghc/*/*.hs
MHS=mhs
.PHONY: all alltest everytest boottest bootboottest bootcombtest $(MHS)test test alltest time example

all:	$(BIN)/eval $(BIN)/$(MHS)

alltest:	test boottest

everytest:	alltest example examplecomb bootboottest bootcombtest

$(BIN)/eval:	src/runtime/eval.c
	@mkdir -p bin
	$(GCC) -Wall -O3 src/runtime/eval.c -o $(BIN)/eval

$(BIN)/$(MHS):	src/*/*.hs convertX.sh
	$(GHCE) -package mtl -isrc -Wall -O src/MicroHs/Main.hs -main-is MicroHs.Main -o $(BIN)/$(MHS)

$(BIN)/boot$(MHS):	$(ALLSRC) convertY.sh
	rm -rf $(BOOTDIR)
	$(GHCB) -c ghc/Primitives.hs
	$(GHCB) -c ghc/Data/Bool_Type.hs
	$(GHCB) -c src/PrimTable.hs
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
	$(GHCC) -c lib/Data/Map.hs
	$(GHCC) -c lib/Data/IntMap.hs
	$(GHCC) -c lib/Unsafe/Coerce.hs
	$(GHCC) -c lib/Control/Monad/State/Strict.hs
	$(GHCC) -c src/Text/ParserComb.hs
	$(GHCC) -c src/MicroHs/Parse.hs
	$(GHCC) -c src/MicroHs/StringMap.hs
#	$(GHCC) -c -package containers -package base src/MicroHs/StringMap.hs
	$(GHCC) -c src/MicroHs/Exp.hs
	$(GHCC) -c src/MicroHs/TCMonad.hs
	$(GHCC) -c src/MicroHs/TypeCheck.hs
	$(GHCC) -c src/MicroHs/Desugar.hs
	$(GHCC) -c src/MicroHs/StateIO.hs
	$(GHCC) -c src/MicroHs/Compile.hs
	$(GHCC) -c src/MicroHs/Translate.hs
	$(GHCC) -c -main-is MicroHs.Main src/MicroHs/Main.hs
	$(GHC) $(PROF) -hide-all-packages -o $(BIN)/boot$(MHS) $(BOOTDIR)/*.o $(BOOTDIR)/*/*.o $(BOOTDIR)/*/*/*/*.o
#	$(GHC) $(PROF) -hide-all-packages -package containers -o $(BIN)/boot$(MHS) $(BOOTDIR)/*.o $(BOOTDIR)/*/*.o $(BOOTDIR)/*/*/*/*.o

# Test Haskell version with local libraries
boottest:	$(BIN)/boot$(MHS)
	$(BIN)/boot$(MHS) -ilib Example

# Compare version compiled with normal GHC libraries and $(MHS) libraries
bootboottest:	$(BIN)/$(MHS) $(BIN)/boot$(MHS)
	$(BIN)/$(MHS)     -ilib -isrc -omain-$(MHS).comb  MicroHs.Main
	$(BIN)/boot$(MHS) -ilib -isrc -omain-boot.comb MicroHs.Main
	cmp main-$(MHS).comb main-boot.comb

# Compare version compiled with GHC, and bootstrapped combinator version
bootcombtest:	$(BIN)/$(MHS) $(BIN)/eval $(MHS).comb
	$(BIN)/$(MHS) -ilib -isrc -omain-$(MHS).comb  MicroHs.Main
	$(BIN)/eval -H50M -r$(MHS).comb --  -ilib -isrc -omain-comb.comb MicroHs.Main
	cmp main-$(MHS).comb main-comb.comb

# Test normal Haskell version
test:	$(BIN)/eval $(BIN)/$(MHS) tests/*.hs
	cd tests; make test

$(MHS).comb:	$(BIN)/$(MHS) $(ALLSRC)
	$(BIN)/$(MHS) -ilib -isrc -o$(MHS).comb MicroHs.Main

$(MHS)comp:	$(BIN)/eval $(MHS).comb
	$(BIN)/eval -H1M -v -r$(MHS).comb -- $(ARG)

time:	$(BIN)/eval $(BIN)/$(MHS) tests/*.hs
	cd tests; make time

example:	$(BIN)/eval $(BIN)/$(MHS) Example.hs
	$(BIN)/$(MHS) -ilib Example && $(BIN)/eval

# does not work
exampleboot:	$(BIN)/boot$(MHS) Example.hs
	$(BIN)/boot$(MHS) -r -ilib Example

examplecomb:	$(BIN)/eval $(MHS).comb Example.hs
	$(BIN)/eval -H5M -r$(MHS).comb -- -r -ilib Example

clean:
	rm -rf src/*/*.hi src/*/*.o eval Main *.comb *.tmp *~ $(BIN)/* a.out $(BOOTDIR) $(OUTDIR)
	cd tests; make clean
