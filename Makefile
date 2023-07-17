BIN=bin
BOOTDIR=ghc-boot
OUTDIR=ghc-out
GHCB=ghc -outputdir $(BOOTDIR)
GHCFLAGS=-i -ighc -ilib -i$(BOOTDIR) -hide-all-packages -XNoImplicitPrelude -F -pgmF $(CURDIR)/convertY.sh 
GHCC=$(GHCB) $(GHCFLAGS)
GHC=ghc
# $(CURDIR) might not be quite right
GHCE=$(GHC) -F -pgmF $(CURDIR)/convertX.sh -outputdir $(OUTDIR)
GCC=gcc
.PHONY: all trtest boottest uhstest test alltest time example

all:	$(BIN)/eval $(BIN)/uhs

alltest:	test boottest uhstest

$(BIN)/eval:	src/runtime/eval.c
	@mkdir -p bin
	$(GCC) -Wall -O3 src/runtime/eval.c -o $(BIN)/eval

$(BIN)/uhs:	src/*/*.hs convertX.sh
	$(GHCE) -package mtl -isrc -Wall -O src/MicroHs/Main.hs -main-is MicroHs.Main -o $(BIN)/uhs

trtest:	$(BIN)/uhs
	$(BIN)/uhs -ilib Main

$(BIN)/tr:	src/*/*.hs lib/*.hs lib/*/*.hs ghc/*.hs ghc/*/*.hs src/*/*.hs Main.hs convertY.sh
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
	$(GHC) -o $(BIN)/tr $(BOOTDIR)/*.o $(BOOTDIR)/Data/*.o $(BOOTDIR)/System/*.o $(BOOTDIR)/Text/*.o $(BOOTDIR)/Control/*.o $(BOOTDIR)/MicroHs/*.o

# Test Haskell version with local libraries
boottest:	$(BIN)/tr
	$(BIN)/tr -v -v T

# Test normal Haskell version
test:	$(BIN)/eval $(BIN)/uhs tests/*.hs
	cd tests; make test

# Test uhs version
uhstest:	$(BIN)/uhs $(BIN)/eval
	$(BIN)/uhs -ilib -isrc Main
	$(BIN)/eval

time:	$(BIN)/eval $(BIN)/uhs tests/*.hs
	cd tests; make time

example:	$(BIN)/eval $(BIN)/uhs Example.hs
	$(BIN)/uhs -ilib Example && $(BIN)/eval

clean:
	rm -rf src/*/*.hi src/*/*.o eval Main *.comb *.tmp *~ $(BIN)/* a.out $(BOOTDIR) $(OUTDIR)
	cd tests; make clean
