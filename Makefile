BIN=bin
BOOTDIR=ghc-boot
GHC=ghc -outputdir $(BOOTDIR)
GHCFLAGS=-i -ighc -ilib -i$(BOOTDIR) -hide-all-packages -XNoImplicitPrelude
GHCC=$(GHC) $(GHCFLAGS)
.PHONY: all test time example

all:	$(BIN)/eval $(BIN)/uhs

$(BIN)/eval:	src/runtime/eval.c
	@mkdir -p bin
	gcc -Wall -O3 src/runtime/eval.c -o $(BIN)/eval

$(BIN)/uhs:	src/*/*.hs
	ghc -outputdir ghc-out -package mtl -isrc -Wall -O src/MicroHs/Main.hs -o $(BIN)/uhs

trtest:	$(BIN)/uhs
	$(BIN)/uhs -ilib -r -v Main

a.out:
	rm -rf $(BOOTDIR)
	$(GHC)  -c ghc/Primitives.hs
	$(GHC)  -c ghc/Data/Bool_Type.hs
	$(GHC)  -c ghc/Data/List_Type.hs
	$(GHCC) -c lib/Control/Error.hs
	$(GHCC) -c lib/Data/Bool.hs
	$(GHCC) -c lib/Data/Char.hs
	$(GHCC) -c lib/Data/Either.hs
	$(GHCC) -c lib/Data/Function.hs
	$(GHCC) -c lib/Data/Int.hs
	$(GHCC) -c lib/Data/List.hs
	$(GHCC) -c lib/Data/Maybe.hs
	$(GHCC) -c lib/Data/Tuple.hs
	$(GHCC) -c lib/System/IO.hs
	$(GHCC) -c lib/Text/String.hs
	$(GHCC) -c lib/Prelude.hs
	$(GHCC) -c Main.hs
	$(GHC) $(BOOTDIR)/*.o $(BOOTDIR)/Data/*.o $(BOOTDIR)/System/*.o $(BOOTDIR)/Text/*.o $(BOOTDIR)/Control/*.o

test:	$(BIN)/eval $(BIN)/uhs tests/*.hs
	cd tests; make test

time:	$(BIN)/eval $(BIN)/uhs tests/*.hs
	cd tests; make time

example:	$(BIN)/eval $(BIN)/uhs Example.hs
	$(BIN)/uhs -ilib Example && $(BIN)/eval

clean:
	rm -f src/*/*.hi src/*/*.o eval Main *.comb *.tmp *~ $(BIN)/* a.out
	cd tests; make clean


#ghc -Wall -O -isrc src/MicroHs/Main.hs -o $(BIN)/uhs
#ghc -c ghc/Primitives.hs
