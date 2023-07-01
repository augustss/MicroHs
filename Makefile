BIN=bin
.PHONY: all test time example

all:	$(BIN)/eval $(BIN)/uhs

$(BIN)/eval:	src/runtime/eval.c
	@mkdir -p bin
	gcc -Wall -O3 src/runtime/eval.c -o $(BIN)/eval

$(BIN)/uhs:	*.hs
	ghc -outputdir ghc-out -package mtl -isrc -Wall -O src/MicroHs/Main.hs -o $(BIN)/uhs

test:	$(BIN)/eval $(BIN)/uhs tests/*.hs
	cd tests; make test

time:	$(BIN)/eval $(BIN)/uhs tests/*.hs
	cd tests; make time

example:	$(BIN)/eval $(BIN)/uhs Example.hs
	$(BIN)/uhs -ilib Example && $(BIN)/eval

clean:
	rm -f src/*/*.hi src/*/*.o eval Main *.comb *.tmp *~ $(BIN)/*
	cd tests; make clean
