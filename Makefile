BIN=bin
.PHONY: all test time example

all:	$(BIN)/eval $(BIN)/uhs

$(BIN)/eval:	eval.c
	gcc -Wall -O3 eval.c -o $(BIN)/eval

$(BIN)/uhs:	*.hs
	ghc -outputdir ghc-out -package mtl -Wall -O Main.hs -o $(BIN)/uhs

test:	$(BIN)/eval $(BIN)/uhs tests/*.hs
	cd tests; make test

time:	$(BIN)/eval $(BIN)/uhs tests/*.hs
	cd tests; make time

example:	$(BIN)/eval $(BIN)/uhs Example.hs
	$(BIN)/uhs -ilib Example && $(BIN)/eval

clean:
	rm -f *.hi *.o eval Main *.comb *.tmp *~ bin/*
	cd tests; make clean
