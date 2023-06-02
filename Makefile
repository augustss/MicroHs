BIN=bin
.PHONY: all test

all:
	@echo pick a target

$(BIN)/eval:	eval.c
	gcc -Wall -O3 eval.c -o $(BIN)/eval

$(BIN)/uhs:	*.hs
	ghc Main.hs -o $(BIN)/uhs

test:	$(BIN)/eval $(BIN)/uhs tests/*.hs
	cd tests; make

clean:
	rm -f *.hi *.o eval Main *.comb *.tmp *~
	cd tests; make clean
