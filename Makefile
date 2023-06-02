BIN=bin
.PHONY: all

all:
	@echo pick a target

$(BIN)/eval:	eval.c
	gcc -Wall -O3 eval.c -o $(BIN)/eval

$(BIN)/uhs:	*.hs
	ghc Main.hs -o $(BIN)/uhs

clean:
	rm -f *.hi *.o eval Main *.comb *.tmp *~

