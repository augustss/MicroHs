test:	eval nfib.comb
	./eval nfib.comb

eval:	eval.c
	gcc -Wall -O3 eval.c -o eval

nfib.comb:	Lam
	./Lam > nfib.comb

Main:	*.hs
	ghc Main.hs -o Main

clean:
	rm -f *.hi *.o eval Lam *.comb
