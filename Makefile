test:	eval nfib.comb
	./eval nfib.comb

eval:	eval.c
	gcc -O3 eval.c -o eval

nfib.comb:	Lam
	./Lam > nfib.comb

Lam:	Lam.hs
	ghc Lam.hs -o Lam

clean:
	rm -f *.hi *.o eval Lam
