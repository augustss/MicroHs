UHS=../bin/uhs -i../lib
EVAL=../bin/eval

tests:
	$(UHS) IOTest && (echo q | $(EVAL) out.comb > IOTest.out) && diff IOTest.ref IOTest.out
	$(UHS) StringTest && $(EVAL) out.comb > StringTest.out && diff StringTest.ref StringTest.out
	$(UHS) Fac && $(EVAL) out.comb > Fac.out && diff Fac.ref Fac.out

clean:
	rm -f *.out *.tmp
