UHS=../bin/uhs -i../lib
EVAL=../bin/eval
.PHONY: test time clean

test:
	$(UHS) IOTest && (echo q | $(EVAL) > IOTest.out) && diff IOTest.ref IOTest.out
	$(UHS) StringTest && $(EVAL) > StringTest.out && diff StringTest.ref StringTest.out
	$(UHS) Fac && $(EVAL) > Fac.out && diff Fac.ref Fac.out

time:
	@echo Expect about 10s runtime
	$(UHS) Nfib && time -p $(EVAL)

clean:
	rm -f *.out *.tmp
