UHS=../bin/uhs -i../lib
EVAL=../bin/eval
.PHONY: test time clean

test:
	$(UHS) IOTest && (echo q | $(EVAL) -- a bb ccc > IOTest.out) && diff IOTest.ref IOTest.out
	$(UHS) StringTest && $(EVAL) > StringTest.out && diff StringTest.ref StringTest.out
	$(UHS) ListTest && $(EVAL) > ListTest.out && diff ListTest.ref ListTest.out
	$(UHS) Fac && $(EVAL) > Fac.out && diff Fac.ref Fac.out
	$(UHS) Misc && $(EVAL) > Misc.out && diff Misc.ref Misc.out
	$(UHS) Case && $(EVAL) > Case.out && diff Case.ref Case.out
	$(UHS) Arith && $(EVAL) > Arith.out && diff Arith.ref Arith.out

time:
	@echo Expect about 10s runtime
	$(UHS) Nfib && time -p $(EVAL)

clean:
	rm -f *.out *.tmp
