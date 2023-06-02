UHS=../bin/uhs
EVAL=../bin/eval

tests:
	$(UHS) IOTest.hs && $(EVAL) out.comb > IOTest.out && diff IOTest.ref IOTest.out
