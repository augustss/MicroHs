MHS=../bin/mhs -i../lib
EVAL=../bin/mhseval
.PHONY: test time clean errtest alltest

alltest:	test errtest

test:
	$(MHS) IOTest && (echo q | $(EVAL) a bb ccc | sed 's/^.ms/1ms/' > IOTest.out) && diff IOTest.ref IOTest.out
	$(MHS) StringTest && $(EVAL) > StringTest.out && diff StringTest.ref StringTest.out
	$(MHS) ListTest   && $(EVAL) > ListTest.out   && diff ListTest.ref ListTest.out
	$(MHS) Fac        && $(EVAL) > Fac.out        && diff Fac.ref Fac.out
	$(MHS) Misc       && $(EVAL) > Misc.out       && diff Misc.ref Misc.out
	$(MHS) Case       && $(EVAL) > Case.out       && diff Case.ref Case.out
	$(MHS) Arith      && $(EVAL) > Arith.out      && diff Arith.ref Arith.out
	$(MHS) Guard      && $(EVAL) > Guard.out      && diff Guard.ref Guard.out
	$(MHS) Newtype    && $(EVAL) > Newtype.out    && diff Newtype.ref Newtype.out
	$(MHS) LitMatch   && $(EVAL) > LitMatch.out   && diff LitMatch.ref LitMatch.out
	$(MHS) Word       && $(EVAL) > Word.out       && diff Word.ref Word.out
	$(MHS) Enum       && $(EVAL) > Enum.out       && diff Enum.ref Enum.out
	$(MHS) Foreign    && $(EVAL) > Foreign.out    && diff Foreign.ref Foreign.out
	$(MHS) MutRec     && $(EVAL) > MutRec.out     && diff MutRec.ref MutRec.out
	$(MHS) LocalPoly  && $(EVAL) > LocalPoly.out  && diff LocalPoly.ref LocalPoly.out
	$(MHS) Rank2      && $(EVAL) > Rank2.out      && diff Rank2.ref Rank2.out
	$(MHS) Catch      && $(EVAL) > Catch.out      && diff Catch.ref Catch.out
	$(MHS) FArith     && $(EVAL) > FArith.out     && diff FArith.ref FArith.out
	$(MHS) Infix      && $(EVAL) > Infix.out      && diff Infix.ref Infix.out
	$(MHS) Class      && $(EVAL) > Class.out      && diff Class.ref Class.out
	$(MHS) Eq         && $(EVAL) > Eq.out         && diff Eq.ref Eq.out
	$(MHS) Floating   && $(EVAL) > Floating.out   && diff Floating.ref Floating.out

errtest:
	sh errtester.sh < errmsg.test

time:
	@echo Expect about 10s runtime
	$(MHS) Nfib && time -p $(EVAL)

clean:
	rm -f *.out *.tmp
