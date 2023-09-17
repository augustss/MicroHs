MHS=../bin/mhs -i../lib
EVAL=../bin/eval
.PHONY: test time clean

test:
	$(MHS) IOTest && (echo q | $(EVAL) a bb ccc | sed 's/^.ms/1ms/' > IOTest.out) && diff IOTest.ref IOTest.out
	$(MHS) StringTest && $(EVAL) > StringTest.out && diff StringTest.ref StringTest.out
	$(MHS) ListTest && $(EVAL) > ListTest.out && diff ListTest.ref ListTest.out
	$(MHS) Fac && $(EVAL) > Fac.out && diff Fac.ref Fac.out
	$(MHS) Misc && $(EVAL) > Misc.out && diff Misc.ref Misc.out
	$(MHS) Case && $(EVAL) > Case.out && diff Case.ref Case.out
	$(MHS) Arith && $(EVAL) > Arith.out && diff Arith.ref Arith.out
	$(MHS) Guard && $(EVAL) > Guard.out && diff Guard.ref Guard.out
	$(MHS) Newtype && $(EVAL) > Newtype.out && diff Newtype.ref Newtype.out
	$(MHS) LitMatch && $(EVAL) > LitMatch.out && diff LitMatch.ref LitMatch.out
	$(MHS) Word && $(EVAL) > Word.out && diff Word.ref Word.out
	$(MHS) Enum && $(EVAL) > Enum.out && diff Enum.ref Enum.out
	$(MHS) Foreign && $(EVAL) > Foreign.out && diff Foreign.ref Foreign.out
	$(MHS) MutRec && $(EVAL) > MutRec.out && diff MutRec.ref MutRec.out

time:
	@echo Expect about 10s runtime
	$(MHS) Nfib && time -p $(EVAL)

clean:
	rm -f *.out *.tmp
