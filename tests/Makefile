MHS=../bin/gmhs
TMHS=$(MHS) -i../lib
EVAL=../bin/mhseval +RTS -H1M -RTS
.PHONY: test nfib clean errtest alltest cache info

alltest:	info test errtest interactivetest

cache:
	rm -f .mhscache
	$(TMHS) -CW AllOfLib

info:
	$(TMHS) Info       && $(EVAL)

test:
	$(TMHS) RecMdl     && $(EVAL) > RecMdl.out     && diff RecMdl.ref RecMdl.out
	$(TMHS) ForeignPtr && $(EVAL) > ForeignPtr.out && diff ForeignPtr.ref ForeignPtr.out

errtest:
	sh errtester.sh < errmsg.test

interactivetest:
	$(TMHS) < itest.in | sed -e '1,/^>/d' > itest.out && diff itest.ref itest.out

nfib:
	$(TMHS) Nfib && $(EVAL)

clean:
	rm -f *.out *.tmp
