#!/bin/sh
# Quick correctness gate for a runtime change (faster than validate.sh, which
# recompiles the full prelude for 23 tests ~= 10 min).  This does the two
# highest-value checks: (1) self-compile byte-identical to gold.comb (exercises
# the whole runtime), and (2) a few representative + threading test programs.
set -u
cd "$(dirname "$0")/.."
fail=0
echo "### rebuild"
cc -Wall -O3 -Isrc/runtime -Isrc/runtime/unix src/runtime/main.c src/runtime/eval.c src/runtime/comb.c -lm -o bin/mhseval || { echo BUILD-mhseval-FAIL; exit 1; }
cc -Wall -O3 -Isrc/runtime -Isrc/runtime/unix src/runtime/main.c src/runtime/eval.c generated/mhs.c -lm -o bin/mhs || { echo BUILD-mhs-FAIL; exit 1; }

echo "### bench values"
out=$(bin/mhseval +RTS -rBench.comb -RTS)
echo "$out"
echo "$out" | grep -q "nfib32: 7049155"      || { echo "FAIL nfib32"; fail=1; }
echo "$out" | grep -q "tak: 9 "               || { echo "FAIL tak"; fail=1; }
echo "$out" | grep -q "sumTo: 12500002500000" || { echo "FAIL sumTo"; fail=1; }
echo "$out" | grep -q "churn: 12002000"       || { echo "FAIL churn"; fail=1; }

echo "### key tests"
for t in Hello Fac Exception IOArray MVar; do
  [ -f tests/$t.hs ] && [ -f tests/$t.ref ] || continue
  if bin/mhs -itests $t -o/tmp/q_$t.comb >/dev/null 2>&1; then
    bin/mhseval +RTS -r/tmp/q_$t.comb -RTS >/tmp/q_$t.out 2>&1
    if diff -q tests/$t.ref /tmp/q_$t.out >/dev/null 2>&1; then printf "  ok   %s\n" "$t"; else printf "  FAIL %s\n" "$t"; fail=1; fi
  else printf "  FAIL %s (compile)\n" "$t"; fail=1; fi
done

echo "### self-compile byte-identical to gold.comb"
if bin/mhs -imhs -isrc MicroHs.Main -o/tmp/q_self.comb >/dev/null 2>&1; then
  if cmp -s gold.comb /tmp/q_self.comb; then echo "  ok   byte-identical"; else echo "  FAIL differs from gold"; fail=1; fi
else echo "  FAIL self-compile errored"; fail=1; fi

echo
[ $fail -eq 0 ] && echo "QVALIDATE: ALL PASS" || echo "QVALIDATE: FAILURES"
exit $fail
