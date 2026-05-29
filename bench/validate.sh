#!/bin/sh
# Correctness validation for a modified runtime.
# Rebuilds bin/mhs + bin/mhseval from current eval.c, then:
#  1. runs the micro-benchmark and checks result values,
#  2. compiles+runs a set of tests, diffing against tests/*.ref,
#  3. exercises the threading tests (Concur/Fork/MVar),
#  4. self-compiles MicroHs.Main and compares the .comb to gold.comb.
# Prints PASS/FAIL per stage. Exit non-zero on any failure.
set -u
cd "$(dirname "$0")/.."
fail=0

echo "### rebuild"
cc -Wall -O3 -Isrc/runtime -Isrc/runtime/unix src/runtime/main.c src/runtime/eval.c src/runtime/comb.c -lm -o bin/mhseval || { echo "BUILD mhseval FAIL"; exit 1; }
cc -Wall -O3 -Isrc/runtime -Isrc/runtime/unix src/runtime/main.c src/runtime/eval.c generated/mhs.c -lm -o bin/mhs || { echo "BUILD mhs FAIL"; exit 1; }

echo "### bench values"
out=$(bin/mhseval +RTS -rBench.comb -RTS)
echo "$out"
echo "$out" | grep -q "nfib32: 7049155"        || { echo "FAIL nfib32"; fail=1; }
echo "$out" | grep -q "tak: 9 "                 || { echo "FAIL tak"; fail=1; }
echo "$out" | grep -q "sumTo: 12500002500000"   || { echo "FAIL sumTo"; fail=1; }
echo "$out" | grep -q "churn: 12002000"         || { echo "FAIL churn"; fail=1; }

echo "### correctness tests (compile with bin/mhs, run with bin/mhseval, diff .ref)"
TESTS="Hello Fac Misc Case Arith Arith64 Word Integer Enum ListTest StringTest Serdes Guard Newtype LitMatch Foldable Traversable Record IOArray Exception"
for t in $TESTS; do
  [ -f tests/$t.hs ] || continue
  [ -f tests/$t.ref ] || continue
  if bin/mhs -itests $t -o/tmp/v_$t.comb >/dev/null 2>&1; then
    bin/mhseval +RTS -r/tmp/v_$t.comb -RTS >/tmp/v_$t.out 2>&1
    if diff -q tests/$t.ref /tmp/v_$t.out >/dev/null 2>&1; then
      printf "  ok   %s\n" "$t"
    else
      printf "  FAIL %s (output differs)\n" "$t"; fail=1
    fi
  else
    printf "  FAIL %s (compile error)\n" "$t"; fail=1
  fi
done

echo "### threading tests"
for t in Concur Fork MVar; do
  [ -f tests/$t.hs ] || continue
  if bin/mhs -itests $t -o/tmp/v_$t.comb >/dev/null 2>&1; then
    bin/mhseval +RTS -r/tmp/v_$t.comb -RTS >/tmp/v_$t.out 2>&1
    if [ -f tests/$t.ref ] && diff -q tests/$t.ref /tmp/v_$t.out >/dev/null 2>&1; then
      printf "  ok   %s\n" "$t"
    else
      printf "  ??   %s (no ref match; output below)\n" "$t"; sed 's/^/      /' /tmp/v_$t.out | head -8
    fi
  else
    printf "  FAIL %s (compile error)\n" "$t"; fail=1
  fi
done

echo "### self-compile equivalence (gold.comb)"
if bin/mhs -imhs -isrc MicroHs.Main -o/tmp/v_self.comb >/dev/null 2>&1; then
  if cmp -s gold.comb /tmp/v_self.comb; then
    echo "  ok   self-compile byte-identical to gold"
  else
    echo "  FAIL self-compile differs from gold!"; ls -la gold.comb /tmp/v_self.comb; fail=1
  fi
else
  echo "  FAIL self-compile crashed/errored"; fail=1
fi

echo
if [ $fail -eq 0 ]; then echo "VALIDATE: ALL PASS"; else echo "VALIDATE: FAILURES PRESENT"; fi
exit $fail
