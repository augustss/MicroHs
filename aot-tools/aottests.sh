#!/bin/sh
# Differential test of mhs --aot: compile each short-form test of all.tests
# to an executable without and with --aot, and compare with the .ref file.
#   aottests.sh MODE OUTDIR [TESTS...]
# MODE "all" compiles every function (--aot=*); anything else compiles
# only the test's own module (--aot=MODULE.*).  Needs mhs-aot from build.sh.
# Serdes, SerdesDescr, Reflect and PartRed are expected to fail with --aot:
# they serialize or print code.
A=$(cd "$(dirname "$0")/.." && pwd)
W=${AOTWORK:-$(dirname "$A")/aot-work}
MHS=$W/mhs-aot
MODE=$1; shift
OUT=$1; shift
mkdir -p "$OUT"
OUT=$(cd "$OUT" && pwd)
cd "$A/tests"
if [ ! -f "$OUT/cache" ]; then
  MHSDIR=$A "$MHS" -i -i. -i"$A/lib" -CW"$OUT/cache" AllOfLib > /dev/null 2>&1
fi
if [ $# -eq 0 ]; then
  set -- $(grep -v '^#' all.tests | awk 'NF==1 {print $1}')
fi
for t in "$@"; do
  if [ "$MODE" = all ]; then spec='*'; else spec="$t.*"; fi
  r=""
  for v in plain aot; do
    exe=$OUT/$t-$v
    if [ $v = plain ]; then flags=""; else flags="--aot=$spec"; fi
    if MHSDIR=$A "$MHS" -i -i. -i"$A/lib" -CR"$OUT/cache" $flags $t -o"$exe" > "$OUT/$t-$v.build" 2>&1; then
      timeout 60 "$exe" > "$OUT/$t-$v.out" 2> "$OUT/$t-$v.err" < /dev/null
      ec=$?
      if cmp -s "$OUT/$t-$v.out" $t.ref && [ ! -s "$OUT/$t-$v.err" ]; then r="$r $v:ok"; else r="$r $v:FAIL($ec)"; fi
    else
      r="$r $v:NOBUILD"
    fi
  done
  echo "$t$r"
done
