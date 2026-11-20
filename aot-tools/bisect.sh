#!/bin/sh
# Bisect a list of definitions (one per line) for one whose compilation
# with --aot breaks the self-compile of the frozen workload.
#   bisect.sh LIST          e.g. bisect.sh aot-tools/alldefs.txt
# Needs mhs-aot, the workload and ref.c from build.sh.  Assumes a single
# culprit: it stops if neither half fails on its own.
A=$(cd "$(dirname "$0")/.." && pwd)
W=${AOTWORK:-$(dirname "$A")/aot-work}
B=$W/bisect
mkdir -p "$B"
cp "$1" "$B/cur.txt"
test_set() {  # file -> 0 if good, 1 if bad, 2 if it does not build
  cd "$A"
  if ! MHSDIR=$A "$W/mhs-aot" +RTS -K2M -H400M -RTS -z -i -imhs -isrc -ilib "--aot=$(paste -sd, - < "$1")" MicroHs.Main -o"$B/mhs-t" > "$B/build.log" 2>&1; then
    echo "BUILD FAILED"; return 2
  fi
  cd "$W/workload"
  MHSDIR=$A timeout 120 "$B/mhs-t" -i -imhs -isrc -ilib MicroHs.Main -o"$B/out.c" > "$B/run.log" 2>&1 || return 1
  cmp -s "$B/out.c" "$W/ref.c" || return 1
  return 0
}
while [ $(wc -l < "$B/cur.txt") -gt 1 ]; do
  n=$(wc -l < "$B/cur.txt"); h=$((n / 2))
  head -$h "$B/cur.txt" > "$B/a.txt"; tail -n +$((h + 1)) "$B/cur.txt" > "$B/b.txt"
  test_set "$B/a.txt"; ra=$?
  echo "n=$n first half ($h): $ra"
  if [ $ra -eq 1 ]; then cp "$B/a.txt" "$B/cur.txt"; continue; fi
  test_set "$B/b.txt"; rb=$?
  echo "n=$n second half ($((n - h))): $rb"
  if [ $rb -eq 1 ]; then cp "$B/b.txt" "$B/cur.txt"; continue; fi
  echo "neither half fails alone"; break
done
echo "result:"; cat "$B/cur.txt"
