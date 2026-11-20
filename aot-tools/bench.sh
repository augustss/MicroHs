#!/bin/sh
# Time the compilers from build.sh on the frozen workload, interleaved,
# and check each output against the reference.
#   bench.sh [RUNS [VERSION...]]     default: 3 runs of 0 8 25 78
# Writes $AOTWORK/meas.csv; the numbers in the plan are in results.csv.
A=$(cd "$(dirname "$0")/.." && pwd)
W=${AOTWORK:-$(dirname "$A")/aot-work}
RUNS=${1:-3}; [ $# -gt 0 ] && shift
[ $# -eq 0 ] && set -- 0 8 25 78
M=$W/meas.csv
echo "version,run,instr,wall,gc,reductions,cells,ok" > "$M"
cd "$W/workload"
run=1
while [ $run -le "$RUNS" ]; do
  for x in "$@"; do
    MHSDIR=$A /usr/bin/time -l "$W/mhs-$x" +RTS -v -RTS -i -imhs -isrc -ilib MicroHs.Main -o"$W/o-$x.c" > "$W/v.txt" 2> "$W/t.txt"
    ok=$(cmp -s "$W/o-$x.c" "$W/ref.c" && echo same || echo DIFF)
    ins=$(awk '/instructions retired/{print $1}' "$W/t.txt")
    wall=$(awk '/ real /{print $1}' "$W/t.txt")
    gc=$(awk '/gc expired time/{print $1}' "$W/v.txt")
    red=$(awk '/ reductions \(/{gsub(",","",$1); print $1}' "$W/v.txt")
    cells=$(awk '/ cells allocated \(/{gsub(",","",$1); print $1}' "$W/v.txt")
    echo "$x,$run,$ins,$wall,$gc,$red,$cells,$ok" | tee -a "$M"
  done
  run=$((run + 1))
done
