#!/bin/sh
# Compare the benchmarks in benchmarks/Loads between versions of MicroHs.
#
#   benchmarks/compare.sh [-n RUNS] [-s] REV...
#
# A REV is a git revision (a tag, a branch, or a commit), or "." for the
# working tree.  For each REV the script
#  * exports the revision to benchmarks/build/src-HASH (not for "."),
#  * builds its bin/mhs from generated/mhs.c, with `make bin/mhs`; with -s
#    it then builds the compiler from the sources with that, so that
#    compiler changes that are not in generated/mhs.c yet are included,
#  * compiles every benchmark in benchmarks/Loads of the working tree, so
#    that all versions run the same programs, with the compiler, the
#    libraries, and the runtime of the version.
# Then it checks that every version prints the same output, and runs each
# benchmark RUNS times (default 5) for every version, interleaving the
# versions.  All the samples go to benchmarks/results/compare-TIME.csv, and
# a table of medians, with the change from the first REV, to
# benchmarks/results/compare-TIME.md and to stdout.
#
# Collected: wall clock time, max RSS, and instructions retired (macOS
# only), from /usr/bin/time, and GC time, reductions, cells allocated, GCs,
# and max cells used, from the runtime's +RTS -v output.  Instructions and
# reductions are deterministic, and are the best measure of small changes.

set -e

usage() {
  echo "usage: $0 [-n RUNS] [-s] REV..." >&2
  exit 1
}

RUNS=5
FROMSRC=0
while getopts n:s opt; do
  case $opt in
    n) RUNS=$OPTARG ;;
    s) FROMSRC=1 ;;
    *) usage ;;
  esac
done
shift $((OPTIND - 1))
[ $# -ge 1 ] || usage

ROOT=$(git rev-parse --show-toplevel)
BENCH=$ROOT/benchmarks
BUILD=$BENCH/build
RESULTS=$BENCH/results
NAMES=$(cd "$BENCH/Loads" && ls *.hs | sed 's/\.hs$//')
STAMP=$(date +%Y%m%d-%H%M%S)
CSV=$RESULTS/compare-$STAMP.csv
MD=$RESULTS/compare-$STAMP.md
mkdir -p "$BUILD" "$RESULTS"

case $(uname -s) in
  Darwin) TIMEFLAG=-l ;;
  *)      if /usr/bin/time -v true >/dev/null 2>&1; then TIMEFLAG=-v; else TIMEFLAG=; fi ;;
esac

# Build the compiler for a REV, and the benchmarks with it.
# Sets LABEL and EXES.
build() {
  rev=$1
  if [ "$rev" = . ]; then
    LABEL=worktree
    dir=$ROOT
  else
    hash=$(git -C "$ROOT" rev-parse --verify --short "$rev^{commit}")
    LABEL=$(echo "$rev" | tr '/' '_')
    dir=$BUILD/src-$hash
    if [ ! -d "$dir" ]; then
      echo "== exporting $rev ($hash)"
      mkdir -p "$dir.tmp"
      git -C "$ROOT" archive "$hash" | tar -x -C "$dir.tmp"
      mv "$dir.tmp" "$dir"
    fi
  fi
  echo "== building the compiler for $LABEL"
  make -C "$dir" bin/mhs > "$BUILD/make-$LABEL.log" 2>&1 ||
    { echo "make bin/mhs failed, see $BUILD/make-$LABEL.log" >&2; exit 1; }
  mhs=$dir/bin/mhs
  suffix=
  if [ $FROMSRC = 1 ]; then
    (cd "$dir" && bin/mhs -z -i -imhs -isrc -ilib MicroHs.Main -obin/mhs-src) \
      > "$BUILD/make-$LABEL-src.log" 2>&1 ||
      { echo "building the compiler from source failed, see $BUILD/make-$LABEL-src.log" >&2; exit 1; }
    mhs=$dir/bin/mhs-src
    suffix=-src
  fi
  EXES=$BUILD/exe-$LABEL$suffix
  mkdir -p "$EXES"
  # A cache of the compiled libraries, used by all the benchmarks of this
  # version.  It is made anew, since the compiler may have changed.
  rm -f "$EXES/.mhscache"
  for n in $NAMES; do
    exe=$EXES/$n
    if [ "$rev" = . ] || [ ! -x "$exe" ] || [ "$BENCH/Loads/$n.hs" -nt "$exe" ] || [ "$mhs" -nt "$exe" ]; then
      echo "   compiling $n"
      # Compile in the build directory, so that no stray modules are found.
      (cd "$EXES" && "$mhs" -C -i"$BENCH" Loads.$n -o"$exe") > "$EXES/$n.log" 2>&1 ||
        { echo "compiling $n failed, see $EXES/$n.log" >&2; exit 1; }
    fi
  done
}

LABELS=
for rev in "$@"; do
  build "$rev"
  LABELS="$LABELS $LABEL"
  eval "EXES_$(echo "$LABEL" | tr -c 'A-Za-z0-9\n' '_')=\$EXES"
done

exes_of() {
  eval "echo \$EXES_$(echo "$1" | tr -c 'A-Za-z0-9\n' '_')"
}

# Check that all versions print the same, which also warms up the caches.
echo "== checking the output"
for n in $NAMES; do
  first=
  for l in $LABELS; do
    out=$(exes_of "$l")/$n.out
    "$(exes_of "$l")/$n" > "$out" 2>&1
    if [ -z "$first" ]; then
      first=$out
    elif ! cmp -s "$first" "$out"; then
      echo "   WARNING: $n prints differently for $l, see $out" >&2
    fi
  done
done

# Parse the output of one run into a line of the CSV file.
parse() {
  awk -v bench="$1" -v label="$2" -v run="$3" '
    function num(s) { gsub(/[,\047]/, "", s); return s + 0 }
    / reductions \(/          { red = num($1) }
    / cells allocated \(/     { cells = num($1) }
    / GCs$/                   { gcs = num($1) }
    / max cells used$/        { maxc = num($1) }
    / gc expired time/        { gc = $1 + 0 }
    / total expired time$/    { rts = $1 + 0 }
    / real .* user /          { wall = $1 + 0 }                     # macOS time -l
    /maximum resident set size$/ { rss = $1 + 0 }
    /instructions retired$/   { instr = $1 + 0 }
    /^\tElapsed \(wall clock\)/ { n = split($NF, t, ":"); wall = 0; for (i = 1; i <= n; i++) wall = wall * 60 + t[i] }  # GNU time -v
    /^\tMaximum resident set size/ { rss = $NF * 1024 }
    END {
      if (wall == "") wall = rts
      printf "%s,%s,%d,%s,%.3f,%.3f,%.0f,%.0f,%.0f,%.0f,%s\n", bench, label, run, instr, wall, gc, red, cells, gcs, maxc, rss
    }' "$4" "$5"
}

echo "bench,version,run,instructions,wall_s,gc_s,reductions,cells,gcs,max_cells,max_rss_bytes" > "$CSV"
TMP=$BUILD/run
echo "== running ($RUNS runs of each)"
r=1
while [ $r -le "$RUNS" ]; do
  for n in $NAMES; do
    for l in $LABELS; do
      /usr/bin/time $TIMEFLAG "$(exes_of "$l")/$n" +RTS -v -RTS > "$TMP.out" 2> "$TMP.err" ||
        { echo "$n failed for $l, see $TMP.out and $TMP.err" >&2; exit 1; }
      parse "$n" "$l" $r "$TMP.out" "$TMP.err" >> "$CSV"
    done
  done
  echo "   run $r done"
  r=$((r + 1))
done
rm -f "$TMP.out" "$TMP.err"

# The table of medians.
awk -F, -v labels="$LABELS" '
  function fmt(v) {
    if (v == "") return "-"
    if (v >= 1e9) return sprintf("%.2fG", v / 1e9)
    if (v >= 1e6) return sprintf("%.1fM", v / 1e6)
    if (v >= 1e3) return sprintf("%.1fK", v / 1e3)
    return sprintf("%d", v)
  }
  function secs(v) { return sprintf("%.2f s", v) }
  function mb(v) { return v == "" ? "-" : sprintf("%.0f MB", v / 1048576) }
  function change(v, b) {
    if (v == "" || b == "" || b == 0 || l == first) return ""
    return sprintf(" (%+.1f%%)", 100 * (v / b - 1))
  }
  function median(key,   n, i, j, t, a) {
    n = cnt[key]
    if (n == 0) return ""
    for (i = 1; i <= n; i++) a[i] = val[key, i]
    for (i = 2; i <= n; i++) { t = a[i]; for (j = i - 1; j >= 1 && a[j] > t; j--) a[j + 1] = a[j]; a[j + 1] = t }
    return n % 2 ? a[(n + 1) / 2] : (a[n / 2] + a[n / 2 + 1]) / 2
  }
  NR == 1 { for (i = 1; i <= NF; i++) col[i] = $i; next }
  {
    if (!($1 in seen)) { seen[$1] = 1; benches[++nb] = $1 }
    for (i = 4; i <= NF; i++) if ($i != "") { k = $1 SUBSEP $2 SUBSEP col[i]; val[k, ++cnt[k]] = $i }
  }
  END {
    nl = split(labels, ls, " ")
    first = ls[1]
    split("instructions wall_s gc_s reductions cells max_rss_bytes", ms, " ")
    for (b = 1; b <= nb; b++) {
      printf "## %s\n\n| version | instructions | wall | GC | reductions | cells | max RSS |\n|---|---|---|---|---|---|---|\n", benches[b]
      for (j = 1; j <= nl; j++) {
        l = ls[j]
        for (m = 1; m <= 6; m++) {
          med[m] = median(benches[b] SUBSEP l SUBSEP ms[m])
          if (j == 1) base[m] = med[m]
          if (j > 1 && med[m] != "" && base[m] != "" && base[m] > 0 && med[m] > 0) { lsum[l, m] += log(med[m] / base[m]); lcnt[l, m]++ }
        }
        printf "| %s | %s%s | %s%s | %s | %s%s | %s%s | %s |\n", l,
          fmt(med[1]), change(med[1], base[1]), secs(med[2]), change(med[2], base[2]), secs(med[3]),
          fmt(med[4]), change(med[4], base[4]), fmt(med[5]), change(med[5], base[5]), mb(med[6])
      }
      printf "\n"
    }
    if (nl > 1) {
      printf "## Geometric mean of the change from %s\n\n| version | instructions | wall | reductions | cells | max RSS |\n|---|---|---|---|---|---|\n", first
      for (j = 2; j <= nl; j++) {
        l = ls[j]
        printf "| %s", l
        split("1 2 4 5 6", ix, " ")
        for (m = 1; m <= 5; m++) {
          k = ix[m]
          printf " | %s", lcnt[l, k] ? sprintf("%+.1f%%", 100 * (exp(lsum[l, k] / lcnt[l, k]) - 1)) : "-"
        }
        printf " |\n"
      }
    }
  }' "$CSV" > "$MD"

cat "$MD"
echo
echo "wrote $CSV and $MD"
