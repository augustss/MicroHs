#!/bin/sh
# Build the compilers for the AOT measurements, and the frozen workload
# and reference output they are checked against.  Everything goes in
# $AOTWORK (default aot-work next to this checkout).
#   build.sh            the AOT code generator, mhs-0, mhs-8, mhs-25, mhs-78
#   build.sh all        also mhs-all (every function; 2 minutes, 9.5 GB)
# Set BASE to a jit-sampler checkout to also build mhs-base, the same
# compiler with a runtime that has no AOT hooks.
set -e
A=$(cd "$(dirname "$0")/.." && pwd)
T=$A/aot-tools
W=${AOTWORK:-$(dirname "$A")/aot-work}
mkdir -p "$W"
cd "$A"
[ -f mhs.conf ] || make mhs.conf
[ -x bin/mhs ] || make bin/mhs

# The code generator: this branch's compiler, which knows --aot.
MHSDIR=$A bin/mhs -z -i -imhs -isrc -ilib MicroHs.Main -o"$W/mhs-aot"

# A copy of the sources, so the workload does not change under the runs.
rm -rf "$W/workload"
mkdir -p "$W/workload"
cp -r src lib mhs "$W/workload/"

build() {  # name flags...
  n=$1; shift
  MHSDIR=$A "$W/mhs-aot" "$@" -z -i -imhs -isrc -ilib MicroHs.Main -o"$W/mhs-$n"
  echo "built mhs-$n"
}
build 0
for n in 8 25 78; do
  build $n "--aot=$(head -$n "$T/ranked.txt" | paste -sd, -)"
done
if [ "$1" = all ]; then
  # The generated C is held as a String: it needs a big heap.
  build all +RTS -K2M -H400M -RTS '--aot=*'
fi
if [ -n "$BASE" ]; then
  [ -f "$BASE/mhs.conf" ] || (cd "$BASE" && make mhs.conf)
  MHSDIR=$BASE "$W/mhs-aot" -z -i -imhs -isrc -ilib MicroHs.Main -o"$W/mhs-base"
  echo "built mhs-base"
fi

# The reference output: the uncompiled compiler on the frozen workload.
cd "$W/workload"
MHSDIR=$A "$W/mhs-0" -i -imhs -isrc -ilib MicroHs.Main -o"$W/ref.c"
echo "reference output in $W/ref.c"
