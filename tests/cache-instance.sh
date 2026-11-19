#!/bin/sh
# Check that the compilation cache forgets the instances of a module
# that is recompiled.
# Run from tests, with the compiler (and optional compiler flags) as arguments.
set -eu
root=$(cd .. && pwd)
compiler=$(cd "$(dirname "$1")" && pwd)/$(basename "$1")
shift
work=$(mktemp -d)
trap 'rm -rf "$work"' EXIT HUP INT TERM
cd "$work"
export MHSDIR="$root"

cat > CacheM.hs <<'EOM'
module CacheM(T(..)) where
data T = A | B
instance Show T where
  show A = "A"
  show B = "B"
EOM
cat > CacheMain.hs <<'EOM'
module CacheMain(main) where
import CacheM
main :: IO ()
main = print [A, B]
EOM
"$compiler" "$@" -C -r CacheMain > out
echo '[A,B]' | diff - out

# Remove the instance.  The cached Show T must not be used.
cat > CacheM.hs <<'EOM'
module CacheM(T(..)) where
data T = A | B
EOM
if "$compiler" "$@" -C -r CacheMain > out 2>&1; then
  echo "cache-instance: the removed instance was used"; exit 1
fi
grep -q 'no instance for (Show T)' out || { cat out; exit 1; }
