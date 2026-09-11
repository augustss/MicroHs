#!/bin/sh
# Run from tests, with the compiler (and optional compiler flags) as arguments.
set -eu
root=$(cd .. && pwd)
compiler=$1
shift
work=$(mktemp -d)
trap 'rm -rf "$work"' EXIT HUP INT TERM

for name in 'installed mhs' "installed mhs's"; do
    inst=$work/$name
    mkdir -p "$inst/bin" "$inst/src" "$inst/tmp" "$inst/input files"
    cp "$compiler" "$inst/bin/mhs"
    cp "$root/bin/cpphs" "$inst/bin/cpphs"
    cp "$root/mhs.conf" "$inst/mhs.conf"
    cp -R "$root/src/runtime" "$inst/src/runtime"
    ln -s "$root/lib" "$inst/lib"
    export MHSDIR="$inst" TMPDIR="$inst/tmp" MHSCPPHS="$inst/bin/cpphs"
    # Resolve this header through the compiler's generated runtime -I argument.
    echo '#define PATH_VALUE 42' > "$inst/src/runtime/path-test.h"
    cat > "$inst/input files/CppPath.hs" <<'EOF'
{-# LANGUAGE CPP #-}
module CppPath where
#include "path-test.h"
main = do
  print PATH_VALUE
  putStrLn PATH_TEXT
EOF
    "$inst/bin/mhs" "$@" "-DPATH_TEXT=\"$name\"" \
        "$inst/input files/CppPath.hs" "-o$work/result.comb"
    "$root/bin/mhseval" +RTS "-r$work/result.comb" -RTS > "$work/result"
    printf '42\n%s\n' "$name" > "$work/expected"
    diff "$work/expected" "$work/result"

    # Check the custom preprocessor's positional arguments, including an empty one.
    cat > "$inst/bin/preprocessor" <<'EOF'
#!/bin/sh
set -eu
test "$#" = 5
test -f "$1"
test "$4" = "space and single ' quote"
test -z "$5"
cp "$2" "$3"
EOF
    chmod +x "$inst/bin/preprocessor"
    "$inst/bin/mhs" "$@" -F -pgmF "$inst/bin/preprocessor" \
        -optF "space and single ' quote" -optF '' \
        "-DPATH_TEXT=\"$name\"" "$inst/input files/CppPath.hs" "-o$work/result.comb"
    "$root/bin/mhseval" +RTS "-r$work/result.comb" -RTS > "$work/result"
    diff "$work/expected" "$work/result"

    # A stand-in for hsc2hs checks its arguments without requiring that tool.
    cat > "$inst/bin/hsc2hs" <<'EOF'
#!/bin/sh
set -eu
test "$#" = 6
test "$1" = -o
test "$3" = -D__MHS__
test "$4" = "-I$MHSDIR/src/runtime"
test "$5" = "-I$MHSDIR/src/runtime/unix"
cp "$6" "$2"
EOF
    chmod +x "$inst/bin/hsc2hs"
    export MHSHSC2HS="$inst/bin/hsc2hs"
    printf 'module HscPath where\nmain = print (42 :: Int)\n' > "$inst/input files/HscPath.hsc"
    "$inst/bin/mhs" "$@" "-i$inst/input files" HscPath "-o$work/result.comb"
    "$root/bin/mhseval" +RTS "-r$work/result.comb" -RTS > "$work/result"
    printf '42\n' > "$work/expected"
    diff "$work/expected" "$work/result"
done
echo 'Preprocessor path tests passed'
