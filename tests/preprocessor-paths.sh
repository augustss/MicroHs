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

    # Locations must keep the whole file name, including spaces, when the
    # source goes through the preprocessor and comes back with #line directives.
    cat > "$inst/input files/CppLine.hs" <<'EOF'
{-# LANGUAGE CPP #-}
module CppLine where
x :: Int
x = "not an int"
EOF
    if "$inst/bin/mhs" "$@" "$inst/input files/CppLine.hs" "-o$work/result.comb" 2> "$work/error"; then
        echo 'expected CppLine.hs to fail to compile'; exit 1
    fi
    grep -F "\"$inst/input files/CppLine.hs\": line 4, col 5" "$work/error" > /dev/null
    # A bare #line directive keeps the current file name.
    cat > "$inst/input files/CppBareLine.hs" <<'EOF'
{-# LANGUAGE CPP #-}
module CppBareLine where
x :: Int
#line 7
x = "not an int"
EOF
    if "$inst/bin/mhs" "$@" "$inst/input files/CppBareLine.hs" "-o$work/result.comb" 2> "$work/error"; then
        echo 'expected CppBareLine.hs to fail to compile'; exit 1
    fi
    grep -F "\"$inst/input files/CppBareLine.hs\": line 7, col 5" "$work/error" > /dev/null

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
printf '{-# LINE 1 "%s" #-}\n' "$6" > "$2"
cat "$6" >> "$2"
EOF
    chmod +x "$inst/bin/hsc2hs"
    export MHSHSC2HS="$inst/bin/hsc2hs"
    printf 'module HscPath where\nmain = print (42 :: Int)\n' > "$inst/input files/HscPath.hsc"
    "$inst/bin/mhs" "$@" "-i$inst/input files" HscPath "-o$work/result.comb"
    "$root/bin/mhseval" +RTS "-r$work/result.comb" -RTS > "$work/result"
    printf '42\n' > "$work/expected"
    diff "$work/expected" "$work/result"
    printf 'module HscLine where\nx :: Int\nx = "not an int"\n' > "$inst/input files/HscLine.hsc"
    if "$inst/bin/mhs" "$@" "-i$inst/input files" HscLine "-o$work/result.comb" 2> "$work/error"; then
        echo 'expected HscLine.hsc to fail to compile'; exit 1
    fi
    grep -F "\"$inst/input files/HscLine.hsc\": line 3, col 5" "$work/error" > /dev/null
done
echo 'Preprocessor path tests passed'
