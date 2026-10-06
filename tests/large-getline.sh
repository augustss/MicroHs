#!/bin/sh
# Run from tests, with the compiler (and optional compiler flags) as arguments.
set -eu
compiler=$1
shift
evaluator=${EVALUATOR:-../bin/mhseval}
work=$(mktemp -d)
trap 'rm -rf "$work"' EXIT HUP INT TERM

"$compiler" "$@" LargeGetLine "-o$work/program.comb"
awk 'BEGIN {
    for (line = 0; line < 2; line++) {
        for (i = 0; i < 100000; i++) printf "0123456789"
        printf "\n"
    }
    printf "\nend\n"
}' > "$work/input"
printf 'discarded\nTrue\nTrue\nTrue\n' > "$work/expected"

# First reproduce the original default-settings failure, then vary GC timing
# and constrain the evaluation stack independently of the live graph size.
for settings in '' '-H8M -K4096'; do
    "$evaluator" +RTS $settings "-r$work/program.comb" -RTS \
        < "$work/input" > "$work/output"
    diff "$work/expected" "$work/output"
done
