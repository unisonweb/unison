#!/bin/sh
# Regenerates the LLVM IR the JIT embeds (src/Unison/Runtime/JIT/Helpers.hs):
# cbits/jit_helpers.ll from cbits/jit_helpers.c (the IR helpers) and
# cbits/jit_exit.ll from cbits/jit_exit.c (the write-back routine). Needs
# clang and the headers of the GHC that stack uses. Run from anywhere;
# `--check` regenerates to temporary files and fails if either differs from
# the committed one.
set -e
cd "$(dirname "$0")"
libdir=$(stack exec -- ghc --print-libdir 2>/dev/null)
inc=$(ls -d "$libdir"/*/rts-*/include | head -n 1)
gen() {
  clang -S -emit-llvm -O1 -fno-discard-value-names -I"$inc" -o - "$1" | python3 gen-helpers.py
}
status=0
for src in jit_helpers.c jit_exit.c; do
  ll="${src%.c}.ll"
  if [ "$1" = "--check" ]; then
    tmp=$(mktemp)
    gen "$src" > "$tmp"
    if diff -u "$ll" "$tmp"; then echo "$ll is up to date"
    else echo "$ll is stale: run cbits/gen-helpers.sh" >&2; status=1; fi
    rm -f "$tmp"
  else
    gen "$src" > "$ll"
    echo "wrote $(pwd)/$ll ($(wc -l < "$ll") lines)"
  fi
done
exit $status
