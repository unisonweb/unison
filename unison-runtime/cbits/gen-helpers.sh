#!/bin/sh
# Regenerates cbits/jit_helpers.ll, the LLVM IR of cbits/jit_helpers.c that the
# JIT embeds (src/Unison/Runtime/JIT/Helpers.hs). Needs clang and the headers
# of the GHC that stack uses. Run from anywhere; `--check` regenerates to a
# temporary file and fails if it differs from the committed one.
set -e
cd "$(dirname "$0")"
libdir=$(stack exec -- ghc --print-libdir 2>/dev/null)
inc=$(ls -d "$libdir"/*/rts-*/include | head -n 1)
gen() {
  clang -S -emit-llvm -O1 -fno-discard-value-names -I"$inc" -o - jit_helpers.c | python3 gen-helpers.py
}
if [ "$1" = "--check" ]; then
  tmp=$(mktemp)
  gen > "$tmp"
  if diff -u jit_helpers.ll "$tmp"; then echo "jit_helpers.ll is up to date"; rm -f "$tmp"
  else echo "jit_helpers.ll is stale: run cbits/gen-helpers.sh" >&2; rm -f "$tmp"; exit 1; fi
else
  gen > jit_helpers.ll
  echo "wrote $(pwd)/jit_helpers.ll ($(wc -l < jit_helpers.ll) lines)"
fi
