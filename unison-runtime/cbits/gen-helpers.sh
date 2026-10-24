#!/usr/bin/env bash
# Regenerates the LLVM IR the JIT embeds (src/Unison/Runtime/JIT/Helpers.hs):
# cbits/jit_helpers.ll from cbits/jit_helpers.c (the IR helpers) and
# cbits/jit_exit.ll from cbits/jit_exit.c (the write-back routine). Needs
# clang and the headers of the GHC that stack uses (the stripping is
# gen-helpers.hs, run with `stack runghc`). Run from anywhere.
#
# Each .ll's header names the SHA-256 of its sources (the .c and the headers
# it includes), so that a change to the C without a regeneration shows in a
# diff, and so that `--check` can say which happened: `--check` compares the
# header's hash with the sources' and regenerates to temporary files and
# diffs them; either difference fails. (The hash is a reviewer's aid: the
# guarantee is the regeneration, which CI runs where clang and the headers
# are, test.yaml's build job.)
set -euo pipefail
cd "$(dirname "$0")"
libdir=$(stack exec -- ghc --print-libdir 2>/dev/null)
inc=$(ls -d "$libdir"/*/rts-*/include | head -n 1)
sources() {
  case "$1" in
    jit_helpers.c) echo jit_helpers.c jit_internal.h jit_rt.h ;;
    jit_exit.c) echo jit_exit.c jit_rt.h ;;
  esac
}
hash_of() { cat $(sources "$1") | shasum -a 256 | cut -d' ' -f1; }
gen() {
  clang -S -emit-llvm -O1 -fno-discard-value-names -I"$inc" -o - "$1" | stack runghc -- gen-helpers.hs "$(hash_of "$1")"
}
status=0
for src in jit_helpers.c jit_exit.c; do
  ll="${src%.c}.ll"
  if [ "${1:-}" = "--check" ]; then
    want=$(hash_of "$src")
    have=$(sed -n 's/^; source sha256: \([0-9a-f]*\).*/\1/p' "$ll" | head -n 1)
    if [ "$have" != "$want" ]; then
      echo "$ll: its header names source hash ${have:-none} but the sources hash to $want: the C changed and the IR was not regenerated (run cbits/gen-helpers.sh)" >&2
      status=1
    fi
    tmp=$(mktemp)
    gen "$src" > "$tmp"
    if diff -u "$ll" "$tmp"; then echo "$ll is up to date"
    else echo "$ll differs from the IR regenerated from $src: run cbits/gen-helpers.sh" >&2; status=1; fi
    rm -f "$tmp"
  else
    tmp=$(mktemp)
    gen "$src" > "$tmp"
    mv "$tmp" "$ll"
    echo "wrote $(pwd)/$ll ($(wc -l < "$ll") lines)"
  fi
done
exit $status
