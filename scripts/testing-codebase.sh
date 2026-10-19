#!/usr/bin/env bash
# Prints the directory of a codebase from https://github.com/unisonweb/testing-codebases,
# downloading and unpacking it first if it isn't there yet. The directory is named by the
# archive's SHA-256, under $TMPDIR (or TESTING_CODEBASES_DIR), so a new release gets a new
# directory and an old one is never half-updated. Use it where a transcript needs a
# codebase that is too large for this repository:
#
#   stack exec unison -- -C "$(scripts/testing-codebase.sh jit)" transcript.fork unison-src/transcripts-manual/jit-benchmarks.md
#
# transcript.fork copies the codebase, so the cached one stays as downloaded.
set -euo pipefail

name=${1:-}
case "$name" in
  jit)
    # project jit-tests, branch main: the @pchiusano/misc-benchmarks suite and base 7.19.2,
    # json 1.3.4, cloud 20.15.1, benchmark 2.0.0 (unison-runtime/src/Unison/Runtime/JIT/development.md)
    release=jit-2026-10-19
    asset=jit-codebase.tar.gz
    sha=a484352d310f32121e535c621170148546ada1ce3fcac821d26b2fdbb793b4a4
    ;;
  *)
    echo "usage: $0 jit" >&2
    exit 2
    ;;
esac

url="https://github.com/unisonweb/testing-codebases/releases/download/$release/$asset"
cache=${TESTING_CODEBASES_DIR:-${TMPDIR:-/tmp}/unison-testing-codebases}
dir="$cache/$sha"

if [ ! -f "$dir/.unison/v2/unison.sqlite3" ]; then
  mkdir -p "$cache"
  tmp=$(mktemp -d "$cache/download.XXXXXX")
  trap 'rm -rf "$tmp"' EXIT
  echo "downloading $url" >&2
  curl -fsSL --retry 3 "$url" -o "$tmp/$asset"
  if command -v sha256sum >/dev/null; then got=$(sha256sum "$tmp/$asset" | cut -d' ' -f1); else got=$(shasum -a 256 "$tmp/$asset" | cut -d' ' -f1); fi
  if [ "$got" != "$sha" ]; then
    echo "$asset: SHA-256 is $got, expected $sha" >&2
    exit 1
  fi
  tar xzf "$tmp/$asset" -C "$tmp"
  rm "$tmp/$asset"
  mv "$tmp" "$dir" 2>/dev/null || true # lost the race to another run: theirs is the same
  trap - EXIT
fi
echo "$dir"
