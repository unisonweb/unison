#!/usr/bin/env bash
# Prints the directory of a fresh codebase for a benchmark task, ready for a worker agent:
# project `scratch`, branch `main`, with the base library installed.
#
#   docs/mcp/benchmarks/prepare.sh <task> [<setup>]   (task is optional; <setup>, default <task>/setup.md, runs first)
#
# The base codebase is built once, by running base-codebase.md (this needs the network the first time),
# and cached under $TMPDIR (or BENCHMARK_CODEBASES_DIR) in a directory named for the hash of
# base-codebase.md, so changing the file builds a new one. Every call copies it, so runs never share state.
# Set UNISON to the executable to use (default: unison on the PATH), for example the one stack built:
#   UNISON="$(stack exec -- which unison)" docs/mcp/benchmarks/prepare.sh
set -euo pipefail

here=$(cd "$(dirname "$0")" && pwd)
unison=${UNISON:-unison}
task=${1:-}
setup=${2:-${task:+$here/$task/setup.md}}

if command -v sha256sum >/dev/null; then hash=$(sha256sum "$here/base-codebase.md" | cut -c1-16); else hash=$(shasum -a 256 "$here/base-codebase.md" | cut -c1-16); fi
cache=${BENCHMARK_CODEBASES_DIR:-${TMPDIR:-/tmp}/unison-mcp-benchmarks}
base="$cache/base-$hash"

if [ ! -f "$base/.unison/v2/unison.sqlite3" ]; then
  mkdir -p "$cache"
  tmp=$(mktemp -d "$cache/build.XXXXXX")
  cp "$here/base-codebase.md" "$tmp/base-codebase.md"
  "$unison" transcript -S "$tmp/codebase" "$tmp/base-codebase.md" >&2
  if grep -q "error occurred" "$tmp/base-codebase.output.md" || [ ! -f "$tmp/codebase/.unison/v2/unison.sqlite3" ]; then
    echo "building the base codebase failed; see $tmp/base-codebase.output.md" >&2
    exit 1
  fi
  mv "$tmp/codebase" "$base" 2>/dev/null || true # lost the race to another run: theirs is the same
  rm -rf "$tmp"
fi

run="$cache/run.$(date +%s).$$"
if [ -n "$setup" ] && [ -f "$setup" ]; then
  mkdir -p "$run.tmp"
  cp "$setup" "$run.tmp/setup.md"
  "$unison" -C "$base" transcript.fork -S "$run" "$run.tmp/setup.md" >&2
  if grep -qE "error occurred|🛑" "$run.tmp/setup.output.md"; then echo "setup.md failed; see $run.tmp/setup.output.md" >&2; exit 1; fi
  rm -rf "$run.tmp"
else
  cp -R "$base" "$run"
fi
echo "$run"
