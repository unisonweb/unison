#!/usr/bin/env bash
# A smoke test of bundles and unisonrt (unison-runtime/unisonrt/README.md):
#
#   scripts/bundle-check.sh [--update] [--codebase DIR] UCM UNISONRT [MINIFY]
#
# With MINIFY, unisonrt is first minified as a release minifies it, and the
# minified copy is what is checked; without it (Windows, which minify doesn't
# read), unisonrt is checked as it is. The JIT needs an LLVM it can load.
#
# The programs of unison-src/tests/bundle/programs.md are compiled and
# bundled, by ucm's `bundle` command, by `ucm bundle project/branch:name` and
# by `unisonrt bundle`, and run with the JIT off and on, as bundles and as
# .uc files. What they print and their exit codes must match
# unison-src/tests/bundle/expected.txt (--update rewrites it).
#
# The transcript runs on a copy of DIR (CI uses the JIT tests' codebase), or
# on a new codebase.
set -euo pipefail

update=
codebase=
while [[ ${1:-} == --* ]]; do
  case $1 in
    --update) update=1; shift ;;
    --codebase) codebase=$2; shift 2 ;;
    *) echo "unknown flag $1"; exit 2 ;;
  esac
done
if [[ $# -lt 2 ]]; then
  sed -n '2,/^set /p' "$0" | sed '$d; s/^# \{0,1\}//'
  exit 2
fi

root=$(cd "$(dirname "$0")/.." && pwd)
abs() { (cd "$(dirname "$1")" && echo "$(pwd)/$(basename "$1")"); }
ucm=$(abs "$1")
plain=$(abs "$2")
minify=${3:+$(abs "$3")}
codebase=${codebase:+$(abs "$codebase")}

exe=
case "$(uname -s)" in
  MINGW* | MSYS* | CYGWIN*) exe=.exe ;;
esac

work=$(mktemp -d)
# BUNDLE_CHECK_KEEP=1 keeps the work directory, to look at
[[ -n ${BUNDLE_CHECK_KEEP:-} ]] && echo "work directory: $work" || trap 'rm -rf "$work"' EXIT
cd "$work"

if [[ -n $minify ]]; then
  echo "minifying $plain"
  "$minify" "$plain" -o "unisonrt$exe" --strip --verify | tail -1
  rt=$work/unisonrt$exe
else
  rt=$plain
fi
# what ucm bundles from
export UNISONRT=$rt

# what a command prints, both streams, and its exit code
outcome() {
  local status=0
  "$@" > out.txt 2>&1 || status=$?
  tr -d '\r' < out.txt
  echo "[exit $status]"
}

echo "compiling and bundling"
mkdir programs
cp "$root/unison-src/tests/bundle/programs.md" programs/
if [[ -n $codebase ]]; then
  cp -R "$codebase" codebase
  create=(-C ../codebase)
else
  create=(--codebase-create ../codebase)
fi
(
  cd programs
  "$ucm" "${create[@]}" transcript.in-place programs.md > ../programs.log 2>&1 \
    || { cat ../programs.log; exit 1; }
)
"$ucm" -C codebase bundle bundle-check/main:hello "programs/hello-cli$exe" > /dev/null
"$rt" bundle programs/hello.uc -o "programs/hello-rt$exe" > /dev/null

echo "running"
results=$work/results.txt
{
  for jit in off on; do
    export UNISON_JIT=$jit
    echo "### JIT $jit"
    for b in hello-ucm hello-cli hello-rt; do
      echo "## $b a 'b c' +RTS -N2 -RTS --jit on"
      outcome "programs/$b$exe" a 'b c' +RTS -N2 -RTS --jit on
    done
    echo "## checks-ucm"
    outcome "programs/checks-ucm$exe"
    for p in hello values checks crash raises divZero; do
      echo "## unisonrt $p.uc x"
      outcome "$rt" "programs/$p.uc" x
    done
  done
  unset UNISON_JIT
  echo "### errors"
  echo "## unisonrt with no file"
  outcome "$rt"
  echo "## unisonrt --bogus"
  outcome "$rt" --bogus programs/hello.uc
  echo "## unisonrt on a file that is not a program"
  outcome "$rt" programs/programs.md
} > "$results"

expected=$root/unison-src/tests/bundle/expected.txt
if [[ -n $update ]]; then
  cp "$results" "$expected"
  echo "wrote $expected"
elif ! diff -u "$expected" "$results"; then
  echo "FAIL: the bundled programs printed something else (above)"
  exit 1
fi
echo "bundle check: pass"
