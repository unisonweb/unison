#!/bin/bash
# Continuations serialized under each optimization setting and resumed under
# each other (JIT/design.md, "Continuations across processes"): runs
# unison-src/transcripts-manual/jit-continuations.md as a sender under every
# setting, then as a recipient under every setting, each in the
# interpreter, with the JIT on and eager. Prints every result line; exits 1
# if any says FAIL or a run does not finish. Needs the jit testing codebase
# (scripts/testing-codebase.sh jit) and a build (stack build; set
# STACK_WORK_DIR for an optimized one, e.g. .stack-work-opt).
set -u
cd "$(dirname "$0")/.."
CB=$(scripts/testing-codebase.sh jit)
T=unison-src/transcripts-manual/jit-continuations.md
DIR=$(mktemp -d)
WORK=${STACK_WORK_DIR:+--work-dir $STACK_WORK_DIR}
status=0
setting() {
  case $1 in
    all) echo "" ;;
    none) echo "none" ;;
    args) echo "-explode_returns" ;;
    returns) echo "-explode_args" ;;
  esac
}
run() {
  local jit=$1 role=$2 label=$3
  echo "=== UNISON_JIT=$jit $role $label (UNISON_OPTIMIZATIONS=$(setting $label))"
  local out
  out=$(UNISON_JIT=$jit UNISON_OPTIMIZATIONS="$(setting $label)" UNISON_CONT_DIR=$DIR UNISON_CONT_ROLE=$role UNISON_CONT_LABEL=$label \
    stack exec $WORK unison -- -C "$CB" transcript.fork $T 2>&1 | tr '\r' '\n' | sed 's/.*Processing stanza [0-9]* of [0-9]*\.//')
  # the result lines (the transcript's own text, echoed on an error, also says FAIL)
  echo "$out" | grep -E "^[A-Za-z]+ [0-9]+ (sent by|from) "
  if ! echo "$out" | grep -q "Completed transcript"; then echo "=== DID NOT FINISH"; echo "$out" | grep -A3 "runtime error\|unhandled exception\|error is" | tail -8; status=1; fi
  if echo "$out" | grep -E "^[A-Za-z]+ [0-9]+ (sent by|from) " | grep -q "FAIL"; then status=1; fi
}
for jit in off on eager; do
  for s in all none args returns; do run $jit send $s; done
  for r in all none args returns; do run $jit recv $r; done
done
rm -rf "$DIR"
if [ $status = 0 ]; then echo "=== all combinations passed"; else echo "=== FAILURES"; fi
exit $status
