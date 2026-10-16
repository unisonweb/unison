#!/usr/bin/env bash
# Runs the JIT's test matrix (unison-runtime/src/Unison/Runtime/JIT/development.md,
# "The test matrix") on unison-src/transcripts/idempotent/jit-tests.md and prints
# a pass/fail line per configuration. A configuration passes when the transcript
# completes, its output equals its input, and the log shows the JIT was in the
# state the configuration asks for (activated and compiling, or off).
#
#   scripts/jit-test-matrix.sh                 # the whole matrix, fast build
#   scripts/jit-test-matrix.sh --opt           # on the optimized build (.stack-work-opt)
#   scripts/jit-test-matrix.sh --debug         # on the debug RTS (.stack-work-debug), +RTS -DS
#   scripts/jit-test-matrix.sh --list          # print the rows and stop
#   scripts/jit-test-matrix.sh on stress-all   # only the named rows
#
# Each run gets its own copy of the transcript in a temporary directory, so the
# checked-in file is never touched and no .output.md is left behind. Runs are
# sequential: two transcript.fork runs started at the same moment can race on the
# codebase copy. Logs of failed runs are kept and their paths printed.
set -u
cd "$(dirname "$0")/.."

workdir=""
rts=""
list=0
rows=()
for a in "$@"; do
  case "$a" in
    --opt) workdir="--work-dir .stack-work-opt" ;;
    --debug) workdir="--work-dir .stack-work-debug"; rts="+RTS -DS -RTS" ;;
    --optdebug) workdir="--work-dir .stack-work-optdebug"; rts="+RTS -DS -RTS" ;;
    --list) list=1 ;;
    -h|--help) sed -n '2,16p' "$0"; exit 0 ;;
    *) rows+=("$a") ;;
  esac
done

transcript=unison-src/transcripts/idempotent/jit-tests.md
codebase=jit_codebase
combo="callee=2,cstack=4096,poll=5,ustack=4,alloc=64,pool=8"
costs="UNISON_JIT_EXIT_COST=0 UNISON_JIT_ENTRY_COST=0"

# name | environment (space separated VAR=value) | what it exercises
matrix=(
  "off|UNISON_JIT=off|the interpreter alone"
  "eager|UNISON_JIT=eager|everything compiled as loaded"
  "on|UNISON_JIT=on|what gets hot, in the background"
  "on-t1|UNISON_JIT=on UNISON_JIT_THRESHOLD=1|every re-entry point on first use"
  "stress-poll|UNISON_JIT=eager UNISON_JIT_STRESS=poll=3 $costs|the preemption poll"
  "stress-ustack|UNISON_JIT=eager UNISON_JIT_STRESS=ustack=4 $costs|Unison stack growth"
  "stress-cstack|UNISON_JIT=eager UNISON_JIT_STRESS=cstack=2048 $costs|the C stack guard"
  "stress-callee|UNISON_JIT=eager UNISON_JIT_STRESS=callee=3 $costs|callees treated as not compiled"
  "stress-alloc|UNISON_JIT=eager UNISON_JIT_STRESS=alloc=64 $costs|the allocation budget"
  "stress-all|UNISON_JIT=eager UNISON_JIT_STRESS=$combo $costs|all the exit paths together"
  "install|UNISON_JIT=on UNISON_JIT_THRESHOLD=1 UNISON_JIT_STRESS=install=5|code installed while the program runs"
  "install-all|UNISON_JIT=on UNISON_JIT_THRESHOLD=1 UNISON_JIT_STRESS=install=5,$combo $costs|installation under the stress combination"
  "no-worker|UNISON_JIT=eager UNISON_JIT_DISABLE=worker|the uniform form everywhere"
  "no-helpers|UNISON_JIT=eager UNISON_JIT_DISABLE=list,text,bytes,name|the fallback paths of the C helpers"
  "no-irhelpers|UNISON_JIT=eager UNISON_JIT_STRESS=$combo UNISON_JIT_DISABLE=irhelpers $costs|the C copies of the IR helpers"
  "no-requests|UNISON_JIT=eager UNISON_JIT_STRESS=$combo UNISON_JIT_DISABLE=requests $costs|every ability request exits"
  "no-records|UNISON_JIT=eager UNISON_JIT_STRESS=$combo UNISON_JIT_DISABLE=records $costs|every record instruction exits"
  "selftests|UNISON_JIT=eager UNISON_JIT_STRESS=lists=20000,texts=20000,bytes=200000|the helper self-tests"
)

if [ "$list" = 1 ]; then
  for row in "${matrix[@]}"; do
    IFS='|' read -r name env what <<<"$row"
    printf '%-14s %s\n               %s\n' "$name" "$what" "$env"
  done
  exit 0
fi

tmp=$(mktemp -d -t jit-matrix)
failed=0
total=0
printf '%-14s %-6s %s\n' CONFIG RESULT NOTE
for row in "${matrix[@]}"; do
  IFS='|' read -r name env what <<<"$row"
  if [ ${#rows[@]} -gt 0 ]; then
    keep=0
    for r in "${rows[@]}"; do [ "$r" = "$name" ] && keep=1; done
    [ $keep = 1 ] || continue
  fi
  total=$((total + 1))
  dir="$tmp/$name"
  mkdir -p "$dir"
  cp "$transcript" "$dir/t.md"
  log="$dir/log"
  start=$(date +%s)
  # shellcheck disable=SC2086
  env $env UNISON_JIT_LOG=1 stack exec $workdir unison -- -C "$codebase" transcript.fork "$dir/t.md" $rts >"$log" 2>&1
  code=$?
  secs=$(( $(date +%s) - start ))
  note=""
  if [ $code -ne 0 ]; then
    note="exit code $code"
  elif ! grep -q 'Completed transcript' "$log"; then
    note="did not complete"
  elif ! cmp -s "$dir/t.md" "$dir/t.output.md"; then
    note="output differs: diff $dir/t.md $dir/t.output.md"
  elif [[ "$env" == *"UNISON_JIT=off"* ]]; then
    grep -q 'JIT compilation activated' "$log" && note="the JIT was on"
  elif grep -q 'the JIT is off' "$log"; then
    note="the JIT was off: $(grep -m1 'the JIT is off' "$log")"
  elif ! grep -q ': compiling ' "$log"; then
    note="nothing was compiled"
  fi
  if [ -z "$note" ]; then
    printf '%-14s %-6s %ss\n' "$name" PASS "$secs"
    rm -rf "$dir"
  else
    printf '%-14s %-6s %ss  %s  (log: %s)\n' "$name" FAIL "$secs" "$note" "$log"
    failed=$((failed + 1))
  fi
done
echo
if [ $failed = 0 ]; then
  echo "all $total passed"
  rmdir "$tmp" 2>/dev/null
else
  echo "$failed of $total failed; logs under $tmp"
  exit 1
fi
