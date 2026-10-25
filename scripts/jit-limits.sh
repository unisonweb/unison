#!/usr/bin/env bash
# Runs the programs whose expected outcome is a controlled failure of the whole
# process, which a transcript can't record: allocation sizes no heap can hold.
# Each is run with the JIT off and eager; both must end the same way the
# interpreter alone does (GHC's heap-overflow handler, exit 251, or Data.Text's
# size overflow error, exit 1), never with a signal (a crash inside native
# code or a helper) or a "[jit]" abort. Needs a built ucm (UCM overrides the
# stack-built one) and a codebase with the builtins (JIT_CODEBASE, default
# ./jit_codebase, else scripts/testing-codebase.sh jit).
set -uo pipefail
cd "$(dirname "$0")/.."
UCM=${UCM:-}
codebase=${JIT_CODEBASE:-jit_codebase}
if [ ! -d "$codebase" ]; then codebase=$(scripts/testing-codebase.sh jit); fi
run_ucm() { if [ -n "$UCM" ]; then "$UCM" "$@"; else stack exec unison -- "$@"; fi; }
tmp=$(mktemp -d "${TMPDIR:-/tmp}/jit-limits.XXXXXX") || exit 1
trap 'rm -rf "$tmp"' EXIT

# expression | expected exit code | description
cases=(
  "MutableByteArray.size (IO.bytearray 9223372036854775807)|251|byte array of 2^63 - 1 bytes"
  "MutableArray.size (IO.array 4611686018427387904)|251|array of 2^62 elements"
  "MutableArray.size (IO.arrayOf 0 1152921504606846976)|251|array of 2^60 elements with a value"
  "Scope.run do MutableByteArray.size (Scope.bytearray 9223372036854775807)|251|scoped byte array of 2^63 - 1 bytes"
  "Scope.run do MutableByteArray.size (PinnedByteArray.cast (Scope.pinnedByteArray 9223372036854775807))|251|pinned byte array of 2^63 - 1 bytes"
  "Text.size (Text.repeat 4611686018427387904 \"ab\")|1|text of 2^63 characters"
)
status=0
n=0
for row in "${cases[@]}"; do
  IFS='|' read -r expr want what <<<"$row"
  n=$((n + 1))
  for mode in off eager; do
    dir="$tmp/$n-$mode"
    mkdir -p "$dir"
    cat >"$dir/case.md" <<MD
\`\`\` ucm :hide
scratch/main> builtins.mergeio
\`\`\`

\`\`\` unison
huge : '{IO} Nat
huge = do $expr
\`\`\`

\`\`\` ucm
scratch/main> run huge
\`\`\`
MD
    UNISON_JIT=$mode UNISON_JIT_LOG=1 run_ucm -C "$codebase" transcript.fork "$dir/case.md" >"$dir/log.txt" 2>&1
    got=$?
    verdict=ok
    if [ "$got" -ne "$want" ]; then verdict="exit $got, wanted $want"; fi
    # a signal death is 128 + the signal number (SIGSEGV 139, SIGBUS 138, SIGABRT 134); 251 is EXIT_HEAPOVERFLOW
    if [ "$got" -gt 128 ] && [ "$got" -lt 160 ]; then verdict="killed by signal $((got - 128))"; fi
    if grep -q '^\[jit\].*abort\|frame records\|out-area' "$dir/log.txt"; then verdict="$verdict; a [jit] abort message"; fi
    if [ "$mode" = eager ] && ! grep -q ': compiling ' "$dir/log.txt"; then verdict="$verdict; the JIT compiled nothing"; fi
    if [ "$verdict" = ok ]; then
      printf 'ok    %-5s %s\n' "$mode" "$what"
    else
      printf 'FAIL  %-5s %s: %s (log: %s)\n' "$mode" "$what" "$verdict" "$dir/log.txt"
      status=1
      trap - EXIT
    fi
  done
done
exit $status
