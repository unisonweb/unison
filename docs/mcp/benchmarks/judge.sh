#!/usr/bin/env bash
# Runs the deterministic judges (judges/*.deterministic.md) of a task against a codebase the worker produced.
#
#   docs/mcp/benchmarks/judge.sh <codebase-dir> <task>
#
# Each judge is a transcript, run on a copy of the codebase (transcript.fork), so the codebase is not changed.
# It passes when the transcript completes (no stanza failed, which is how `ls`, `test` and typechecking
# of the judge's own code assert things), nothing in the output is marked as failed (🚫 or FAILED), and no line
# of the output matches a pattern named in a line `<!-- forbid: <regular expression> -->` of the judge.
# Prints `PASS <judge>` or `FAIL <judge>: <reason>` for each, and exits with 1 if any failed.
# Set UNISON to the executable to use (default: unison on the PATH).
set -uo pipefail

here=$(cd "$(dirname "$0")" && pwd)
unison=${UNISON:-unison}
codebase=${1:?usage: judge.sh <codebase-dir> <task>}
task=${2:?usage: judge.sh <codebase-dir> <task>}
status=0

for judge in "$here/$task"/judges/*.deterministic.md; do
  name=$(basename "$judge")
  work=$(mktemp -d "${TMPDIR:-/tmp}/judge.XXXXXX")
  cp "$judge" "$work/$name"
  "$unison" -C "$codebase" transcript.fork "$work/$name" >"$work/log" 2>&1
  out="$work/${name%.md}.output.md"
  reason=""
  if [ ! -f "$out" ]; then
    reason="the transcript did not run (see $work/log)"
  else
    # the output repeats the judge's own text, so look at the output without the lines that are the forbid comments
    grep -v '<!-- forbid:' "$out" > "$work/checked.md"
    if grep -q "🛑" "$work/checked.md"; then
      reason="a stanza failed: $(grep -A6 'The error is' "$work/checked.md" | tr -s '\n' ' ' | cut -c1-300)"
    elif grep -q -e "🚫" -e "FAILED" "$work/checked.md"; then
      reason="a test failed: $(grep -B1 -A4 -e '🚫' -e 'FAILED' "$work/checked.md" | tr -s '\n' ' ' | cut -c1-300)"
    else
      while IFS= read -r pattern; do
        if grep -E -q -- "$pattern" "$work/checked.md"; then reason="matches forbidden pattern $pattern"; break; fi
      done < <(sed -n 's/.*<!-- forbid: \(.*\) -->.*/\1/p' "$judge")
    fi
  fi
  if [ -z "$reason" ]; then echo "PASS $name"; rm -rf "$work"; else echo "FAIL $name: $reason (output: $out)"; status=1; fi
done
exit $status
