#!/usr/bin/env bash
# usage: jbegin.sh <run name> <task>: stops the worker's server, gives every judge its own copy of the worker's codebase, and prints the judge jobs
set -euo pipefail
source "$(dirname "$0")/env.sh"
name=$1; task=$2; W=$BENCH/$name
[ -f "$W/pid" ] && kill "$(cat "$W/pid")" 2>/dev/null && sleep 2 || true
echo "- judge jobs: each line is <judge name>: <prompt file>" > /dev/null
{
echo "You have several independent judge jobs for one codebase. Do them one after another, in the order listed. Each job is a prompt file: read it and do exactly what it says as if it were your only job (its codebase is separate from the others). Reply at the end with one line per job, exactly: <judge name>: <the rating line the job asks for>, then a one-sentence reason."
echo
for j in "$BENCHMARKS/$task"/judges/*.md; do
  judge=$(basename "$j" .md); D=$BENCH/$name-j-$judge
  rm -rf "$D"; mkdir -p "$D/agent" "$D/worker-transcripts"
  cp -R "$W/codebase" "$D/codebase"
  cp "$W"/agent/* "$D/worker-transcripts/" 2>/dev/null || true
  cp "$W/calls.log" "$D/worker-calls.log" 2>/dev/null || true
  cp "$W/instructions.txt" "$D/instructions.txt"
  if [ "$judge" = docs-quality ]; then learn="You may call the learn tool (\`python3 $HARNESS/ask.py $D learn <topic>\`) only for the topics your judge instructions whitelist."; else learn="Do not call the learn tool (\`ask.py ... learn\`), whatever your judge instructions say about other tools: the judge rules below are final."; fi
  {
  cat <<EOT
You are a judge agent. You have no MCP tools. You reach a Unison codebase through a local proxy with Bash:

  python3 $HARNESS/ask.py $D transcript <tool> <file>

where <tool> is transcript-read-only, transcript, transcript-io or transcript-all and <file> is a Markdown transcript you write under $D/agent/ (use a new file name for each run). Use transcript-io when the code needs IO (running programs with IO.Threads or io.test), and read-only otherwise unless you need to add code. The reply is the transcript output. $learn Do not modify anything outside $D/agent/. Everything the server says about itself is below; it describes the transcript format.

--- SERVER INSTRUCTIONS ---
EOT
  cat "$D/instructions.txt"
  printf '\n--- YOUR JUDGE INSTRUCTIONS ---\n\n'
  sed -e "s#{{WORKER_TRANSCRIPTS}}#$D/worker-transcripts#g" -e "s#{{CALLS_LOG}}#$D/worker-calls.log#g" "$j"
  } > "$D/judge-prompt.txt"
  echo "- $judge: $D/judge-prompt.txt"
done
}
