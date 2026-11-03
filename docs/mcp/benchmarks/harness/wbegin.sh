#!/usr/bin/env bash
# usage: wbegin.sh <run name> <task>: makes a fresh codebase for the task, starts its server, and prints the worker's complete instructions
set -euo pipefail
source "$(dirname "$0")/env.sh"
name=$1; task=$2; D=$BENCH/$name
pkill -f "mcpd.py .* $D\$" 2>/dev/null || true
rm -rf "$D"; mkdir -p "$D/agent"
cb=$(UNISON=$BIN "$BENCHMARKS/prepare.sh" "$task" 2>"$D/prepare.log" | tail -1)
mv "$cb" "$D/codebase"
python3 "$HARNESS/ask.py" "$D" tools > "$D/tools.json"
ASK="python3 $HARNESS/ask.py $D"
cat <<EOT
You are an AI agent. You have a Unison MCP server connected. Your harness exposes its tools to you through Bash, with this client:

  $ASK learn [<topic path>]
  $ASK transcript <tool> <file>        (for example: $ASK transcript transcript-read-only $D/agent/t1.md)

For \`learn\`, the optional argument is the topic path. For the transcript tools, the word \`transcript\` is the subcommand and \`<tool>\` is the tool's name, one of transcript-read-only, transcript, transcript-io or transcript-all, and \`<file>\` is a Markdown transcript you write under $D/agent/ (use a new file name each time). The reply is the tool's output. Treat these as your MCP tools: use only them, plus Bash and file writing under $D/agent/, to do the task. Calls can take several minutes (for example installing a library from Share): run each one in the foreground and wait for its output (give the Bash tool a timeout of 600000 ms); never run them in the background or wait for a notification, because nothing will arrive. Reply as soon as you have finished the task. Don't run unison directly and don't read other files on this machine, except a file that a tool reply names (long replies are saved to a file whose path the reply gives, and some commands write files): you may read and edit the files that tool replies name, wherever they are.

The server's tool definitions, as the MCP client shows them to you:

EOT
python3 - "$D/tools.json" <<'PY'
import json,sys
d=json.load(open(sys.argv[1]))
for t in d['result']['tools']:
    print('### tool `%s`\n\n%s\n\nInput schema: %s\n' % (t['name'], t['description'], json.dumps(t['inputSchema'])))
PY
printf -- '--- SERVER INSTRUCTIONS ---\n'
cat "$D/instructions.txt"
printf '\n--- YOUR TASK ---\n\n'
cat "$BENCHMARKS/$task/prompt.md"
