#!/usr/bin/env python3
"""usage: trace.py <run dir or name> [max reply chars, default 6000]: prints a run's MCP trace (trace.jsonl) as Markdown, one section per
call: the tool, the time it took, the transcript it was given and its reply. learn replies are left out (they are the topics), only their
path and size are shown, a reply over the limit is cut in the middle, and a transcript much like the one before it is shown as a diff."""
import difflib, json, os, sys
root = os.path.join(os.path.expanduser(os.environ.get("BENCH_ROOT", "~/.cache/unison-mcp-bench")), "bench")
d = sys.argv[1] if os.path.isdir(sys.argv[1]) else os.path.join(root, sys.argv[1])
limit = int(sys.argv[2]) if len(sys.argv) > 2 else 6000
rows = [json.loads(l) for l in open(os.path.join(d, "trace.jsonl")) if l.strip()]
rows = [r for r in rows if r["tool"] != "tools/list"]
print(f"# {os.path.basename(d)}: {len(rows)} calls, {sum(1 for r in rows if r['tool'] == 'learn')} learn, {sum(len(r['reply']) for r in rows)} reply chars, {round(rows[-1]['started'] + rows[-1]['seconds'] - rows[0]['started']) if rows else 0} s")
prev = None
for i, r in enumerate(rows, 1):
    err = " ERROR" if r["reply"].startswith("[isError]") else ""
    if r["tool"] == "learn":
        print(f"\n## {i}. learn({r['arguments'].get('path', '')!r}): {len(r['reply'])} chars{err}")
        continue
    inp = r["input"] if r["input"] is not None else json.dumps(r["arguments"])
    if inp == prev: shown = "(the same transcript as the previous call)"
    elif prev and len(inp) > 300 and difflib.SequenceMatcher(None, prev.splitlines(), inp.splitlines()).ratio() > 0.5:
        shown = "(diff from the previous transcript)\n" + "\n".join(l for l in difflib.unified_diff(prev.splitlines(), inp.splitlines(), lineterm="", n=1) if not l.startswith(("---", "+++")))
    else: shown = inp
    prev = inp
    print(f"\n## {i}. {r['tool']} ({r['seconds']} s, {len(r['reply'])} chars){err}\n\n--- input\n{shown}\n--- reply")
    t = r["reply"]
    print(t if len(t) <= limit else t[:limit // 2] + f"\n[... {len(t) - limit} chars cut by trace.py ...]\n" + t[-limit // 2:])
