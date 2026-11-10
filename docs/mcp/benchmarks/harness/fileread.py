#!/usr/bin/env python3
"""usage: fileread.py <workflow transcript dir> [task]: for each worker, the commands that touched a generated `.u` file, and how many characters of tool result came back from them. The MCP trace doesn't show these: they are in the agents' logs."""
import glob, json, os, re, sys
for f in sorted(glob.glob(os.path.join(sys.argv[1], "agent-*.jsonl"))):
    label = json.load(open(f.replace(".jsonl", ".meta.json"))).get("description", "")
    if not label.startswith("worker:") or (len(sys.argv) > 2 and sys.argv[2] not in label): continue
    calls = {}; total = 0
    print("##", label, os.path.basename(f))
    for line in open(f):
        try: r = json.loads(line)
        except ValueError: continue
        m = r.get("message"); c = m.get("content") if isinstance(m, dict) else None
        if not isinstance(c, list): continue
        for x in c:
            if x.get("type") == "tool_use":
                s = json.dumps(x["input"])
                if re.search(r"\.u\b", s) and "ask.py" not in s: calls[x["id"]] = (x["name"], x["input"].get("command") or s)
            elif x.get("type") == "tool_result" and x.get("tool_use_id") in calls:
                n = len(json.dumps(x.get("content"))); total += n
                name, cmd = calls[x["tool_use_id"]]
                print(f"  {n:7} {name}: {cmd[:160]!r}")
    print("  total", total)
