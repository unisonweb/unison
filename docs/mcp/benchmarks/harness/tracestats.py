#!/usr/bin/env python3
"""usage: tracestats.py <round prefix, e.g. r13>: one line per run from trace.jsonl: calls, learn calls, topic chars, failed calls, transcript chars sent, reply chars, seconds; then the topics read, most read first."""
import json, os, sys, glob, collections
root = os.path.join(os.path.expanduser(os.environ.get("BENCH_ROOT", "~/.cache/unison-mcp-bench")), "bench")
topics = collections.Counter(); runs = 0
print("run calls learn learnK fail sentK replyK secs")
for d in sorted(glob.glob(os.path.join(root, sys.argv[1] + "[hs]-*"))):
    f = os.path.join(d, "trace.jsonl")
    if "-j-" in d or not os.path.exists(f): continue
    rows = [json.loads(l) for l in open(f) if l.strip()]
    rows = [r for r in rows if r["tool"] != "tools/list"]
    if not rows: continue
    runs += 1
    lr = [r for r in rows if r["tool"] == "learn"]; tr = [r for r in rows if r["tool"] != "learn"]
    for r in lr: topics[r["arguments"].get("path", "")] += 1
    fail = sum(1 for r in tr if r["reply"].startswith("[isError]"))
    print(os.path.basename(d)[len(sys.argv[1]) + 2:], len(rows), len(lr), sum(len(r["reply"]) for r in lr) // 1000, fail, sum(len(r["input"] or "") for r in tr) // 1000, sum(len(r["reply"]) for r in tr) // 1000, round(rows[-1]["started"] + rows[-1]["seconds"] - rows[0]["started"]))
print(runs, "runs; topics:", ", ".join(f"{k or '(root)'} {v}" for k, v in topics.most_common()))
