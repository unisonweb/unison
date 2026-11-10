#!/usr/bin/env python3
"""usage: metrics.py <round prefix, e.g. r8>: one line per worker run: name, calls, learn calls, failed stanzas, topics"""
import json, os, sys, glob
B = os.path.join(os.environ.get("BENCH_ROOT", os.path.expanduser("~/.cache/unison-mcp-bench")), "bench")
for d in sorted(glob.glob(os.path.join(B, sys.argv[1] + "[hs]-*"))):
    if "-j-" in d: continue
    f = os.path.join(d, "calls.log")
    if not os.path.exists(f): print(os.path.basename(d), "no calls"); continue
    rows = [json.loads(l) for l in open(f) if l.strip()]
    learn = [r["learn_path"] or "" for r in rows if r["tool"] == "learn"]
    errs = sum(1 for r in rows if r["tool"] != "learn" and r["isError"])
    print(os.path.basename(d), len(rows), "learn", len(learn), "errs", errs, ",".join(learn))
