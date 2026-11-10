#!/usr/bin/env python3
"""usage: compare.py <round a> <round b>: calls, failed calls and characters per run for two rounds, side by side, with totals; then the failures of round b, by error."""
import json, os, sys, glob, re, collections
root = os.path.join(os.path.expanduser(os.environ.get("BENCH_ROOT", "~/.cache/unison-mcp-bench")), "bench")
def load(rnd):
    out = {}
    for d in glob.glob(os.path.join(root, rnd + "s-*")):
        f = os.path.join(d, "trace.jsonl")
        if "-j-" in d or not os.path.exists(f): continue
        rows = [json.loads(l) for l in open(f) if l.strip()]
        out[os.path.basename(d)[len(rnd) + 2:]] = [r for r in rows if r["tool"] != "tools/list"]
    return out
def stats(rows):
    tr = [r for r in rows if r["tool"] != "learn"]
    return len(rows), sum(1 for r in rows if r["tool"] == "learn"), sum(1 for r in tr if r["reply"].startswith("[isError]")), sum(len(r["reply"]) for r in rows if r["tool"] == "learn"), sum(1 for r in tr if "was written to" in r["reply"])
a, b = load(sys.argv[1]), load(sys.argv[2])
tot = [[0] * 5, [0] * 5]
print(f"{'task':34} calls      learn   fail    tofile")
for t in sorted(a):
    x, y = stats(a[t]), stats(b.get(t, []))
    for i in range(5): tot[0][i] += x[i]; tot[1][i] += y[i]
    print(f"{t:34} {x[0]:3} {y[0]:3}   {x[1]:2} {y[1]:2}   {x[2]:2} {y[2]:2}   {x[4]:2} {y[4]:2}")
print("total calls/learn/fail/learnchars/tofile", tot)
C = collections.Counter(); ex = {}
for t, rows in b.items():
    for r in rows:
        if r["tool"] != "learn" and r["reply"].startswith("[isError]"):
            m = re.search(r"The error is:\s*```\s*\n(.*?)(\n\n|```)", r["reply"], re.S)
            k = re.sub(r"\s+", " ", (m.group(1) if m else r["reply"][:120]).strip())[:90]
            C[k] += 1; ex.setdefault(k, t)
for k, v in C.most_common(): print(v, ex[k], "|", k)
