#!/usr/bin/env python3
"""usage: ledger.py <round> [--json]: prints one line per run (model:task, ratings) from the runs' judged.json files (and r7-partial.json
for the part of round r7 that was recovered after a crash), and the failures with the judges' reasons."""
import json, os, sys, glob
root = os.path.expanduser(os.environ.get("BENCH_ROOT", "~/.cache/unison-mcp-bench"))
rnd = sys.argv[1]
runs = {}
p = os.path.join(root, f"{rnd}-partial.json")
if os.path.exists(p):
    for k, v in json.load(open(p)).items(): runs[k] = v.get("results", [])
for f in glob.glob(os.path.join(root, "bench", f"{rnd}[hs]-*", "judged.json")):
    name = os.path.basename(os.path.dirname(f)); m = {"h": "haiku", "s": "sonnet"}[name[len(rnd)]]; t = name[len(rnd) + 2:]
    try: runs[f"{m}:{t}"] = json.load(open(f))["results"]
    except Exception as e: print("bad", f, e)
if "--json" in sys.argv: print(json.dumps(runs, indent=1)); sys.exit()
ok = lambda r: r not in ("FAIL",)
tot = {"haiku": [0, 0], "sonnet": [0, 0]}
for k in sorted(runs, key=lambda k: (k.split(":", 1)[1], k)):
    rs = runs[k]; m = k.split(":")[0]
    passed = all(ok(x["rating"]) for x in rs)
    tot[m][0] += passed; tot[m][1] += 1
    print(("pass " if passed else "FAIL ") + k, " ".join(f"{x['judge']}={x['rating']}" for x in rs))
print(tot)
for k in sorted(runs):
    for x in runs[k]:
        if not ok(x["rating"]): print(f"  {k} {x['judge']}: {x['reason'][:260]}")
