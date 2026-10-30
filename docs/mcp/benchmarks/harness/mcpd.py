#!/usr/bin/env python3
"""Keeps one MCP server running for a run and answers requests on a local socket. Exits after IDLE_SECONDS (default 900)
without a request, so that finished runs don't keep memory; ask.py starts it again on demand.
usage: mcpd.py <unison executable> <run dir>   (the codebase is <run dir>/codebase)"""
import json, os, socket, subprocess, sys, threading
BIN, DIR = sys.argv[1], os.path.abspath(sys.argv[2])
IDLE = float(os.environ.get("IDLE_SECONDS", "900"))
os.makedirs(DIR, exist_ok=True)
cb = os.path.join(DIR, "codebase")
args = [BIN, "--codebase", cb, "mcp"] if os.path.exists(cb) else [BIN, "--codebase-create", cb, "mcp"]
p = subprocess.Popen(args, stdin=subprocess.PIPE, stdout=subprocess.PIPE, stderr=subprocess.DEVNULL, cwd=DIR, text=True, env={**os.environ, "UCM_MERGETOOL": ""})
n = [0]
def rpc(method, params=None, notify=False):
    n[0] += 1
    msg = {"jsonrpc": "2.0", "method": method}
    if params is not None: msg["params"] = params
    if not notify: msg["id"] = n[0]
    p.stdin.write(json.dumps(msg) + "\n"); p.stdin.flush()
    if notify: return None
    while True:
        line = p.stdout.readline()
        if line == "": return {"error": "server closed"}
        i = line.find('{"')
        if i >= 0: return json.loads(line[i:])
init = rpc("initialize", {"protocolVersion": "2024-11-05", "capabilities": {}, "clientInfo": {"name": "proxy", "version": "0"}})
rpc("notifications/initialized", notify=True)
open(os.path.join(DIR, "instructions.txt"), "w").write(init.get("result", {}).get("instructions", ""))
srv = socket.socket(); srv.bind(("127.0.0.1", 0)); srv.listen(5); srv.settimeout(IDLE)
open(os.path.join(DIR, "pid"), "w").write(str(os.getpid()))
open(os.path.join(DIR, "port"), "w").write(str(srv.getsockname()[1]))
def shutdown():
    for f in ("port", "pid"):
        try: os.remove(os.path.join(DIR, f))
        except OSError: pass
    try: p.stdin.close(); p.wait(timeout=10)
    except Exception: p.kill()
    sys.exit(0)
import signal
signal.signal(signal.SIGTERM, lambda *a: shutdown())
while True:
    try: c, _ = srv.accept()
    except socket.timeout: shutdown()
    data = b""
    while not data.endswith(b"\n"):
        chunk = c.recv(65536)
        if not chunk: break
        data += chunk
    try:
        req = json.loads(data.decode())
        if "tool" in req:
            r = rpc("tools/call", {"name": req["tool"], "arguments": req.get("arguments", {})})
            res = r.get("result", r)
            text = "".join(x.get("text", "") for x in res.get("content", [])) if isinstance(res, dict) else str(res)
            out = ("[isError] " if isinstance(res, dict) and res.get("isError") else "") + text
        else:
            out = json.dumps(rpc(req["method"], req.get("params")), indent=1)
    except Exception as e:
        out = f"proxy error: {e}"
    try:
        with open(os.path.join(DIR, "calls.log"), "a") as lg:
            a = req.get("arguments", {}) if isinstance(req, dict) else {}
            lg.write(json.dumps({"tool": req.get("tool", req.get("method")), "learn_path": a.get("path"), "reply_chars": len(out), "isError": out.startswith("[isError]")}) + "\n")
    except Exception: pass
    try: c.sendall(out.encode()); c.close()
    except Exception: pass
