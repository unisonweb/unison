#!/usr/bin/env python3
"""usage: ask.py <run dir> learn [path]
          ask.py <run dir> transcript <tool> <file>     (tool: transcript-read-only | transcript | transcript-io | transcript-all)
          ask.py <run dir> tools
Starts the run's server (mcpd.py) if it isn't running."""
import json, os, socket, subprocess, sys, time
d = os.path.abspath(sys.argv[1]); cmd = sys.argv[2]
if cmd == "learn": req = {"tool": "learn", "arguments": {"path": sys.argv[3] if len(sys.argv) > 3 else ""}}
elif cmd == "transcript": req = {"tool": sys.argv[3], "arguments": {"transcript-file": os.path.abspath(sys.argv[4])}}
elif cmd == "tools": req = {"method": "tools/list"}
else: sys.exit(__doc__)
def connect():
    s = socket.socket(); s.connect(("127.0.0.1", int(open(os.path.join(d, "port")).read()))); return s
try: s = connect()
except Exception:
    here = os.path.dirname(os.path.abspath(__file__))
    binary = os.environ.get("BIN") or os.path.join(os.environ.get("BENCH_ROOT", os.path.expanduser("~/.cache/unison-mcp-bench")), "bin", "unison")
    try: os.remove(os.path.join(d, "port"))
    except OSError: pass
    subprocess.Popen([sys.executable, os.path.join(here, "mcpd.py"), binary, d], stdout=subprocess.DEVNULL, stderr=subprocess.DEVNULL, start_new_session=True, env=os.environ)
    for _ in range(120):
        time.sleep(0.5)
        if os.path.exists(os.path.join(d, "port")):
            try: s = connect(); break
            except Exception: pass
    else: sys.exit("could not start the server")
s.sendall((json.dumps(req) + "\n").encode())
out = b""
while True:
    c = s.recv(65536)
    if not c: break
    out += c
print(out.decode())
