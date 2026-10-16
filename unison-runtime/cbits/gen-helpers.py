#!/usr/bin/env python3
# Makes clang's IR for jit_helpers.c platform-independent: reads the .ll on
# stdin, writes the stripped one on stdout. What goes and why is in "The IR
# helpers" in src/Unison/Runtime/JIT/design.md. Run through gen-helpers.sh.
import re, sys

# function attributes that mean the same on every target
KEEP = {"nounwind", "mustprogress", "nocallback", "nofree", "nosync", "norecurse",
        "willreturn", "inlinehint", "noinline", "alwaysinline", "noreturn", "cold"}

out = []
empty = set()  # attribute groups with nothing left: dropped, with their uses
for line in sys.stdin:
    line = line.rstrip("\n")
    # the module header: clang's name for the file, the target and its data layout
    if line.startswith(("; ModuleID", "source_filename", "target datalayout", "target triple")):
        continue
    # metadata: TBAA, loop hints, the module flags and the compiler's ident
    if line.startswith("!"):
        continue
    if line.startswith("; Function Attrs:"):
        continue
    line = re.sub(r", ![\w.]+ ![0-9]+", "", line)
    # attribute groups: keep the target-neutral attributes and memory(...)
    m = re.match(r"attributes (#\d+) = \{ (.*) \}$", line)
    if m:
        kept = []
        for tok in re.findall(r'memory\([^)]*\)|"[^"]*"(?:="[^"]*")?|[\w.]+(?:\([^)]*\))?', m.group(2)):
            if tok.startswith("memory(") or tok in KEEP:
                kept.append(tok)
        if not kept:
            empty.add(m.group(1))
            continue
        line = f"attributes {m.group(1)} = {{ {' '.join(kept)} }}"
    # the struct return: clang spells the 16-byte pair as an array on arm64
    # and as a struct on x86-64 System V; the generator declares the struct
    line = line.replace("[2 x i64]", "{ i64, i64 }")
    # visibility words that only matter to a static linker
    line = re.sub(r"\b(dso_local|local_unnamed_addr|unnamed_addr) ", "", line)
    out.append(line)

# collapse runs of blank lines left by the removals
text = "\n".join(out)
for g in empty:
    text = re.sub(r" " + re.escape(g) + r"\b", "", text)
text = re.sub(r"\n{3,}", "\n\n", text).strip("\n") + "\n"
sys.stdout.write("; Generated from jit_helpers.c by gen-helpers.sh. Do not edit.\n\n" + text)
