# minify: removing unreachable definitions from a linked GHC executable

`minify` takes an executable that GHC linked and writes a smaller one that
behaves the same. It removes every definition that nothing reachable from
`main` or from the runtime system refers to, and compacts what remains so
the file shrinks by what was removed. A second mode removes named
definitions first, by prefix, so that whole modules or packages can be cut
out of a binary even when they are reachable; a cut definition that is still
referred to becomes an abort that names it.

The input is the only source of information: there are no object files, no
link map and no relocations. The symbol table has to be present, since it is
what makes "a definition" a thing in the file at all.

## Formats and architectures

The tool reads Mach-O and ELF executables for AArch64 and x86-64. The
format reader turns the file into one description the rest of the tool
works with: segments (Mach-O segments, ELF program headers of type
PT_LOAD), sections with a role in compaction (code, strings, constants,
static closures, or none), symbols with a class (an external definition, a
C compiler's local, a GHC local label, or a mapping symbol to ignore), the
pointer slots the loader fixes up, and the slots it binds to named
symbols. The architecture module sits behind a small set of hooks: given
a definition, it decodes code from a label, sweeps fixed code for what it
names, and reports references, labels and pins; the analysis knows nothing
about instructions and the architecture modules nothing about definitions.
A reference carries its kind and, for the x86-64 kinds and jump table
entries, the anchor the displacement is measured from. The writers share
the compaction of the pieces and differ in what surrounds them.

### x86-64

GHC's native code generator names addresses through rel32 branches and
calls, RIP-relative operands, and, in a non-PIE executable, sign-extended
32-bit immediates and displacements. Decoding is by instruction length: a
decoder that knows every legacy, VEX and EVEX encoding well enough to find
the end of an instruction and the position of its displacement and
immediate fields. A definition is decoded by recursive descent as on
AArch64, at byte granularity, stopping at an unconditional jump, a return
or a trap. A call into a Haskell definition's region means C code lives
there and pins its target, as a `bl` does on AArch64. A RIP-relative
operand whose target is in the code section is a label with a table (a
return point) unless it begins a jump table.

Switches with dense cases compile to a jump table. On Darwin the table
lives in the text section after the procedure's code and holds 64-bit
displacements from the table; on Linux with PIC it lives in read-only data
and holds 32-bit displacements; without PIC it holds absolute addresses.
The lookup is one contiguous sequence, `lea T(%rip), r` then `add (r, i,
8), r` and `jmp *r` (or `movslq (r, i, 4), c; add c, r; jmp *r`), with
spills and reloads of other registers possibly between, so a table is
recognised at the `lea` by looking at the instructions after it. The
tables of one procedure are adjacent and their entries look alike, so a
table's entries can only be read once the address of the next one is
known: every definition's bytes are scanned for the lookup idiom before any
table is read, the discovery passes repeat until no new table turns up,
and an entry whose target lands inside an instruction already decoded is
taken as the end of the table. Each entry is a reference from the table to
a block (invariant when both move together, rewritten when the table is in
read-only data) and a label to decode from. On Darwin the 32-bit
`.long info - dsp` datum that GHC's dead-strip preventer leaves after each
procedure is data the descent never reaches; the `_dsp_` symbols are
ignored, and the `L`-prefixed local symbols Darwin keeps for return points
and local closures are carve points when their table is a closure's.

The macOS x86-64 linker dead-strips at the symbol level (GHC emits
`.subsections_via_symbols` there), so little is unreachable in such a
binary; the gains come from folding, constructor names and the link-edit
data. x86 needs no branch islands: rel32 reaches 2 GB.

### ELF

A PT_LOAD program header is a segment; `.text`, `.rodata` and `.data` are
the compacted sections, mixing C and Haskell as on Mach-O. GHC's local
labels survive in the symbol table as untyped local symbols (constructor
name strings among them), which are carve points of their own; the C
compiler's statics are typed locals and begin fixed regions; the mapping
symbols `$x` and `$d` are ignored. The program header table cannot grow in
place, so the output's goes in the patch area in the code section, and the
holes are capped so that the table fits the one page the kernel loads it
into (73 headers of 56 bytes). Each piece keeps its address congruent to
its file offset modulo the page size, which is the largest alignment the
program headers ask for (64 KB for Linux arm64 links). The section headers
are rebuilt with the compacted sections cut into `.text`, `.text.1` and so
on, the symbol table relocated and renumbered, the dynamic symbols' values
moved with their definitions, and every relocation's slot moved with its
definition (a bind can sit in a Haskell closure, the payload of a
`foreign import "&f"`). In a position-independent executable the relative
relocations are the pointer slots, like Mach-O's rebases, and their addends
are relocated; the ones whose slots were removed are pointed at a scratch
word, since the loader applies the first DT_RELACOUNT entries without
looking at their type. The GOT's entries have no relocations of their own
in an executable, so they are read directly: a GOT entry is an edge from
the code that loads it to what it holds, not a root, and one that fixed
code names is a root. A table of closures in a constant section, indexed at
run time, is a root as before.

### Pointer slots without a relocation list

A Linux executable GHC links by default is not position independent and
carries no relative relocations: nothing lists the pointer slots in data.
With `-optl-fuse-ld=bfd -optl-Wl,--emit-relocs` the linker keeps every
relocation of the link in non-loaded sections, and their absolute 64-bit
entries are an exact list, which the reader uses; a PIE link gives the
same guarantee. Without either, the tool parses the static closures: from
every Haskell data symbol, every address code or a GOT entry names in
data, every SRT, and every pointer field found, the info table's layout
gives the pointer fields, and a static thunk's leading words are looked at
as well since the precomputed `unpackCString#` closure keeps its string
address behind the header. A non-pointer field whose value lands in a
Haskell string is taken for an `Addr#` literal. This is a heuristic in two
directions: an integer field whose value falls inside a Haskell definition
would be rewritten, and a closure shape not modelled is missed, so writing
an output on that basis needs `--guess-pointers`. Pointers in C data are
never rewritten there, only pinned. The C compiler's unlabeled constants
lie in the region of whatever symbol precedes them, so a pointer-like word
in a Haskell string region pins its target too.

## What GHC leaves in the file

GHC compiles every top-level binding to a handful of symbols: `f_info` (an
info table followed by the entry code, in `__text`), `f_closure` (a static
closure in `__data`), and for data constructors `C_con_info`, plus string
literals as `f1_bytes` in `__cstring`. The names are z-encoded
`package_Module_name_kind`. Every one of them is an external symbol in the
linked file, so the file carries a complete, sorted map from address to
definition. Local closures (`sat_s1Xy_info`) have no symbol of their own;
their info table and code sit inside the region of the external symbol that
precedes them. A region bounded by two consecutive symbols is therefore the
unit this tool works with: a *definition* is the bytes from one external
symbol to the next, less the info table that belongs to the next.

That region is then split where something inside it can live on its own.
In code, a label with a function, thunk or constructor table inside the
region begins a local closure (a return-point table belongs to its
procedure, so those don't split). In data, a referenced closure start, a
word that is itself a pointer slot at least a word in, begins another
static closure. In strings, a constructor name begins its own string. The
labels come out of the decoding described below, so splitting happens
after it and the references are then attributed by address. A dead local
closure, a dead static closure or a dead constructor name goes alone
instead of with its named neighbour; on unison this is the difference
between 36% and 57% of the compactable bytes being removable.

The info table sits before the code its label points at (tables next to
code). Its size follows from the closure type in the table: 16 bytes for
thunks and return points, 24 for constructors (a `con_desc` offset first)
and for functions whose argument pattern is one of the standard ones, 40 for
functions with a general argument pattern (`slow_apply` and bitmap offsets
first). The table holds relative offsets rather than pointers: the SRT
offset in the last 4 bytes, `con_desc`, `slow_apply_offset`, and large
bitmap offsets. Each is measured from the info label, so moving a definition
relative to its target means rewriting them. In a constructor's table the
last 4 bytes are the constructor tag, not an offset; reading them as one
made every constructor's tag a "reference" to whatever lay that many bytes
past the label, and rewriting it corrupted the tags of types with more than
seven constructors whenever that neighbour moved by a different amount.
In-order compaction moved neighbours together and hid this; folding exposed
it.

References from code use a small, closed set of forms on AArch64. GHC's
native code generator emits `adrp` followed immediately by `add` for the
address of a label, `adrp` followed by `ldr` for a GOT entry, `b` for tail
calls and jumps, `bl` for foreign calls, and `b.cond`, `cbz`, `cbnz` within
a function. It emits no jump tables (switches are compare-and-branch
chains), no literal pools and no `adr`. Static closures in `__data` refer to
other definitions through absolute pointers, every one of which has a dyld
rebase entry, so the rebase stream is an exact list of the pointer slots in
data. These are the only references that have to be found and rewritten,
and the tool fails rather than guess when it meets an instruction outside
the set in code it intends to move.

Code the C compiler produced has none of these guarantees (its `adrp` may be
scheduled away from its `add` and shared, and its switch tables are relative
offsets in `__const`). It is interleaved with the Haskell code: the runtime
system, the C parts of libraries such as sqlite and crypton, GHC's foreign
call wrappers, about seventy stretches in all. The tool never moves it.

## The approach

Nothing C made moves, and no address it refers to changes. Haskell
definitions are compacted into the gaps between the fixed stretches, each
definition landing at or below its old address, and every reference inside
moved code and data is rewritten. Where compaction leaves a gap of whole
pages before a fixed stretch, the file omits those pages: the segment is
split there into two segments whose virtual addresses are unchanged, so
fixed code keeps its addresses while the file loses the pages between.

dyld allows one segment named `__TEXT` (the one holding the header), so the
later pieces are `__TEXT_1`, `__TEXT_2` and so on, and `__DATA_1` for data.
Its rebase and bind opcodes name a segment with a four-bit immediate, which
would allow 16 segments in all, but only the data segments ever hold a
pointer slot, so the extra `__TEXT` pieces are listed after the data
segments in the load commands and the data segments keep small indices
however many pieces `__TEXT` is cut into. dyld does not require the load
commands to be in address order. The holes in the data segments are
limited by the budget that remains; the largest are taken.

Classification is by name: a symbol of the form `package_Module_name_kind`
with a z-encoded module (an uppercase initial) and one of GHC's kind
suffixes is a Haskell definition; everything else is fixed. The `stg_*`
procedures of the runtime are written in Cmm and compiled by the same code
generator, but they are classified fixed with the rest of the runtime,
which costs nothing.

### Finding the references

The code of a movable definition is decoded by recursive descent from its
labels, not by a linear sweep: info tables and padding lie between the basic
blocks, and a linear sweep would read them as instructions. A label is the
definition's own symbol, any address inside the definition that an
`adrp`/`add` pair or a branch anywhere in the file points at, and any
address a rebase pointer holds (a static closure's info pointer names the
code of a local closure). Decoding runs from each label to the first
unconditional transfer (`b`, `br`, `ret`), adding branch targets inside the
definition as new labels. The set of labels grows as other definitions are
decoded, so the whole thing is a fixpoint over a worklist. Code never
reached this way is dead local code and travels with its definition
untouched. Each info table found before a label is parsed for its relative
fields, and the slow-apply field of a general-arity function table is a
label too: the slow entry is code with no table of its own (a local
function's sits unlabeled in the region before its fast entry), and its
jump back to the fast entry is a reference like any other. Missing that was
the one case where in-order compaction happened to work, since neighbours
kept their distance, and anything that changed a distance within a module
(folding, moving a definition past a fixed one) crashed the slow path.

Fixed code is swept linearly for `adrp`, `adr`, `b`, `bl` and literal loads
whose target falls in a movable definition. An `adrp` is paired with its
`add` and load consumers when the register's uses can all be followed to
the point where it is overwritten (no branch in between, a bounded window)
and each consumer's target is pinned; when they cannot, the target's 4 KB
page is pinned, and every definition overlapping a pinned page keeps its
address. This is how the runtime's references to `ZCMain_main_closure`,
the exception closures and the rest become roots and stay where C expects
them. Pointer slots in fixed data are not pins: a pointer is an absolute
address and is rewritten like any other reference, so a C table may point
at a closure that moves. GHC code also turns up inside fixed regions (the
unlabeled lead of `__text` holds the main module's local closures; a local
closure can follow a C symbol), where only its instructions are swept, so a
label with an info table that lands in a fixed region has its table parsed
as well: its SRT field is rewritten, and its other fields, which a
hand-written runtime table may not have, pin what they point at.

### Reachability

Roots are all fixed definitions and all pinned ones. Edges run from a
definition to each definition one of its references lands in: decoded
instructions, info table offsets, rebase pointers within its range. A
reference landing anywhere inside a definition keeps the whole definition,
so a live local closure keeps its host. Marking is a plain traversal.

In prefix mode, definitions whose decoded name (`Module.name`, or
`package:Module.name`) starts with a given prefix are cut before marking.
Nothing is marked through a cut definition. A cut definition that a kept one
still refers to is replaced: an `_info` by a copy of its info table (SRT
cleared) whose code calls `barf` with a message naming the definition, and
a `_closure` by a static function closure whose entry does the same.
Entering either aborts the program with that message. Inspecting such a
value without entering it, as code does with a static constructor, is
undefined; the report lists every replacement so this can be seen. Cut
literals that are still referred to are kept.

### Layout and rewriting

Within each compacted section (`__text`, `__cstring`, `__const` in `__TEXT`,
and `__data`), fixed and pinned definitions are anchors at their old
addresses and the movable ones fill the gaps before each anchor, first-fit
in address order with a bounded look-ahead for one that fits when the next
in line does not, each aligned as its old address was (to at most 8). Dead
space collects at the end of each section, where whole pages leave the
file.

Before placement, identical definitions are folded. Two kept, movable
definitions of code or strings whose bytes agree once their reference
fields are masked, and whose references agree once each target is named
canonically (the representative of the target's definition and the offset
into it, or the fixed address), share one copy, and every address inside
the folded one is routed to the representative. GHC emits a great deal of
byte-identical code (dictionary selectors, derived instances,
specialisations), and nothing compares code or string addresses for
identity. Static closures are never folded: a CAF or a constructor is
observable by identity. Folding a target makes more pairs equal, so the
grouping is iterated to a fixed point. On unison this folds 78,841
definitions, 10.7 MB.

Bytes in a compacted section that no kept definition, island or stub
occupies are zeroed, so neither a removed definition nor the old copy of a
moved one survives in the file.

Every reference is then re-encoded for the new addresses of both ends:
`adrp` gets the new page difference and its `add` or `ldr` the new low 12
bits; `b` and `bl` get the new displacement; the conditional branches, which
stay within a definition, are re-encoded the same way; info table offsets
become the new target minus the new label; rebase slots get the new value,
tag bits preserved, wherever the slot is, moved or fixed.

A `b` or `bl` reaches 128 MB either way. `__text` is longer than that, and
the original only linked because ld64 planted branch islands, which are
ordinary code to this tool and travel with their definitions. Compaction
moves early code further from the runtime at the far end, so a few branches
stop reaching (22 in unison). For each target of such a branch the tool
writes an island, `adrp x16; add x16; br x16`, in a patch area placed in
the gap nearest the middle of `__text`, which every branch in the section
reaches; x16 is the register the ABI reserves for exactly this. The abort
stubs for cut definitions and their message strings go in the same area,
and the closure stubs in a gap in `__data`, since the garbage collector
writes to a static closure's link field.

The link-edit data is rebuilt: the symbol table keeps the surviving symbols
at their new addresses and in their new sections, with the indirect symbol
table renumbered; the export trie, which answers `dlsym` by name and which
GHC fills with every symbol, is regenerated without the Haskell definitions
unless `--keep-haskell-exports` says otherwise (nothing looks a closure up
by name; the C helpers the JIT resolves stay); the rebase stream is
regenerated from the surviving slots plus the closure stubs' info words;
binds are re-emitted against the new segment numbering with lazy binds
converted to ordinary ones (the lazy stream's byte offsets are baked into
`__stub_helper`, so it cannot be re-encoded in place), and a bind slot
moves with its definition like a rebase does, since a `foreign import "&f"`
is a static closure whose payload dyld binds; function starts are
regenerated, or dropped with `--strip`, which also drops every symbol but
the undefined ones the stubs bind through. Constructor name strings
(`con_desc`), which only heap profiling and `GHC.Exts.Heap` read, are
dropped unless `--keep-con-desc`: every `con_desc` field then names one
empty string. The code signature is dropped and the output is re-signed ad
hoc with `codesign`.

### Checking the result

With `--verify`, the tool analyzes the file it wrote the same way it
analyzed the input and compares: every reference the output's decoding
finds must be one the plan made, at the planned address and pointing at
the planned target, with branches through islands counted as reaching the
island's target. The output may decode fewer references than the input,
since a label that only a removed static closure pointed at is gone and
the code behind it with it, and it may split less finely where the
reference that marked a split point was removed, so each output definition
is checked against the union of the input definitions placed in it. Every
pointer slot in a kept definition must still be rebased, and the bind slots
must be exactly the planned ones. An anomaly in decoding the output fails
the check too. This catches a
mis-rewritten reference without running the program. Running the program
is the other half: the Unison transcript suite against the minified
`unison`, compared with the original binary's output on the same files.

## What it does not do

Mach-O and ELF, AArch64 and x86-64, 64-bit little-endian only. Stripped
binaries cannot be processed. C code is never removed, even when
unreferenced. Dead bytes that do not fill a page stay in the file as zeros.
The unwinder looks for `__eh_frame` under a segment named `__TEXT`, so C
code in a later Mach-O piece has no unwind information at runtime; the GHC
runtime does not use it. A Linux executable without a relocation list is
handled by the heuristic above. RELR relocation sections are not handled.
The relocation sections a link with `--emit-relocs` kept describe the
input and are dropped from the output, so such an output cannot be
minified again exactly.
