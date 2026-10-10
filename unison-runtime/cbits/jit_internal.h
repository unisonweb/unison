// What jit_rt.c shares with the helpers that are compiled to LLVM IR
// (jit_helpers.c): the layout tables the startup checks fill in, the
// closure accessors over them, and the walkers that stay in C. Everything
// here is a type, a macro, a static inline function (inlined into whichever
// file uses it), an exported `unison_jit_` function, or one of the tables.
// The tables stay private to jit_rt.c (so that its code addresses them
// directly, not through the global offset table); the IR module refers to
// them by name and the JIT is told their addresses before it is added
// (unison_jit_table_addresses, defined as absolute symbols in JIT.hs).
//
// The IR is generated from jit_helpers.c by cbits/gen-helpers.sh and must be
// platform-independent (see "The IR helpers" in internals.md), so nothing in
// this header may depend on the target beyond 64-bit words and little-endian
// byte order: fixed-width types only, `unsigned char` for bytes, no varargs,
// no stdio.

#ifndef UNISON_JIT_INTERNAL_H
#define UNISON_JIT_INTERNAL_H

#include "Rts.h"
#include <stdint.h>

// jit_rt.c defines the tables, with UNISON_JIT_TABLES_HERE set; everyone
// else sees them as external
#ifdef UNISON_JIT_TABLES_HERE
#define UNISON_JIT_TABLE static
#else
#define UNISON_JIT_TABLE extern
#endif

// (ok, value): a helper's answer for a Nat result, or "not handled" when ok
// is 0. Returned as an LLVM `{ i64, i64 }`: both sides of the call are
// compiled by the JIT, so how the pair comes back is LLVM's choice on every
// target.
typedef struct { int64_t ok, v; } JitNat;

#define LTAG(p) GET_CLOSURE_TAG((StgClosure *)(p))
#define LUN(p) UNTAG_CLOSURE((StgClosure *)(p))
#define LP(c, i) (LUN(c)->payload[i])
#define LW(c, i) ((StgInt)LUN(c)->payload[i])

// a list level's packed tag: the size is the high bits ("Lists" in jit_rt.c)
#define TSIZE(t) ((t) >> 8)

// --- lists (Unison.Util.Deque) ---

typedef struct {
  StgWord foreign_info, wrapseq_info, wrapseq_tag;
  StgWord deep_info, mdeep_info, scons_info, n8_info, na_info;
  StgWord val_info, data1_info, data2_info;
  StgClosure *nil, *mnil, *snil; // the constructors without fields (tagged)
} ListFacts;
UNISON_JIT_TABLE ListFacts unison_jit_LF;
#define LF unison_jit_LF

// The deque inside a list value (tagged), or NULL if the closure isn't a list.
static inline StgClosure *deque_of(void *list) {
  if (LTAG(list) != 7 || (StgWord)LUN(list)->header.info != LF.foreign_info) return NULL;
  StgClosure *w = LP(list, 0);
  if (LTAG(w) != LF.wrapseq_tag || (StgWord)LUN(w)->header.info != LF.wrapseq_info) return NULL;
  return LP(w, 0);
}

typedef void (*ItemFn)(StgClosure *item, void *env);

// the elements of a deque level in order: `depth` is how many levels of
// nodes its items hold (0 at the top)
void unison_jit_lv_each(StgClosure *c, int top, int depth, ItemFn fn, void *env);

// Follows indirections to the constructor and gives the pointer its tag.
void *unison_jit_settle(void *p);

// --- texts and bytes (Unison.Util.Rope) ---

typedef struct {
  StgWord foreign_info, wrap_info, wrap_tag; // Foreign (WrapText rope) or (WrapBytes rope)
  StgWord one_info, deep_info, chunk_info;
  StgClosure *empty; // Rope's Empty (tagged)
  // the most elements two chunks may have between them to be made one
  // (Rope.threshold, handed to the init function)
  StgInt threshold;
  StgInt chunk_words;           // a chunk's payload words
  int ix_count, ix_off, ix_len; // the chunk's element count, byte offset and byte length; field 0 is the array
  int utf8;                     // the elements are characters in UTF-8 (else they are the bytes)
} RopeKind;
UNISON_JIT_TABLE RopeKind unison_jit_TK, unison_jit_BK; // text, bytes
#define TK unison_jit_TK
#define BK unison_jit_BK

static inline StgClosure *rope_of(const RopeKind *K, void *x) {
  if (LTAG(x) != 7 || (StgWord)LUN(x)->header.info != K->foreign_info) return NULL;
  StgClosure *w = LP(x, 0);
  if (LTAG(w) != K->wrap_tag || (StgWord)LUN(w)->header.info != K->wrap_info) return NULL;
  return LP(w, 0);
}

static inline StgInt chunk_size(const RopeKind *K, StgClosure *c) { return LW(c, K->ix_count); }
static inline StgInt chunk_len(const RopeKind *K, StgClosure *c) { return LW(c, K->ix_len); }
static inline StgInt chunk_off(const RopeKind *K, StgClosure *c) { return LW(c, K->ix_off); }

static inline StgInt rope_size(const RopeKind *K, StgClosure *r) {
  switch (LTAG(r)) {
    case 2: return chunk_size(K, LP(r, 0));
    case 3: return TSIZE(LW(r, 3));
    default: return 0;
  }
}

static inline const unsigned char *chunk_bytes(const RopeKind *K, StgClosure *c) {
  return (const unsigned char *)((StgArrBytes *)LP(c, 0))->payload + chunk_off(K, c);
}

// The chunk holding element i, 0 <= i < size, and i's offset in it.
StgClosure *unison_jit_rope_chunk_at(const RopeKind *K, StgClosure *r, StgInt i, StgInt *off);

// --- arrays and refs ---

// Each Wrap constructor's info pointer and pointer tag come from the layout
// probe (unison_jit_array_init). A Wrap holds the unlifted array (or MutVar#,
// or the ticket's value) as its one field: Data.Primitive's boxes are
// unpacked into the strict field. WrapPtr holds an Addr#, a non-pointer.
typedef struct {
  StgWord info;
  StgWord tag;
} WrapInfo;

typedef struct {
  WrapInfo marray, array, mbarray, barray, ioref, ticket, ptr;
  StgClosure *empty_val; // what Scope.array fills a new array with
  int ok;
} ArrayFacts;
UNISON_JIT_TABLE ArrayFacts unison_jit_AK;
#define AK unison_jit_AK

// The wrapper's field, or NULL if x isn't Foreign (that wrapper ...).
static inline StgWord wrapped_word(void *x, const WrapInfo *w) {
  if (!AK.ok || LTAG(x) != 7 || (StgWord)LUN(x)->header.info != LF.foreign_info) return 0;
  StgClosure *wr = LP(x, 0);
  if (LTAG(wr) != (int)w->tag || (StgWord)LUN(wr)->header.info != w->info) return 0;
  return LW(wr, 0);
}
static inline void *wrapped_of(void *x, const WrapInfo *w) { return (void *)wrapped_word(x, w); }

// --- Universal.murmurHashUntyped ---

typedef struct {
  StgWord utag_info; // the closure that marks an unboxed value's type
  StgWord enum_info, datag_info;
  int ok;
} HashFacts;
UNISON_JIT_TABLE HashFacts unison_jit_HK;
#define HK unison_jit_HK

// why a value was left to the interpreter, under UNISON_JIT_TRACE_TEST
void unison_jit_murmur_fail(const char *what, StgClosure *c);

// The hash of a stack slot's two halves, or "not handled". Defined in
// jit_helpers.c; generated code calls the IR copy.
JitNat unison_jit_murmur(int64_t u, void *b);

#endif
