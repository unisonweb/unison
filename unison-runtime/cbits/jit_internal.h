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
// platform-independent (see "The IR helpers" in design.md), so nothing in
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

// A list value is `ListC deque`, one closure whose field is the deque;
// list_info and list_tag are its info pointer and pointer tag (from
// unison_jit_list_init, like the data constructors' tags: GClosure's order).
// foreign_info is Foreign's, which the other wrappers sit under.
typedef struct {
  StgWord foreign_info, list_info, list_tag;
  StgWord deep_info, mdeep_info, scons_info, n8_info, na_info;
  StgWord val_info, data1_info, data2_info;
  StgClosure *nil, *mnil, *snil; // the constructors without fields (tagged)
  StgWord data1_tag, data2_tag;  // the pointer tags of GData1 and GData2
} ListFacts;
UNISON_JIT_TABLE ListFacts unison_jit_LF;
#define LF unison_jit_LF

// The deque inside a list value (tagged), or NULL if the closure isn't a list.
static inline StgClosure *deque_of(void *list) {
  if (LTAG(list) != (int)LF.list_tag || (StgWord)LUN(list)->header.info != LF.list_info) return NULL;
  return LP(list, 0);
}

typedef void (*ItemFn)(StgClosure *item, void *env);

// the elements of a deque level in order: `depth` is how many levels of
// nodes its items hold (0 at the top)
void unison_jit_lv_each(StgClosure *c, int top, int depth, ItemFn fn, void *env);

// Follows indirections to the constructor and gives the pointer its tag.
void *unison_jit_settle(void *p);

// --- texts and bytes (Unison.Util.Rope) ---

// A text is `TextC rope`, one closure whose field is the rope (direct);
// bytes are `Foreign (WrapBytes rope)`, two. wrap_info and wrap_tag are the
// closure holding the rope: TextC's, or WrapBytes' under foreign_info.
typedef struct {
  int direct;
  StgWord foreign_info, wrap_info, wrap_tag;
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
  if (K->direct) {
    if (LTAG(x) != (int)K->wrap_tag || (StgWord)LUN(x)->header.info != K->wrap_info) return NULL;
    return LP(x, 0);
  }
  if (LTAG(x) != 7 || (StgWord)LUN(x)->header.info != K->foreign_info) return NULL;
  StgClosure *w = LP(x, 0);
  if (LTAG(w) != (int)K->wrap_tag || (StgWord)LUN(w)->header.info != K->wrap_info) return NULL;
  return LP(w, 0);
}

// Whether the info pointer is a closure holding a Haskell value: a text, a
// list, or Foreign (the kinds Universal.compare orders together).
static inline int is_builtin_info(StgWord info) {
  return info == LF.foreign_info || info == LF.list_info || (TK.direct && info == TK.wrap_info);
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

// The data constructors ("Layouts" in design.md): DataN for 0 to 6
// fields is one closure whose pointers are the fields' boxed halves then
// the type reference, and whose words are the packed tag then the fields'
// unboxed halves; DataG (seven or more) has pointers the ByteArray#, the
// Array# and the reference, and one word, the tag. The arrays hold the
// fields in reverse (field 0 last).
#define DATA_MAX 6
typedef struct {
  StgWord utag_info; // the closure that marks an unboxed value's type
  StgWord data_info[DATA_MAX + 1], datag_info;
  int ok;
} HashFacts;
UNISON_JIT_TABLE HashFacts unison_jit_HK;
#define HK unison_jit_HK
// the field count of a data closure's info pointer: 0 to DATA_MAX, DATA_MAX + 1
// for DataG, -1 for anything else
static inline int data_arity(StgWord info) {
  if (info == HK.data_info[0]) return 0;
  if (info == HK.data_info[1]) return 1;
  if (info == HK.data_info[2]) return 2;
  if (info == HK.data_info[3]) return 3;
  if (info == HK.data_info[4]) return 4;
  if (info == HK.data_info[5]) return 5;
  if (info == HK.data_info[6]) return 6;
  return info == HK.datag_info ? DATA_MAX + 1 : -1;
}
// field j of a DataN closure with n fields
#define DATA_B(c, n, j) LP(c, j)
#define DATA_TAG(c, n) LW(c, (n) + 1)
#define DATA_U(c, n, j) LW(c, (n) + 2 + (j))

// why a value was left to the interpreter, under UNISON_JIT_TRACE_TEST
void unison_jit_murmur_fail(const char *what, StgClosure *c);

// --- Universal.== and Universal.compare ---

// What the comparison walkers need beyond LF, TK, BK, AK and HK; from
// unison_jit_cmp_init.
typedef struct {
  StgWord pap_info;         // PAp (CIx ref grp w) comb useg bseg: pointers ref, entry, the ByteArray#, the Array#; words grp, w, ...
  StgWord int_tag, nat_tag; // the packed tags of boxed Int and Nat literals, which match each other under equality
  StgWord any_type;         // the type part (above bit 16) of Any's packed tag: its fields compare with types
  int ok;
} CmpFacts;
UNISON_JIT_TABLE CmpFacts unison_jit_CK;
#define CK unison_jit_CK

// The Val closure at index i of a deque (0 <= i < its size), or NULL.
StgClosure *unison_jit_list_at(StgClosure *dq, StgInt i);

// Universal equality and ordering on two texts or two bytes (bit 0 of
// `kinds` takes texts, bit 1 bytes): 1 or 0, or -1 for anything else;
// -1, 0 or 1, or 2 for anything else.
int64_t unison_jit_foreign_eq(void *x, void *y, int64_t kinds);
int64_t unison_jit_foreign_cmp(void *x, void *y, int64_t kinds);

// why a comparison was left to the interpreter, under UNISON_JIT_TRACE_TEST
void unison_jit_cmp_fail(const char *what, StgClosure *c);

// Universal.== and Universal.compare of two stack slots: (ok, 0 or 1) and
// (ok, -1, 0 or 1), or "not handled". Defined in jit_helpers.c; generated
// code calls the IR copies.
JitNat unison_jit_ueq(int64_t u1, void *b1, int64_t u2, void *b2, int64_t kinds);
JitNat unison_jit_ucmp(int64_t u1, void *b1, int64_t u2, void *b2, int64_t kinds);

// --- the handler environment (HEnv, Unison.Runtime.Stack) ---

// HEnv is a record of two lazy pointer fields, `aenv` (ability number to
// AffineRef, an IORef of the current affine handler) and `denv` (ability
// number to the installed non-affine handler), both EnumMaps, newtypes over
// IntMap. An IntMap is Bin (two subtree pointers, then the prefix word, or
// prefix and mask before containers 0.7), Tip (the value pointer, then the
// key word) or Nil, told apart by pointer tag. An AffineRef unpacks to its MutVar# where it is a strict field
// (GAffine, -funbox-strict-fields) but is the STRef box in a Tip, whose
// value field is lazy. All of this comes from unison_jit_env_init.
typedef struct {
  StgWord henv_info;
  int henv_tag;
  int bin_tag, tip_tag, nil_tag;
  StgWord stref_info; // the box around a MutVar#: one pointer field, tag 1
  StgWord affine_info;
  int affine_tag; // GAffine ps aenv ref: the set, the map, the MutVar#
  int ok;
  void *empty_denv; // the static closure of the empty map, for an InLocal's HEnv
} EnvFacts;
UNISON_JIT_TABLE EnvFacts unison_jit_EK;
#define EK unison_jit_EK

// The affine handler for ability `key` in the environment: the closure the
// handler's ref holds, or NULL when the key is handled by a non-affine
// handler (denv shadows aenv), unhandled, or anything on the way isn't
// evaluated or isn't the expected shape; the interpreter then resolves it.
// Defined in jit_helpers.c; generated code calls the IR copy.
void *unison_jit_resolve_dyn(void *henv, int64_t key);

// --- records (GRecord, Unison.Runtime.Stack) ---
// A record is `GRecord shape vals` with the shape (MCode.RecordShape) and the
// vector of values unpacked into the closure (both modules are compiled with
// -funbox-strict-fields): pointer tag 7, told from Foreign by its info
// pointer; pointers the field names' Array# (Data.Text elements, ascending),
// the field numbers' ByteArray# (ascending), the slots' ByteArray# (the slot
// of the field number at the same index) and the values' Array# (Val
// closures, slot i the value of field i); words the interned shape
// reference, the names' offset and length in their array, the values' offset
// and length. All of it from unison_jit_record_init.
typedef struct {
  StgWord record_info;
  StgWord text_info; // Data.Text: pointer the ByteArray#, words the byte offset and length (UTF-8)
  int ok;
} RecordFacts;
UNISON_JIT_TABLE RecordFacts unison_jit_RK;
#define RK unison_jit_RK
// payload indices of a record closure
#define REC_NAMES 0
#define REC_KEYS 1
#define REC_SLOTS 2
#define REC_VALS 3
#define REC_REF 4
#define REC_NOFF 5
#define REC_NLEN 6
#define REC_VOFF 7
#define REC_VLEN 8
static inline int is_record(void *c) {
  return RK.ok && LTAG(c) == 7 && (StgWord)LUN(c)->header.info == RK.record_info;
}
// The slot of the field with this number in the record's shape (a binary
// search of the shape's field numbers), or -1 when the record has no such
// field. Defined in jit_helpers.c; generated code calls the IR copy.
int64_t unison_jit_rec_pos(void *rec, int64_t field);
// why a record was left to the interpreter, under UNISON_JIT_TRACE_TEST
void unison_jit_rec_fail(const char *what, StgClosure *c);
// The hash of a stack slot's two halves, or "not handled". Defined in
// jit_helpers.c; generated code calls the IR copy.
JitNat unison_jit_murmur(int64_t u, void *b);

#endif
