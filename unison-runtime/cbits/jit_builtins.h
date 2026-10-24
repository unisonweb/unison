// Private to the JIT's C runtime (jit_rt.c and the jit_builtins_*.c files it includes): what
// the files share. They are compiled as one translation unit, so everything here is static, and
// a function one file defines and another calls is inlined as it would be in one file. Nothing
// here is for Haskell (that is jit_rt.h) or for the IR-compiled helpers (that is jit_internal.h).
//
// Cabal recompiles jit_rt.c only when it is newer than its object and knows nothing of its
// includes, this header and the jit_builtins_*.c files among them: after changing any of them,
// `touch jit_rt.c` before building (development.md, "Building").
#ifndef UNISON_JIT_BUILTINS_H
#define UNISON_JIT_BUILTINS_H
#include "Rts.h"
#include "jit_rt.h"
#include "jit_internal.h"
#include <stdint.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <math.h>
#include <ctype.h>
#include <limits.h>

// Set from UNISON_JIT_TRACE (unison_jit_configure); the builtins log with it.
static int64_t jit_trace;

// Generated code calls the helpers below by name; the JIT is told their
// addresses from the table unison_jit_helpers (jit_rt.c), which also
// keeps them from being dropped by the linker.
void *unison_jit_alloc_words(UnisonJitCtx *ctx, int64_t n);
// (unison_jit_exit_frame, the write-back routine, is not here: it is IR,
// cbits/jit_exit.c, compiled by the JIT at startup, since it needs the
// preserve_most convention that only clang would give a C copy.)
void unison_jit_exit_fail(void);
void unison_jit_write_mutvar(UnisonJitCtx *ctx, StgMutVar *mv, StgClosure *v);
int64_t unison_jit_list_size(void *list);
void *unison_jit_list_view(UnisonJitCtx *ctx, void *list, void *empty, int64_t elem_tag, int64_t left);
void *unison_jit_list_push(UnisonJitCtx *ctx, void *list, int64_t u, void *b, int64_t front);
void *unison_jit_list_index(UnisonJitCtx *ctx, void *list, int64_t i, void *none, int64_t some_tag);
void *unison_jit_list_lit(UnisonJitCtx *ctx, void *acc, int64_t u, void *b);
void *unison_jit_list_wrap(UnisonJitCtx *ctx, void *acc);
void *unison_jit_list_cut(UnisonJitCtx *ctx, void *list, int64_t n, int64_t take);
void *unison_jit_list_split(UnisonJitCtx *ctx, void *list, int64_t n, void *empty, int64_t elem_tag, int64_t left);
void *unison_jit_list_append(UnisonJitCtx *ctx, void *x, void *y);
int64_t unison_jit_text_size(void *text);
void *unison_jit_text_append(UnisonJitCtx *ctx, void *x, void *y);
void *unison_jit_text_cut(UnisonJitCtx *ctx, void *text, int64_t n, int64_t take);
int64_t unison_jit_text_eq(void *x, void *y);
int64_t unison_jit_bytes_size(void *bytes);
void *unison_jit_bytes_append(UnisonJitCtx *ctx, void *x, void *y);
void *unison_jit_bytes_cut(UnisonJitCtx *ctx, void *bytes, int64_t n, int64_t take);
int64_t unison_jit_foreign_eq(void *x, void *y, int64_t kinds);
void *unison_jit_bytes_index(UnisonJitCtx *ctx, void *bytes, int64_t i, void *none, int64_t some_tag, void *nat_tag);
void *unison_jit_bytes_flatten(UnisonJitCtx *ctx, void *bytes);
void *unison_jit_text_uncons(UnisonJitCtx *ctx, void *text, void *none, int64_t some_tag, void *pair, int64_t pair_tag,
                             void *unit, void *char_tag, int64_t front);
void *unison_jit_int_to_text(UnisonJitCtx *ctx, int64_t n, int64_t is_signed);
void *unison_jit_float_to_text(UnisonJitCtx *ctx, int64_t bits);
void *unison_jit_text_to_num(UnisonJitCtx *ctx, void *text, void *none, int64_t some_tag, void *tag, int64_t kind);
void *unison_jit_text_pack(UnisonJitCtx *ctx, void *list, void *char_tag);
void *unison_jit_bytes_pack(UnisonJitCtx *ctx, void *list, void *nat_tag);
void *unison_jit_text_unpack(UnisonJitCtx *ctx, void *text, void *char_tag);
void *unison_jit_bytes_unpack(UnisonJitCtx *ctx, void *bytes, void *nat_tag);
void *unison_jit_text_index_of(UnisonJitCtx *ctx, void *needle, void *hay, void *none, int64_t some_tag, void *nat_tag);
void *unison_jit_bytes_index_of(UnisonJitCtx *ctx, void *needle, void *hay, void *none, int64_t some_tag, void *nat_tag);
int64_t unison_jit_text_cmp(void *x, void *y);
int64_t unison_jit_foreign_cmp(void *x, void *y, int64_t kinds);
void *unison_jit_char_to_text(UnisonJitCtx *ctx, int64_t c);
void *unison_jit_text_repeat(UnisonJitCtx *ctx, int64_t n, void *text);
void *unison_jit_text_reverse(UnisonJitCtx *ctx, void *text);
void *unison_jit_text_case(UnisonJitCtx *ctx, void *text, int64_t upper);
void *unison_jit_text_to_utf8(UnisonJitCtx *ctx, void *text);
void *unison_jit_text_from_utf8(UnisonJitCtx *ctx, void *bytes, void *either, int64_t right_tag);
void *unison_jit_bytes_decode_nat(UnisonJitCtx *ctx, void *bytes, int64_t width, int64_t be, void *none, int64_t some_tag,
                                  void *pair, int64_t pair_tag, void *unit, void *nat_tag);
void *unison_jit_bytes_encode_nat(UnisonJitCtx *ctx, int64_t n, int64_t width, int64_t be);
int64_t unison_jit_bytes_read_ok(void *bytes, int64_t i, int64_t width);
int64_t unison_jit_bytes_read_at(void *bytes, int64_t i, int64_t width, int64_t be);
void *unison_jit_bytes_to_base(UnisonJitCtx *ctx, void *bytes, int64_t base);
void *unison_jit_bytes_from_base(UnisonJitCtx *ctx, void *bytes, int64_t base, void *either, int64_t right_tag);
int64_t unison_jit_pow(int64_t m, int64_t n);
double unison_jit_atan2(double y, double x);
int64_t unison_jit_barray_copy(void *dst, int64_t doff, void *src, int64_t soff, int64_t l, int64_t kind);
void *unison_jit_barray_freeze(UnisonJitCtx *ctx, void *x, int64_t off, int64_t len, int64_t in_place);
void *unison_jit_barray_to_bytes(UnisonJitCtx *ctx, void *x, int64_t off, int64_t len);
void *unison_jit_barray_from_bytes(UnisonJitCtx *ctx, void *bytes);
void *unison_jit_barray_new(UnisonJitCtx *ctx, int64_t n, int64_t fill, int64_t pinned);
void *unison_jit_barray_contents(UnisonJitCtx *ctx, void *x);
void *unison_jit_wrap_ptr(UnisonJitCtx *ctx, int64_t addr);
void *unison_jit_parray_new(UnisonJitCtx *ctx, int64_t n, int64_t has_init, int64_t u, void *b);
int64_t unison_jit_parray_copy(void *dst, int64_t doff, void *src, int64_t soff, int64_t l, int64_t kind);
void *unison_jit_parray_freeze(UnisonJitCtx *ctx, void *x, int64_t off, int64_t len, int64_t in_place);
void *unison_jit_ref_new(UnisonJitCtx *ctx, int64_t u, void *b);
void *unison_jit_ref_read_for_cas(UnisonJitCtx *ctx, void *ref);
int64_t unison_jit_ref_cas(UnisonJitCtx *ctx, void *ref, void *ticket, int64_t u, void *b);
JitNat unison_jit_murmur(int64_t u, void *b);
void *unison_jit_name(UnisonJitCtx *ctx, void *fun, int64_t n, int64_t base);

int64_t unison_jit_set_aff(UnisonJitCtx *ctx, void *aff, void *v);
void *unison_jit_local_env(UnisonJitCtx *ctx, void *aff);
void *unison_jit_rec_set(UnisonJitCtx *ctx, void *rec, int64_t field, int64_t u, void *b);


// --- shared between the files, out of line ---
static const unsigned char *mem_find(const unsigned char *hay, StgInt hlen, const unsigned char *needle, StgInt nlen);

static void unison_jit_mark_bstk(void **elems, int64_t lo, int64_t hi);

static StgInt check_slist(StgClosure *l);

static int con_is(StgClosure *c, StgWord tag, StgWord ptrs, StgWord nptrs);

static int con_nullary(StgClosure *c);

static int is_small_array(StgClosure *a);

static StgClosure *lv_append(UnisonJitCtx *ctx, int top, StgClosure *a, StgInt sns, StgClosure **ns, StgInt nn,
                             StgClosure *b);

static StgClosure *lv_cons(UnisonJitCtx *ctx, int top, StgClosure *x, StgClosure *c);

static StgClosure *lv_snoc(UnisonJitCtx *ctx, int top, StgClosure *c, StgClosure *x);

static StgClosure *lv_uncons(UnisonJitCtx *ctx, int top, StgClosure *c, StgClosure **out);

static StgClosure *lv_unsnoc(UnisonJitCtx *ctx, int top, StgClosure *c, StgClosure **out);

static StgClosure *mid_drop(UnisonJitCtx *ctx, StgClosure *c, StgInt j, StgClosure **node, StgInt *k);

static StgClosure *mid_look(StgClosure *c, int sh, StgInt i, StgInt *off);

static StgClosure *mid_take(UnisonJitCtx *ctx, StgClosure *c, StgInt j, StgClosure **node, StgInt *k);

static StgClosure *s_append(UnisonJitCtx *ctx, StgClosure *a, StgClosure *b);

static StgClosure *s_from(UnisonJitCtx *ctx, StgClosure **k, StgInt n, StgClosure *rest);

static StgClosure *s_from_rev(UnisonJitCtx *ctx, StgClosure **k, StgInt n, StgClosure *rest);

static StgClosure *s_rev_onto(UnisonJitCtx *ctx, StgClosure *l, StgClosure *acc);

static StgClosure *s_take(UnisonJitCtx *ctx, StgInt k, StgClosure *l);

static StgClosure *rope_append(UnisonJitCtx *ctx, const RopeKind *K, StgClosure *a, StgClosure *b);

static StgClosure *rope_cons(UnisonJitCtx *ctx, const RopeKind *K, StgInt s, StgClosure *c, StgClosure *r);

static StgClosure *rope_drop(UnisonJitCtx *ctx, const RopeKind *K, StgInt i, StgClosure *r);

static StgClosure *rope_snoc(UnisonJitCtx *ctx, const RopeKind *K, StgClosure *r, StgInt s, StgClosure *c);

static StgClosure *rope_take(UnisonJitCtx *ctx, const RopeKind *K, StgInt i, StgClosure *r);

static void *wrap_rope(UnisonJitCtx *ctx, const RopeKind *K, StgClosure *rope, void *same);

static const unsigned char *rope_flat(const RopeKind *K, StgClosure *r, StgInt *len, unsigned char **heap);

static StgMutArrPtrs *parray_new(UnisonJitCtx *ctx, StgWord n, StgClosure *init, const StgInfoTable *info);

static void *bytes_copy(UnisonJitCtx *ctx, const unsigned char *p, StgInt n);


// --- the list structure's constants (see jit_builtins_lists.c), used by the ropes too ---
// the most items in a digit (Deque.maxD)
#define MAXD 10
#define TPC(t) ((t)&15)
#define TSC(t) (((t) >> 4) & 15)
#define MK(n, p, s) (((StgInt)(n) << 8) | ((StgInt)(s) << 4) | (StgInt)(p))
#define HEAD(l) LP(l, 0)
#define TAIL(l) LP(l, 1)
#define IS_CONS(l) (LTAG(l) == 2)

// one level, read out of a Deep or an MDeep
typedef struct {
  StgClosure *pr, *m, *sf;
  StgInt t, ps;
} Lv;

// --- shared between the files, inline (allocation and the small constructors) ---
static inline StgRegTable *reg_table(void *cap) { return (StgRegTable *)((char *)cap + sizeof(StgFunTable)); }

// the block's free pointer catches up with hp
static inline void alloc_sync(UnisonJitCtx *ctx) {
  if (ctx->hp != NULL) reg_table(ctx->cap)->rCurrentAlloc->free = (StgPtr)ctx->hp;
}

// hp is taken from the current allocation block, budget_end keeps `left`
// words ahead of it, and hp_lim is the nearer of the block's end and
// budget_end. With no block, hp is NULL and hp_lim is too, so that the
// first allocation takes the slow path.
static inline void alloc_refresh(UnisonJitCtx *ctx, int64_t left) {
  bdescr *bd = reg_table(ctx->cap)->rCurrentAlloc;
  uintptr_t hp = 0, end = 0;
  if (bd != NULL) {
    hp = (uintptr_t)bd->free;
    end = (uintptr_t)(bd->start + BLOCK_SIZE_W);
  }
  uintptr_t bend = hp + (uintptr_t)(left * (int64_t)sizeof(StgWord));
  ctx->hp = (void *)hp;
  ctx->budget_end = (void *)bend;
  // as signed offsets from hp, so that an exhausted budget (bend below hp)
  // wins over the block's end
  ctx->hp_lim = (void *)((intptr_t)(bend - hp) < (intptr_t)(end - hp) ? bend : end);
}

// the words the budget still allows (negative once it is exhausted)
static inline int64_t budget_left(const UnisonJitCtx *ctx) {
  return ((int64_t)(intptr_t)ctx->budget_end - (int64_t)(intptr_t)ctx->hp) / (int64_t)sizeof(StgWord);
}

// The fast path, for the C helpers; generated code emits the same sequence
// (allocWords in Codegen.hs). Objects of the large-object size go to
// allocate(), which puts them in their own block as it would for Haskell.
static inline StgWord *nat_alloc(UnisonJitCtx *ctx, int64_t n) {
  uintptr_t hp = (uintptr_t)ctx->hp, nhp = hp + (uintptr_t)n * sizeof(StgWord);
  if (__builtin_expect(nhp <= (uintptr_t)ctx->hp_lim && n < (int64_t)(LARGE_OBJECT_THRESHOLD / sizeof(W_)), 1)) {
    ctx->hp = (void *)nhp;
    return (StgWord *)hp;
  }
  return (StgWord *)unison_jit_alloc_words(ctx, n);
}

// Everything allocated is written in full before the helper returns: the
// heap must have no gaps.
static inline StgWord *list_alloc(UnisonJitCtx *ctx, int64_t words) { return nat_alloc(ctx, words); }

// nat_alloc for an object whose size the program chose: NULL instead of
// exiting the process when no heap can hold it (see jit_rt.c). The fast
// path is the same; only a size past the block or the large-object
// threshold reaches the slow path, where the difference is.
void *unison_jit_alloc_words_mayfail(UnisonJitCtx *ctx, int64_t n);
static inline StgWord *nat_alloc_mayfail(UnisonJitCtx *ctx, int64_t n) {
  uintptr_t hp = (uintptr_t)ctx->hp, nhp = hp + (uintptr_t)n * sizeof(StgWord);
  if (__builtin_expect(n >= 0 && nhp <= (uintptr_t)ctx->hp_lim && n < (int64_t)(LARGE_OBJECT_THRESHOLD / sizeof(W_)), 1)) {
    ctx->hp = (void *)nhp;
    return (StgWord *)hp;
  }
  return (StgWord *)unison_jit_alloc_words_mayfail(ctx, n);
}

static inline StgInt node_arity(StgClosure *nd) {
  return LTAG(nd) == 1 ? 8 : (StgInt)((StgSmallMutArrPtrs *)LP(nd, 0))->ptrs;
}

static inline StgClosure **node_kids(StgClosure *nd) {
  return LTAG(nd) == 1 ? LUN(nd)->payload : ((StgSmallMutArrPtrs *)LP(nd, 0))->payload;
}

static inline StgInt node_size(StgClosure *nd) { return LTAG(nd) == 1 ? 8 : LW(nd, 1); }

static inline StgInt mid_size(StgClosure *m) { return LTAG(m) == 2 ? TSIZE(LW(m, 3)) : 0; }

static inline int lv_read(StgClosure *c, int top, Lv *v) {
  if (LTAG(c) != 2) return 0;
  v->pr = LP(c, 0), v->m = LP(c, 1), v->sf = LP(c, 2);
  v->t = LW(c, 3);
  v->ps = top ? 0 : LW(c, 4);
  return 1;
}

static inline StgClosure *s_drop(StgInt k, StgClosure *l) {
  for (; k > 0 && IS_CONS(l); k--) l = TAIL(l);
  return l;
}

// a list's items into an array, in list order; their number
static inline StgInt s_items(StgClosure *l, StgClosure **out) {
  StgInt n = 0;
  for (; IS_CONS(l); l = TAIL(l)) out[n++] = HEAD(l);
  return n;
}

// Whether c is the same constructor without fields as ref. Not a comparison
// of addresses: a constructor without fields has a static closure of its own,
// but so does every top-level binding that is just that constructor (empty =
// Nil), and which of them a piece of code refers to is up to the compiler.
static inline int same_con0(StgClosure *c, StgClosure *ref) {
  return LTAG(c) == LTAG(ref) && LUN(c)->header.info == LUN(ref)->header.info;
}

static inline StgClosure *scons_at(StgWord *p, StgClosure *x, StgClosure *rest) {
  p[0] = LF.scons_info;
  p[1] = (StgWord)x;
  p[2] = (StgWord)rest;
  return (StgClosure *)((StgWord)p | 2);
}

static inline StgClosure *scons(UnisonJitCtx *ctx, StgClosure *x, StgClosure *rest) {
  return scons_at(list_alloc(ctx, 3), x, rest);
}

// ListC dq, in two words at p.
static inline void *wrap_at(StgWord *p, StgClosure *dq) {
  p[0] = LF.list_info;
  p[1] = (StgWord)dq;
  return (void *)((StgWord)p | LF.list_tag);
}

static inline void *wrap_list(UnisonJitCtx *ctx, StgClosure *dq) { return wrap_at(list_alloc(ctx, 2), dq); }

// NA size (the n items at k, in an array)
static inline StgClosure *mk_na(UnisonJitCtx *ctx, StgInt size, StgClosure **k, StgInt n) {
  StgWord *p = list_alloc(ctx, 2 + n + 3);
  p[0] = (StgWord)&stg_SMALL_MUT_ARR_PTRS_FROZEN_CLEAN_info;
  p[1] = (StgWord)n;
  for (StgInt i = 0; i < n; i++) p[2 + i] = (StgWord)k[i];
  StgWord *q = p + 2 + n;
  q[0] = LF.na_info;
  q[1] = (StgWord)p;
  q[2] = (StgWord)size;
  return (StgClosure *)((StgWord)q | 2);
}

// all but the first `from` children of a node, front to back (nodeDropFwd)
static inline StgClosure *kids_fwd(UnisonJitCtx *ctx, StgClosure *nd, StgInt from) {
  return s_from(ctx, node_kids(nd) + from, node_arity(nd) - from, LF.snil);
}

// the first k children of a node, back to front (nodeTakeRev)
static inline StgClosure *kids_rev(UnisonJitCtx *ctx, StgClosure *nd, StgInt k) {
  return s_from_rev(ctx, node_kids(nd), k, LF.snil);
}

// a fresh byte array of n bytes
static inline StgArrBytes *new_arr(UnisonJitCtx *ctx, StgWord n) {
  StgArrBytes *arr = (StgArrBytes *)list_alloc(ctx, sizeofW(StgArrBytes) + ROUNDUP_BYTES_TO_WDS(n));
  SET_INFO((StgClosure *)arr, &stg_ARR_WORDS_info);
  arr->bytes = n;
  return arr;
}

// new_arr for a size the program chose: NULL when it can't be had (the
// caller returns "not handled"; n is at most 2^63 - 1, so the word count
// can't wrap)
static inline StgArrBytes *new_arr_mayfail(UnisonJitCtx *ctx, StgWord n) {
  StgArrBytes *arr = (StgArrBytes *)nat_alloc_mayfail(ctx, (int64_t)(sizeofW(StgArrBytes) + ROUNDUP_BYTES_TO_WDS(n)));
  if (arr == NULL) return NULL;
  SET_INFO((StgClosure *)arr, &stg_ARR_WORDS_info);
  arr->bytes = n;
  return arr;
}

static inline StgClosure *new_chunk_cap(UnisonJitCtx *ctx, const RopeKind *K, void *arr, StgInt count, StgInt off,
                                        StgInt len, StgInt cap) {
  StgWord *p = list_alloc(ctx, 1 + K->chunk_words);
  p[0] = K->chunk_info;
  p[1] = (StgWord)arr;
  p[1 + K->ix_len] = len;
  p[1 + K->ix_count] = count; // the length's own field for bytes, with the same value
  p[1 + K->ix_off] = off;
  p[1 + K->ix_cap] = cap;
  return (StgClosure *)((StgWord)p | 1);
}

// a chunk with no slack (a slice, or an exact array)
static inline StgClosure *new_chunk(UnisonJitCtx *ctx, const RopeKind *K, void *arr, StgInt count, StgInt off,
                                    StgInt len) {
  return new_chunk_cap(ctx, K, arr, count, off, len, 0);
}

static inline StgClosure *rope_one(UnisonJitCtx *ctx, const RopeKind *K, StgClosure *chunk) {
  StgWord *p = list_alloc(ctx, 2);
  p[0] = K->one_info;
  p[1] = (StgWord)chunk;
  return (StgClosure *)((StgWord)p | 2);
}

// a Val closure (a list element)
static inline StgClosure *mk_val(UnisonJitCtx *ctx, void *b, StgInt u) {
  StgWord *p = list_alloc(ctx, 3);
  p[0] = LF.val_info;
  p[1] = (StgWord)b;
  p[2] = u;
  return (StgClosure *)((StgWord)p | 1);
}

static inline void *mk_data1(UnisonJitCtx *ctx, StgClosure *ref, StgInt tag, void *b, StgInt u) {
  StgWord *p = list_alloc(ctx, 5);
  p[0] = LF.data1_info;
  p[1] = (StgWord)b;
  p[2] = (StgWord)ref;
  p[3] = tag;
  p[4] = u;
  return (void *)((StgWord)p | LF.data1_tag);
}

static inline void *mk_data2(UnisonJitCtx *ctx, StgClosure *ref, StgInt tag, void *b1, StgInt u1, void *b2, StgInt u2) {
  StgWord *p = list_alloc(ctx, 7);
  p[0] = LF.data2_info;
  p[1] = (StgWord)b1;
  p[2] = (StgWord)b2;
  p[3] = (StgWord)ref;
  p[4] = tag;
  p[5] = u1;
  p[6] = u2;
  return (void *)((StgWord)p | LF.data2_tag);
}

// the pair (a, b): Tuple a (Tuple b ()); `pair` is the pooled Tuple
// enumeration (its first field the reference) and `unit` the pooled ()
static inline void *mk_pair(UnisonJitCtx *ctx, void *pair, StgInt pair_tag, void *unit, void *b1, StgInt u1, void *b2,
                            StgInt u2) {
  StgClosure *ref = LP(pair, 0);
  void *inner = mk_data2(ctx, ref, pair_tag, b2, u2, unit, -1);
  return mk_data2(ctx, ref, pair_tag, b1, u1, inner, -1);
}

// Some v; `none` is the pooled None, whose first field is Optional's reference
static inline void *mk_some(UnisonJitCtx *ctx, void *none, StgInt some_tag, void *b, StgInt u) {
  return mk_data1(ctx, LP(none, 0), some_tag, b, u);
}

#endif
