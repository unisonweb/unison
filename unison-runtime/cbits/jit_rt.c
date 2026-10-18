// Runtime helpers for JIT-compiled code, and the pieces of the GHC runtime it needs.
// See unison-runtime/src/Unison/Runtime/JIT/design.md.
//
// The build doesn't know this file depends on jit_rt.h and jit_internal.h (Cabal tracks C
// sources by content, not their includes): after changing a header, change this file too,
// or the stale object stays in the binary (see development.md).

// glibc: pthread_getattr_np (thread_stack_bounds) is a GNU extension
#ifndef _GNU_SOURCE
#define _GNU_SOURCE
#endif
#include "Rts.h"
#include "jit_rt.h"
#define UNISON_JIT_TABLES_HERE
#include "jit_internal.h"

#if defined(_WIN32)
#define WIN32_LEAN_AND_MEAN
#define NOMINMAX
#include <windows.h>
#else
#include <pthread.h>
#endif
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <ctype.h>
#include <limits.h>
#include <math.h>
#include <stdint.h>

// Field offsets, so the Haskell side can check that its picture of Ctx matches this one.
// The order matches the fields of UnisonJitCtx.
int64_t unison_jit_ctx_layout(int64_t *out, int64_t n) {
  int64_t offs[] = {
      offsetof(UnisonJitCtx, ustk),        offsetof(UnisonJitCtx, bstk),
      offsetof(UnisonJitCtx, pool),        offsetof(UnisonJitCtx, stack_size),
      offsetof(UnisonJitCtx, hplim),       offsetof(UnisonJitCtx, ap),
      offsetof(UnisonJitCtx, fp),          offsetof(UnisonJitCtx, sp),
      offsetof(UnisonJitCtx, max_sp),      offsetof(UnisonJitCtx, stress_poll),
      offsetof(UnisonJitCtx, stress_poll_left), offsetof(UnisonJitCtx, stress_callee),
      offsetof(UnisonJitCtx, stress_callee_left), offsetof(UnisonJitCtx, frames),
      offsetof(UnisonJitCtx, n_frames),    offsetof(UnisonJitCtx, max_frames),
      offsetof(UnisonJitCtx, cstack_limit), offsetof(UnisonJitCtx, cap),
      offsetof(UnisonJitCtx, budget_end),  offsetof(UnisonJitCtx, hp),
      offsetof(UnisonJitCtx, hp_lim),      offsetof(UnisonJitCtx, henv),
      offsetof(UnisonJitCtx, n_locals),
  };
  int64_t count = sizeof offs / sizeof offs[0];
  for (int64_t i = 0; i < n && i < count; i++) out[i] = offs[i];
  return sizeof(UnisonJitCtx);
}

// For UNISON_JIT_TRACE_YIELD: a constructor's pointer fields, with tags and closure types, two levels down.
static void trace_fields(StgClosure *c, int depth) {
  if (c == NULL || GET_CLOSURE_TAG(c) == 0) {
    if (c != NULL) {
      const StgInfoTable *it = get_itbl(c);
      fprintf(stderr, "%*s untagged %p type %d%s\n", depth * 2, "", (void *)c, (int)it->type,
              it->type == IND_STATIC ? " (IND_STATIC)" : it->type == THUNK_STATIC ? " (THUNK_STATIC)" : "");
    }
    return;
  }
  StgClosure *u = UNTAG_CLOSURE(c);
  const StgInfoTable *it = get_itbl(u);
  if (!(it->type >= CONSTR && it->type <= CONSTR_NOCAF) || depth > 2) return;
  for (StgWord i = 0; i < it->layout.payload.ptrs; i++) {
    StgClosure *f = u->payload[i];
    const StgInfoTable *fi = (f != NULL) ? get_itbl(UNTAG_CLOSURE(f)) : NULL;
    fprintf(stderr, "%*s field %lu: %p tag %d type %d\n", depth * 2, "", (unsigned long)i, (void *)f,
            (int)GET_CLOSURE_TAG(f), fi ? (int)fi->type : -1);
    trace_fields(f, depth + 1);
  }
}

// Marks a boxed array as changed in slots lo..hi, as writeArray# would have.
static void unison_jit_mark_bstk(void **elems, int64_t lo, int64_t hi) {
  StgMutArrPtrs *arr = (StgMutArrPtrs *)((StgWord *)elems - sizeofW(StgMutArrPtrs));
  SET_INFO((StgClosure *)arr, &stg_MUT_ARR_PTRS_DIRTY_info);
  for (int64_t card = lo >> MUT_ARR_PTRS_CARD_BITS; card <= hi >> MUT_ARR_PTRS_CARD_BITS; card++)
    *mutArrPtrsCard(arr, card) = 1;
}

// The address of HpLim for a capability. Valid for as long as the thread stays on the
// capability, which it does throughout an unsafe foreign call; the trampoline refreshes it
// on every entry. A Capability starts with the function table and then the register table,
// both of which are public types.
static inline void **hplim_of(Capability *cap) {
  StgRegTable *reg = (StgRegTable *)((char *)cap + sizeof(StgFunTable));
  return (void **)&reg->rHpLim;
}

void **unison_jit_hplim_address(void) { return hplim_of(rts_unsafeGetMyCapability()); }

// ---------------------------------------------------------------------------
// Entering native code

// Settings, copied into each context when it is created.
static int64_t stress_poll = 0;
static int64_t stress_callee = 0;
static int64_t trace = 0;
// How much C stack native code may use for non-tail calls, at most. The
// stress mode cstack=N makes this small.
static int64_t cstack_budget = 1 << 20;
// Words native code may allocate between two polls. The stress mode
// alloc=N makes this small.
static int64_t alloc_budget = 256 * 1024;
// Whether native code bumps a pointer inline for its allocations (the default)
// or calls allocate() for each. UNISON_JIT_BUMP=0 turns it off, for comparison.
static int64_t bump_alloc = 1;
// C stack kept free for the runtime's own C code (the GC in particular),
// below which native code never goes whatever the budget says.
#define CSTACK_RESERVE (256 * 1024)
// Every native non-tail call in progress uses at least 16 bytes of C stack,
// and a frame writes one record per inline Let it is inside of plus its
// own, so the budget bounds the number of frame records. Frames are far
// bigger than this in practice; the pages of the buffer that are never
// touched cost nothing.
#define MIN_NATIVE_FRAME 4

// Generated code calls the helpers below by name; the JIT is told their
// addresses from the table unison_jit_helpers (further down), which also
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
void *unison_jit_name(UnisonJitCtx *ctx, void *fun, int64_t n, int64_t u0, void *b0, int64_t u1, void *b1, int64_t u2,
                      void *b2, int64_t u3, void *b3);
// The tables the IR helpers (jit_helpers.ll) refer to by name: the JIT
// defines each name at its address before adding the module. Fills up to n
// entries and returns how many there are.
int64_t unison_jit_table_addresses(const char **names, void **addrs, int64_t n) {
  static const char *const tnames[] = {"unison_jit_LF", "unison_jit_TK", "unison_jit_BK", "unison_jit_AK", "unison_jit_HK", "unison_jit_EK", "unison_jit_CK", "unison_jit_RK"};
  void *taddrs[] = {&unison_jit_LF, &unison_jit_TK, &unison_jit_BK, &unison_jit_AK, &unison_jit_HK, &unison_jit_EK, &unison_jit_CK, &unison_jit_RK};
  int64_t count = sizeof taddrs / sizeof taddrs[0];
  for (int64_t i = 0; i < n && i < count; i++) names[i] = tnames[i], addrs[i] = taddrs[i];
  return count;
}

int64_t unison_jit_set_aff(UnisonJitCtx *ctx, void *aff, void *v);
void *unison_jit_local_env(UnisonJitCtx *ctx, void *aff);
void *unison_jit_rec_set(UnisonJitCtx *ctx, void *rec, int64_t field, int64_t u, void *b);

// Every C function generated code or the IR modules call by name, with its
// address. The JIT is told these before any module is added
// (unison_jit_helper_addresses, defined as absolute symbols in JIT.hs), so
// that no lookup in the process's own symbol table is needed: on Linux an
// executable's symbols aren't visible to dlsym without -rdynamic, and on
// Windows not at all. The table also keeps the functions alive: the linker
// drops a function nothing refers to (GHC links with -dead_strip).
typedef struct {
  const char *name;
  void *addr;
} JitHelper;
#define HELPER(f) {#f, (void *)&f}
static const JitHelper unison_jit_helpers[] = {
    HELPER(unison_jit_set_aff),
    HELPER(unison_jit_resolve_dyn),
    HELPER(unison_jit_local_env),
    HELPER(unison_jit_alloc_words),
    HELPER(unison_jit_write_mutvar),
    HELPER(unison_jit_list_size),
    HELPER(unison_jit_list_view),
    HELPER(unison_jit_list_push),
    HELPER(unison_jit_list_index),
    HELPER(unison_jit_text_size),
    HELPER(unison_jit_text_append),
    HELPER(unison_jit_text_cut),
    HELPER(unison_jit_text_eq),
    HELPER(unison_jit_name),
    HELPER(unison_jit_list_lit),
    HELPER(unison_jit_list_wrap),
    HELPER(unison_jit_list_cut),
    HELPER(unison_jit_list_split),
    HELPER(unison_jit_list_append),
    HELPER(unison_jit_bytes_size),
    HELPER(unison_jit_bytes_append),
    HELPER(unison_jit_bytes_cut),
    HELPER(unison_jit_foreign_eq),
    HELPER(unison_jit_bytes_index),
    HELPER(unison_jit_bytes_flatten),
    HELPER(unison_jit_text_uncons),
    HELPER(unison_jit_int_to_text),
    HELPER(unison_jit_float_to_text),
    HELPER(unison_jit_text_to_num),
    HELPER(unison_jit_text_pack),
    HELPER(unison_jit_bytes_pack),
    HELPER(unison_jit_text_unpack),
    HELPER(unison_jit_bytes_unpack),
    HELPER(unison_jit_text_index_of),
    HELPER(unison_jit_bytes_index_of),
    HELPER(unison_jit_text_cmp),
    HELPER(unison_jit_foreign_cmp),
    HELPER(unison_jit_char_to_text),
    HELPER(unison_jit_text_repeat),
    HELPER(unison_jit_text_reverse),
    HELPER(unison_jit_text_case),
    HELPER(unison_jit_text_to_utf8),
    HELPER(unison_jit_text_from_utf8),
    HELPER(unison_jit_bytes_decode_nat),
    HELPER(unison_jit_bytes_encode_nat),
    HELPER(unison_jit_bytes_read_ok),
    HELPER(unison_jit_bytes_read_at),
    HELPER(unison_jit_bytes_to_base),
    HELPER(unison_jit_bytes_from_base),
    HELPER(unison_jit_pow),
    HELPER(unison_jit_atan2),
    HELPER(unison_jit_barray_copy),
    HELPER(unison_jit_barray_freeze),
    HELPER(unison_jit_barray_to_bytes),
    HELPER(unison_jit_barray_from_bytes),
    HELPER(unison_jit_barray_new),
    HELPER(unison_jit_barray_contents),
    HELPER(unison_jit_wrap_ptr),
    HELPER(unison_jit_parray_new),
    HELPER(unison_jit_parray_copy),
    HELPER(unison_jit_parray_freeze),
    HELPER(unison_jit_ref_new),
    HELPER(unison_jit_ref_read_for_cas),
    HELPER(unison_jit_ref_cas),
    HELPER(unison_jit_murmur),
    HELPER(unison_jit_exit_fail),
    HELPER(unison_jit_ueq),
    HELPER(unison_jit_ucmp),
    HELPER(unison_jit_rec_pos),
    HELPER(unison_jit_rec_set),
    HELPER(unison_jit_settle),
    HELPER(unison_jit_rope_chunk_at),
    HELPER(unison_jit_lv_each),
    HELPER(unison_jit_murmur_fail),
    HELPER(unison_jit_table_addresses),
    HELPER(unison_jit_list_at),
    HELPER(unison_jit_cmp_fail),
    HELPER(unison_jit_rec_fail),
};
#undef HELPER

int64_t unison_jit_helper_addresses(const char **names, void **addrs, int64_t n) {
  int64_t count = sizeof unison_jit_helpers / sizeof unison_jit_helpers[0];
  for (int64_t i = 0; i < n && i < count; i++) names[i] = unison_jit_helpers[i].name, addrs[i] = unison_jit_helpers[i].addr;
  return count;
}

void unison_jit_configure(int64_t poll_every, int64_t callee_every, int64_t cstack, int64_t alloc,
                          int64_t trace_on) {
  for (size_t i = 0; i < sizeof unison_jit_helpers / sizeof unison_jit_helpers[0]; i++)
    if (unison_jit_helpers[i].addr == NULL) abort();
  stress_poll = poll_every;
  stress_callee = callee_every;
  if (cstack > 0) cstack_budget = cstack;
  if (alloc > 0) alloc_budget = alloc;
  trace = trace_on;
  const char *bump = getenv("UNISON_JIT_BUMP");
  bump_alloc = !(bump != NULL && strcmp(bump, "0") == 0);
}

// ---------------------------------------------------------------------------
// Numbers

// Haskell's ^ for Int and Nat with a Nat exponent: the product modulo 2^64,
// which doesn't depend on the order of the multiplications, so squaring
// gives the same bits as the Prelude's. (The Prelude rejects a negative
// exponent; a Nat can't be one.)
int64_t unison_jit_pow(int64_t m, int64_t n) {
  uint64_t b = (uint64_t)m, e = (uint64_t)n, r = 1;
  while (e != 0) {
    if (e & 1) r *= b;
    b *= b;
    e >>= 1;
  }
  return (int64_t)r;
}

// GHC.Float's atan2 for Double, which is defined by cases in Haskell rather
// than by libm's atan2, so that the edge cases (signed zeros, NaN) and the
// rounding of pi + atan (y/x) come out the same. A port of the instance.
double unison_jit_atan2(double y, double x) {
  if (x > 0) return atan(y / x);
  if (x == 0 && y > 0) return M_PI / 2;
  if (x < 0 && y > 0) return M_PI + atan(y / x);
  if ((x <= 0 && y < 0) || (x < 0 && signbit(y) && y == 0) || (signbit(x) && x == 0 && signbit(y) && y == 0))
    return -unison_jit_atan2(-y, x);
  if (y == 0 && (x < 0 || (signbit(x) && x == 0))) return M_PI;
  if (x == 0 && y == 0) return y;
  return x + y; // one is a NaN
}

// ---------------------------------------------------------------------------
// Allocation

// Native code allocates the way allocate() does, but inline: the capability's
// current allocation block (rCurrentAlloc in the register table) has a free
// pointer and an end, and an object that fits is taken by bumping the free
// pointer. Compiled Haskell does the same with Hp and HpLim, but Hp lives in
// a callee-saved machine register during an unsafe foreign call, where it
// can't be seen from C, which is what rCurrentAlloc is for. The context holds
// its own copy of the block's free pointer (hp) and end (hp_lim), so that the
// fast path is three loads, a compare and a store with no call; the block's
// own free pointer is stale while native code runs, and is written back
// (alloc_sync) before anything else in the runtime looks at the block: when
// the slow path calls allocate(), and when the trampoline returns to Haskell.
// Nothing else allocates on this capability in between, since native code
// runs inside an unsafe foreign call and never calls into Haskell.
//
// The allocation budget is folded into the same compare. budget_end is the
// address hp would reach by allocating everything the budget still allows,
// and hp_lim is the nearer of the block's end and budget_end; so the fast
// path never touches a counter, and the slow path finds out which of the two
// limits was hit. When it was the budget, the object is still handed out (an
// allocation can't exit halfway through a Pack), hp_lim is left below hp so
// that the remaining allocations of the run take the slow path too, and the
// entry poll, which tests hp against budget_end, fires at the next function
// entry or loop head. When the slow path changes block, budget_end moves
// with hp, keeping the words that remain.
//
// allocate() never returns NULL (it aborts on heap overflow like Haskell
// code would); the budget bounds how much can be allocated before the
// interpreter gets a chance to run a GC. The words allocated inline are
// charged to the thread's allocation counter when the trampoline returns, as
// allocate() would have charged them (accountAllocation).

static inline StgRegTable *reg_table(void *cap) { return (StgRegTable *)((char *)cap + sizeof(StgFunTable)); }

// the block's free pointer catches up with hp
static inline void alloc_sync(UnisonJitCtx *ctx) {
  if (ctx->hp != NULL) reg_table(ctx->cap)->rCurrentAlloc->free = (StgPtr)ctx->hp;
}

// the words the budget still allows (negative once it is exhausted)
static inline int64_t budget_left(const UnisonJitCtx *ctx) {
  return ((int64_t)(intptr_t)ctx->budget_end - (int64_t)(intptr_t)ctx->hp) / (int64_t)sizeof(StgWord);
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

// The slow path: the object doesn't fit in the block, or the budget is used
// up, or there is no block. Generated code calls this by name.
void *unison_jit_alloc_words(UnisonJitCtx *ctx, int64_t n) {
  if (!ctx->bump) {
    // no block to bump in: hp counts the words taken (from 0), so that the
    // poll's test against budget_end still works
    ctx->hp = (void *)((uintptr_t)ctx->hp + (uintptr_t)n * sizeof(StgWord));
    return allocate((Capability *)ctx->cap, n);
  }
  int64_t left = budget_left(ctx) - n;
  alloc_sync(ctx);
  void *p = allocate((Capability *)ctx->cap, n);
  ctx->slow_words += n;
  alloc_refresh(ctx, left);
  return p;
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

// Writes a MutVar# (an IORef) the way compiled Haskell does: the store,
// then the runtime's barrier, which moves a clean variable onto the
// mutable list. Cold code in C (decision D9).
void unison_jit_write_mutvar(UnisonJitCtx *ctx, StgMutVar *mv, StgClosure *v) {
  StgClosure *old = mv->var;
  __atomic_thread_fence(__ATOMIC_RELEASE);
  mv->var = v;
  // the register table follows the function table at the start of a Capability
  // (see unison_jit_hplim_address)
  StgRegTable *reg = (StgRegTable *)((char *)ctx->cap + sizeof(StgFunTable));
  dirty_MUT_VAR(reg, mv, old);
}

// Addresses and sizes from the runtime that generated code needs: the
// info pointers for the two kinds of array in a Seg, and their layouts.
int64_t unison_jit_rts_facts(int64_t *out, int64_t n) {
  int64_t facts[] = {
      (int64_t)&stg_ARR_WORDS_info,                // 0: ByteArray# info
      (int64_t)&stg_MUT_ARR_PTRS_FROZEN_CLEAN_info, // 1: frozen Array# info
      sizeofW(StgArrBytes),                         // 2: ByteArray# header words (incl. the byte count)
      offsetof(StgArrBytes, bytes) / sizeof(W_),    // 3: word index of the byte count
      sizeofW(StgMutArrPtrs),                       // 4: Array# header words (incl. ptrs and size)
      offsetof(StgMutArrPtrs, ptrs) / sizeof(W_),   // 5: word index of the element count
      offsetof(StgMutArrPtrs, size) / sizeof(W_),   // 6: word index of the payload size (elements + card table)
      MUT_ARR_PTRS_CARD_BITS,                       // 7: log2 of elements per card
      (int64_t)&unison_jit_alloc_words,             // 8: the allocator
      offsetof(StgMutVar, var) / sizeof(W_),        // 9: word index of a MutVar#'s content
      (int64_t)&unison_jit_write_mutvar,            // 10: the MutVar# write barrier
      (int64_t)&stg_MUT_ARR_PTRS_DIRTY_info,        // 11: mutable Array# info after a write
  };
  int64_t count = sizeof facts / sizeof facts[0];
  for (int64_t i = 0; i < n && i < count; i++) out[i] = facts[i];
  return count;
}

// ---------------------------------------------------------------------------
// Lists
//
// A Unison list is a Unison.Util.Deque of Val (a strict finger tree), held
// as ListC deque. Every field is strict, so everything reachable
// from a list is an evaluated, tagged constructor and can be read and built
// here without evaluating anything. These helpers are ports of the Haskell
// operations in lib/unison-util-rope's Deque.hs (same results, structure
// included) and take every case: a helper answers "not handled" (NULL, or a
// negative size) only for a closure that isn't a list. They allocate with
// allocate(), like generated code, and never call into Haskell.
//
// The layouts are GHC's for the constructors in Deque.hs (pointer fields
// first, in declaration order, then the unpacked words):
//
//   SList: SNil (tag 1), SCons x rest (tag 2)
//   Deque: Nil (tag 1), Deep t pr m sf (tag 2): pointers pr, m, sf; word t
//   Mid:   MNil (tag 1), MDeep t ps pr m sf (tag 2): pointers pr, m, sf;
//          words t, ps
//   Node:  N8 a .. h (tag 1): eight pointers
//          NA size kids (tag 2): the pointer to a SmallArray#, then the size
//
// t packs the number of leaves (bits 8 and up), the suffix's item count
// (bits 4 to 7) and the prefix's (bits 0 to 3). A prefix runs front to back
// and a suffix back to front. A Deque is the top level, whose items are the
// elements; a Mid is a level below, whose items are nodes, and ps is the
// number of leaves under its prefix. The code here handles both with one set
// of functions that take `top`.
//
// The layouts are checked against sample lists at startup, see
// unison_jit_list_init and unison_jit_list_check, and then every helper is
// run against the Haskell operation; if anything is off the JIT stays off.



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

static inline int lv_read(StgClosure *c, int top, Lv *v) {
  if (LTAG(c) != 2) return 0;
  v->pr = LP(c, 0), v->m = LP(c, 1), v->sf = LP(c, 2);
  v->t = LW(c, 3);
  v->ps = top ? 0 : LW(c, 4);
  return 1;
}

static inline StgInt mid_size(StgClosure *m) { return LTAG(m) == 2 ? TSIZE(LW(m, 3)) : 0; }

static inline StgClosure **node_kids(StgClosure *nd) {
  return LTAG(nd) == 1 ? LUN(nd)->payload : ((StgSmallMutArrPtrs *)LP(nd, 0))->payload;
}
static inline StgInt node_arity(StgClosure *nd) {
  return LTAG(nd) == 1 ? 8 : (StgInt)((StgSmallMutArrPtrs *)LP(nd, 0))->ptrs;
}
static inline StgInt node_size(StgClosure *nd) { return LTAG(nd) == 1 ? 8 : LW(nd, 1); }

// Everything allocated is written in full before the helper returns: the
// heap must have no gaps.
static inline StgWord *list_alloc(UnisonJitCtx *ctx, int64_t words) { return nat_alloc(ctx, words); }

// --- building ---

// ListC dq, in two words at p.
static inline void *wrap_at(StgWord *p, StgClosure *dq) {
  p[0] = LF.list_info;
  p[1] = (StgWord)dq;
  return (void *)((StgWord)p | LF.list_tag);
}

static inline void *wrap_list(UnisonJitCtx *ctx, StgClosure *dq) { return wrap_at(list_alloc(ctx, 2), dq); }

#define DEEP_WORDS 5
static inline StgClosure *deep_at(StgWord *p, StgInt t, StgClosure *pr, StgClosure *m, StgClosure *sf) {
  p[0] = LF.deep_info;
  p[1] = (StgWord)pr;
  p[2] = (StgWord)m;
  p[3] = (StgWord)sf;
  p[4] = (StgWord)t;
  return (StgClosure *)((StgWord)p | 2);
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

// a Deep (ps isn't stored) or an MDeep
static inline StgClosure *mk_level(UnisonJitCtx *ctx, int top, StgInt t, StgInt ps, StgClosure *pr, StgClosure *m,
                                   StgClosure *sf) {
  if (top) return deep_at(list_alloc(ctx, DEEP_WORDS), t, pr, m, sf);
  StgWord *p = list_alloc(ctx, 6);
  p[0] = LF.mdeep_info;
  p[1] = (StgWord)pr;
  p[2] = (StgWord)m;
  p[3] = (StgWord)sf;
  p[4] = (StgWord)t;
  p[5] = (StgWord)ps;
  return (StgClosure *)((StgWord)p | 2);
}

static inline StgClosure *mk_n8(UnisonJitCtx *ctx, StgClosure **k) {
  StgWord *p = list_alloc(ctx, 9);
  p[0] = LF.n8_info;
  for (int i = 0; i < 8; i++) p[1 + i] = (StgWord)k[i];
  return (StgClosure *)((StgWord)p | 1);
}

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

// the first k cells of a list, copied (all of them if it is shorter)
static StgClosure *s_take(UnisonJitCtx *ctx, StgInt k, StgClosure *l) {
  StgClosure *res = LF.snil, **tail = &res;
  for (; k > 0 && IS_CONS(l); k--, l = TAIL(l)) {
    StgWord *p = list_alloc(ctx, 3);
    *tail = scons_at(p, HEAD(l), LF.snil);
    tail = (StgClosure **)&p[2];
  }
  return res;
}

static inline StgClosure *s_drop(StgInt k, StgClosure *l) {
  for (; k > 0 && IS_CONS(l); k--) l = TAIL(l);
  return l;
}

// the first list reversed, then acc
static StgClosure *s_rev_onto(UnisonJitCtx *ctx, StgClosure *l, StgClosure *acc) {
  for (; IS_CONS(l); l = TAIL(l)) acc = scons(ctx, HEAD(l), acc);
  return acc;
}

// a copy of the first list that ends in the second
static StgClosure *s_append(UnisonJitCtx *ctx, StgClosure *a, StgClosure *b) {
  StgClosure *res = b, **tail = &res;
  for (; IS_CONS(a); a = TAIL(a)) {
    StgWord *p = list_alloc(ctx, 3);
    *tail = scons_at(p, HEAD(a), b);
    tail = (StgClosure **)&p[2];
  }
  return res;
}

// the leaves under a list of nodes
static inline StgInt s_sum(StgClosure *l) {
  StgInt s = 0;
  for (; IS_CONS(l); l = TAIL(l)) s += node_size(HEAD(l));
  return s;
}

// a list's items into an array, in list order; their number
static inline StgInt s_items(StgClosure *l, StgClosure **out) {
  StgInt n = 0;
  for (; IS_CONS(l); l = TAIL(l)) out[n++] = HEAD(l);
  return n;
}

// k[0], ..., k[n-1], then rest
static StgClosure *s_from(UnisonJitCtx *ctx, StgClosure **k, StgInt n, StgClosure *rest) {
  for (StgInt i = n - 1; i >= 0; i--) rest = scons(ctx, k[i], rest);
  return rest;
}

// k[n-1], ..., k[0], then rest
static StgClosure *s_from_rev(UnisonJitCtx *ctx, StgClosure **k, StgInt n, StgClosure *rest) {
  for (StgInt i = 0; i < n; i++) rest = scons(ctx, k[i], rest);
  return rest;
}

// all but the first `from` children of a node, front to back (nodeDropFwd)
static inline StgClosure *kids_fwd(UnisonJitCtx *ctx, StgClosure *nd, StgInt from) {
  return s_from(ctx, node_kids(nd) + from, node_arity(nd) - from, LF.snil);
}

// the first k children of a node, back to front (nodeTakeRev)
static inline StgClosure *kids_rev(UnisonJitCtx *ctx, StgClosure *nd, StgInt k) {
  return s_from_rev(ctx, node_kids(nd), k, LF.snil);
}

// --- adding at the ends (cons, consFull, consM; snoc, snocFull, snocM) ---

static StgClosure *lv_cons(UnisonJitCtx *ctx, int top, StgClosure *x, StgClosure *c) {
  Lv v;
  StgInt s = top ? 1 : node_size(x);
  if (!lv_read(c, top, &v)) return mk_level(ctx, top, MK(s, 1, 0), s, scons(ctx, x, LF.snil), LF.mnil, LF.snil);
  if (TPC(v.t) < MAXD) return mk_level(ctx, top, v.t + (s << 8) + 1, v.ps + s, scons(ctx, x, v.pr), v.m, v.sf);
  if (TSC(v.t) == 0) {
    // no middle yet: the far half of the prefix becomes the suffix
    StgClosure *kept = s_take(ctx, 5, v.pr);
    StgInt ps = top ? 0 : s + s_sum(kept);
    return mk_level(ctx, top, MK(TSIZE(v.t) + s, 6, 5), ps, scons(ctx, x, kept), LF.mnil,
                    s_rev_onto(ctx, s_drop(5, v.pr), LF.snil));
  }
  // the prefix keeps its two outermost items and sheds the other eight
  StgClosure *k[MAXD];
  s_items(v.pr, k);
  StgInt keep = top ? 2 : node_size(k[0]) + node_size(k[1]);
  StgClosure *nd = top ? mk_n8(ctx, k + 2) : mk_na(ctx, v.ps - keep, k + 2, 8);
  StgClosure *m = lv_cons(ctx, 0, nd, v.m);
  return mk_level(ctx, top, v.t + (s << 8) - 7, s + keep, scons(ctx, x, s_from(ctx, k, 2, LF.snil)), m, v.sf);
}

static StgClosure *lv_snoc(UnisonJitCtx *ctx, int top, StgClosure *c, StgClosure *x) {
  Lv v;
  StgInt s = top ? 1 : node_size(x);
  if (!lv_read(c, top, &v)) return mk_level(ctx, top, MK(s, 0, 1), 0, LF.snil, LF.mnil, scons(ctx, x, LF.snil));
  if (TSC(v.t) < MAXD) return mk_level(ctx, top, v.t + (s << 8) + 0x10, v.ps, v.pr, v.m, scons(ctx, x, v.sf));
  if (TPC(v.t) == 0) {
    StgClosure *pr = s_rev_onto(ctx, s_drop(5, v.sf), LF.snil);
    return mk_level(ctx, top, MK(TSIZE(v.t) + s, 5, 6), top ? 0 : s_sum(pr), pr, LF.mnil,
                    scons(ctx, x, s_take(ctx, 5, v.sf)));
  }
  // the suffix runs back to front: s1, s2, then the eight to shed, last first
  StgClosure *k[MAXD], *shed[8];
  s_items(v.sf, k);
  for (int i = 0; i < 8; i++) shed[i] = k[9 - i];
  StgInt sz = top ? 8 : TSIZE(v.t) - v.ps - mid_size(v.m) - node_size(k[0]) - node_size(k[1]);
  StgClosure *nd = top ? mk_n8(ctx, shed) : mk_na(ctx, sz, shed, 8);
  StgClosure *m = lv_snoc(ctx, 0, v.m, nd);
  return mk_level(ctx, top, v.t + (s << 8) - 0x70, v.ps, v.pr, m, scons(ctx, x, s_from(ctx, k, 2, LF.snil)));
}

// --- removing at the ends (uncons, unconsLast, unconsSlow, unconsM; and unsnoc) ---

// The first item of a level that isn't empty, and the level without it.
static StgClosure *lv_uncons(UnisonJitCtx *ctx, int top, StgClosure *c, StgClosure **out) {
  Lv v;
  lv_read(c, top, &v);
  StgClosure *empty = top ? LF.nil : LF.mnil;
  if (IS_CONS(v.pr)) {
    StgClosure *x = HEAD(v.pr), *rest = TAIL(v.pr);
    StgInt s = top ? 1 : node_size(x);
    *out = x;
    if (!IS_CONS(rest) && LTAG(v.m) == 2) {
      // the prefix's only item goes: the first node of the middle becomes the prefix
      StgClosure *nn;
      StgClosure *m = lv_uncons(ctx, 0, v.m, &nn);
      return mk_level(ctx, top, v.t - (s << 8) - 1 + node_arity(nn), node_size(nn), kids_fwd(ctx, nn, 0), m, v.sf);
    }
    StgInt t = v.t - (s << 8) - 1;
    return t < 0x100 ? empty : mk_level(ctx, top, t, v.ps - s, rest, v.m, v.sf);
  }
  // the prefix is empty, so there is no middle: half of the suffix moves over
  StgInt sc = TSC(v.t), q = (sc - 1) / 2, p = sc - 1 - q;
  StgClosure *l = s_rev_onto(ctx, s_drop(q, v.sf), LF.snil);
  StgClosure *x = HEAD(l), *pr = TAIL(l);
  *out = x;
  if (sc == 1) return empty;
  return mk_level(ctx, top, MK(TSIZE(v.t) - (top ? 1 : node_size(x)), p, q), top ? 0 : s_sum(pr), pr, LF.mnil,
                  s_take(ctx, q, v.sf));
}

static StgClosure *lv_unsnoc(UnisonJitCtx *ctx, int top, StgClosure *c, StgClosure **out) {
  Lv v;
  lv_read(c, top, &v);
  StgClosure *empty = top ? LF.nil : LF.mnil;
  if (IS_CONS(v.sf)) {
    StgClosure *x = HEAD(v.sf), *rest = TAIL(v.sf);
    StgInt s = top ? 1 : node_size(x);
    *out = x;
    if (!IS_CONS(rest) && LTAG(v.m) == 2) {
      StgClosure *nn;
      StgClosure *m = lv_unsnoc(ctx, 0, v.m, &nn);
      StgInt k = node_arity(nn);
      return mk_level(ctx, top, v.t - (s << 8) - 0x10 + (k << 4), v.ps, v.pr, m, kids_rev(ctx, nn, k));
    }
    StgInt t = v.t - (s << 8) - 0x10;
    return t < 0x100 ? empty : mk_level(ctx, top, t, v.ps, v.pr, v.m, rest);
  }
  StgInt pc = TPC(v.t), p = (pc - 1) / 2, q = pc - 1 - p;
  StgClosure *l = s_rev_onto(ctx, s_drop(p, v.pr), LF.snil);
  StgClosure *x = HEAD(l), *sf = TAIL(l);
  *out = x;
  if (pc == 1) return empty;
  StgClosure *pr = s_take(ctx, p, v.pr);
  return mk_level(ctx, top, MK(TSIZE(v.t) - (top ? 1 : node_size(x)), p, q), top ? 0 : s_sum(pr), pr, LF.mnil, sf);
}

// A level whose suffix would be empty: the suffix gets the last node of the
// middle, if there is one (deepR, mdeepR). And the same for a prefix.
static StgClosure *lv_deep_r(UnisonJitCtx *ctx, int top, StgInt n, StgInt pc, StgInt ps, StgClosure *pr,
                             StgClosure *m) {
  if (LTAG(m) != 2) return mk_level(ctx, top, MK(n, pc, 0), ps, pr, LF.mnil, LF.snil);
  StgClosure *nd;
  StgClosure *m2 = lv_unsnoc(ctx, 0, m, &nd);
  StgInt k = node_arity(nd);
  return mk_level(ctx, top, MK(n, pc, k), ps, pr, m2, kids_rev(ctx, nd, k));
}

static StgClosure *lv_deep_l(UnisonJitCtx *ctx, int top, StgInt n, StgInt sc, StgClosure *m, StgClosure *sf) {
  if (LTAG(m) != 2) return mk_level(ctx, top, MK(n, 0, sc), 0, LF.snil, LF.mnil, sf);
  StgClosure *nd;
  StgClosure *m2 = lv_uncons(ctx, 0, m, &nd);
  return mk_level(ctx, top, MK(n, node_arity(nd), sc), node_size(nd), kids_fwd(ctx, nd, 0), m2, sf);
}

// --- lookup ---

static inline StgClosure *s_nth(StgClosure *l, StgInt k) {
  while (k-- > 0) l = TAIL(l);
  return HEAD(l);
}

// The node of a middle holding its leaf number i, and the leaf's offset in
// the node (lookM). A full child of this level's nodes has 2^sh leaves; sh is
// negative for a text's middle, where a node's size says nothing of its
// children's.
static StgClosure *mid_look(StgClosure *c, int sh, StgInt i, StgInt *off) {
  Lv v;
  lv_read(c, 0, &v);
  if (i < v.ps) {
    for (StgClosure *l = v.pr;; l = TAIL(l)) {
      StgClosure *nd = HEAD(l);
      StgInt s = node_size(nd);
      if (i < s) {
        *off = i;
        return nd;
      }
      i -= s;
    }
  }
  StgInt i2 = i - v.ps;
  if (i2 < mid_size(v.m)) {
    StgInt o;
    StgClosure *nn = mid_look(v.m, sh < 0 ? sh : sh + 3, i2, &o);
    StgClosure **kids = node_kids(nn);
    if (sh >= 0 && node_size(nn) == (StgInt)8 << (sh + 3) && node_arity(nn) == 8) { // all eight children full
      *off = o & (((StgInt)1 << (sh + 3)) - 1);
      return kids[o >> (sh + 3)];
    }
    for (StgInt q = 0;; q++) {
      StgInt s = node_size(kids[q]);
      if (o < s) {
        *off = o;
        return kids[q];
      }
      o -= s;
    }
  }
  StgInt r = TSIZE(v.t) - 1 - i; // leaves after the one looked for
  for (StgClosure *l = v.sf;; l = TAIL(l)) {
    StgClosure *nd = HEAD(l);
    StgInt s = node_size(nd);
    if (r < s) {
      *off = s - 1 - r;
      return nd;
    }
    r -= s;
  }
}

// --- take and drop ---

// The first j leaves of a middle, 0 < j <= size (takeM): returns everything
// before the node holding the last of them; that node, and how many of its
// leaves are among them.
static StgClosure *mid_take(UnisonJitCtx *ctx, StgClosure *c, StgInt j, StgClosure **node, StgInt *k) {
  Lv v;
  lv_read(c, 0, &v);
  StgInt n = TSIZE(v.t), pc = TPC(v.t), sc = TSC(v.t), ms = mid_size(v.m);
  if (j <= v.ps) {
    StgInt q = 0, sb = 0; // q whole nodes of the prefix, with sb leaves, come before the cut
    for (StgClosure *l = v.pr;; l = TAIL(l), q++) {
      StgClosure *nd = HEAD(l);
      StgInt s = node_size(nd);
      if (j <= sb + s) {
        *node = nd;
        *k = j - sb;
        return q == 0 ? LF.mnil : mk_level(ctx, 0, MK(sb, q, 0), sb, s_take(ctx, q, v.pr), LF.mnil, LF.snil);
      }
      sb += s;
    }
  }
  if (j <= v.ps + ms) {
    StgClosure *nn;
    StgInt kk;
    StgClosure *m2 = mid_take(ctx, v.m, j - v.ps, &nn, &kk);
    // the children of the node the level below cut in: those before the one
    // holding the cut make the new suffix
    StgClosure **kids = node_kids(nn);
    StgInt q = 0, sb = 0;
    for (;; q++) {
      StgInt s = node_size(kids[q]);
      if (kk - 1 < sb + s) break;
      sb += s;
    }
    StgInt total = v.ps + mid_size(m2) + sb;
    *node = kids[q];
    *k = kk - sb;
    if (total == 0) return LF.mnil;
    if (q == 0) return lv_deep_r(ctx, 0, total, pc, v.ps, v.pr, m2);
    return mk_level(ctx, 0, MK(total, pc, q), v.ps, v.pr, m2, kids_rev(ctx, nn, q));
  }
  StgInt r = n - j, cnt = sc; // r leaves are to go from the back; cnt nodes of the suffix are left
  for (StgClosure *l = v.sf;; l = TAIL(l), cnt--) {
    StgClosure *nd = HEAD(l);
    StgInt s = node_size(nd);
    if (r >= s) {
      r -= s;
      continue;
    }
    StgInt keep = s - r, total = j - keep;
    *node = nd;
    *k = keep;
    if (total == 0) return LF.mnil;
    if (cnt == 1) return lv_deep_r(ctx, 0, total, pc, v.ps, v.pr, v.m);
    return mk_level(ctx, 0, MK(total, pc, cnt - 1), v.ps, v.pr, v.m, TAIL(l));
  }
}

// All but the first j leaves of a middle, 0 <= j < size (dropM): returns
// everything after the node holding leaf j; that node, and how many of its
// leaves to drop.
static StgClosure *mid_drop(UnisonJitCtx *ctx, StgClosure *c, StgInt j, StgClosure **node, StgInt *k) {
  Lv v;
  lv_read(c, 0, &v);
  StgInt n = TSIZE(v.t), pc = TPC(v.t), sc = TSC(v.t), ms = mid_size(v.m);
  if (j < v.ps) {
    // kk leaves still to drop; the prefix has cnt nodes and psz leaves left, the level tot
    StgInt kk = j, cnt = pc, psz = v.ps, tot = n;
    for (StgClosure *l = v.pr;; l = TAIL(l)) {
      StgClosure *nd = HEAD(l);
      StgInt s = node_size(nd);
      if (kk >= s) {
        kk -= s, cnt--, psz -= s, tot -= s;
        continue;
      }
      *node = nd;
      *k = kk;
      if (tot == s) return LF.mnil;
      if (cnt == 1) return lv_deep_l(ctx, 0, tot - s, sc, v.m, v.sf);
      return mk_level(ctx, 0, MK(tot - s, cnt - 1, sc), psz - s, TAIL(l), v.m, v.sf);
    }
  }
  if (j < v.ps + ms) {
    StgClosure *nn;
    StgInt kk;
    StgClosure *m2 = mid_drop(ctx, v.m, j - v.ps, &nn, &kk);
    // the children of the node the level below cut in: those after the one
    // holding the cut make the new prefix
    StgClosure **kids = node_kids(nn);
    StgInt q = 0, sb = 0;
    for (;; q++) {
      StgInt s = node_size(kids[q]);
      if (kk < sb + s) break;
      sb += s;
    }
    StgClosure *nd = kids[q];
    StgInt cn = node_arity(nn) - q - 1;
    StgInt sa = node_size(nn) - sb - node_size(nd);
    StgInt total = sa + mid_size(m2) + (n - v.ps - ms);
    *node = nd;
    *k = kk - sb;
    if (total == 0) return LF.mnil;
    if (cn == 0) return lv_deep_l(ctx, 0, total, sc, m2, v.sf);
    return mk_level(ctx, 0, MK(total, cn, sc), sa, kids_fwd(ctx, nn, q + 1), m2, v.sf);
  }
  // r leaves come after leaf j; cn nodes of the suffix, with sa leaves, are wholly after it
  StgInt r = n - 1 - j, cn = 0, sa = 0;
  for (StgClosure *l = v.sf;; l = TAIL(l)) {
    StgClosure *nd = HEAD(l);
    StgInt s = node_size(nd);
    if (r >= s) {
      r -= s, cn++, sa += s;
      continue;
    }
    *node = nd;
    *k = s - 1 - r;
    if (cn == 0) return LF.mnil;
    return mk_level(ctx, 0, MK(sa, 0, cn), 0, LF.snil, LF.mnil, s_take(ctx, cn, v.sf));
  }
}

// take i d; d itself when nothing is cut
static StgClosure *dq_take(UnisonJitCtx *ctx, StgInt i, StgClosure *d) {
  Lv v;
  if (!lv_read(d, 1, &v) || i <= 0) return LF.nil;
  StgInt n = TSIZE(v.t), pc = TPC(v.t), sc = TSC(v.t);
  if (i >= n) return d;
  if (i <= pc) return mk_level(ctx, 1, MK(i, i, 0), 0, s_take(ctx, i, v.pr), LF.mnil, LF.snil);
  if (i >= n - sc) {
    StgInt k = i - (n - sc);
    if (k == 0) return lv_deep_r(ctx, 1, i, pc, 0, v.pr, v.m);
    return mk_level(ctx, 1, MK(i, pc, k), 0, v.pr, v.m, s_drop(sc - k, v.sf));
  }
  StgClosure *nd;
  StgInt k;
  StgClosure *m = mid_take(ctx, v.m, i - pc, &nd, &k);
  return mk_level(ctx, 1, MK(i, pc, k), 0, v.pr, m, kids_rev(ctx, nd, k));
}

static StgClosure *dq_drop(UnisonJitCtx *ctx, StgInt i, StgClosure *d) {
  Lv v;
  if (!lv_read(d, 1, &v)) return LF.nil;
  if (i <= 0) return d;
  StgInt n = TSIZE(v.t), pc = TPC(v.t), sc = TSC(v.t);
  if (i >= n) return LF.nil;
  if (i >= n - sc) {
    StgInt r = n - i;
    return mk_level(ctx, 1, MK(r, 0, r), 0, LF.snil, LF.mnil, s_take(ctx, r, v.sf));
  }
  if (i <= pc) {
    if (i == pc) return lv_deep_l(ctx, 1, n - i, sc, v.m, v.sf);
    return mk_level(ctx, 1, MK(n - i, pc - i, sc), 0, s_drop(i, v.pr), v.m, v.sf);
  }
  StgClosure *nd;
  StgInt k;
  StgClosure *m = mid_drop(ctx, v.m, i - pc, &nd, &k);
  return mk_level(ctx, 1, MK(n - i, node_arity(nd) - k, sc), 0, kids_fwd(ctx, nd, k), m, v.sf);
}

// --- append ---

// c leaves as nodes (packLeaves), c >= 2: eight to a node while that leaves
// none or at least two for the next. Returns the number of nodes.
static StgInt pack_leaves(UnisonJitCtx *ctx, StgInt c, StgClosure **items, StgClosure **out) {
  StgInt n = 0;
  while (c > 0) {
    StgInt k = c <= 8 ? c : c == 9 ? 5 : 8;
    out[n++] = k == 8 ? mk_n8(ctx, items) : mk_na(ctx, k, items, k);
    items += k, c -= k;
  }
  return n;
}

// c nodes with sz leaves under them as nodes one level down (packNodes),
// c >= 2: one node of up to eight, or two or three of about the same number
// of children.
static StgInt pack_nodes(UnisonJitCtx *ctx, StgInt c, StgInt sz, StgClosure **items, StgClosure **out) {
  if (c <= 8) {
    out[0] = mk_na(ctx, sz, items, c);
    return 1;
  }
  StgInt q = (c + 7) >> 3, k1 = (c + q - 1) / q, sx = 0;
  for (StgInt i = 0; i < k1; i++) sx += node_size(items[i]);
  out[0] = mk_na(ctx, sx, items, k1);
  if (q == 2) {
    out[1] = mk_na(ctx, sz - sx, items + k1, c - k1);
    return 2;
  }
  StgInt k2 = (c - k1 + 1) / 2, sy = 0;
  for (StgInt i = 0; i < k2; i++) sy += node_size(items[k1 + i]);
  out[1] = mk_na(ctx, sy, items + k1, k2);
  out[2] = mk_na(ctx, sz - sx - sy, items + k1 + k2, c - k1 - k2);
  return 3;
}

// Two levels with nn items between them (none at the top), which hold sns
// leaves (append, appM). The digits that end up inside either join the outer
// digit of a side that has no middle, when they fit there, or are packed
// into nodes and handed down between the two middles.
static StgClosure *lv_append(UnisonJitCtx *ctx, int top, StgClosure *a, StgInt sns, StgClosure **ns, StgInt nn,
                             StgClosure *b) {
  Lv x, y;
  if (!lv_read(a, top, &x)) {
    for (StgInt i = nn - 1; i >= 0; i--) b = lv_cons(ctx, top, ns[i], b);
    return b;
  }
  if (!lv_read(b, top, &y)) {
    for (StgInt i = 0; i < nn; i++) a = lv_snoc(ctx, top, a, ns[i]);
    return a;
  }
  StgInt pc1 = TPC(x.t), sc1 = TSC(x.t), pc2 = TPC(y.t), sc2 = TSC(y.t);
  StgInt n = TSIZE(x.t) + sns + TSIZE(y.t), c = sc1 + nn + pc2;
  if (LTAG(x.m) != 2 && pc1 + c <= MAXD) {
    StgClosure *inner = s_rev_onto(ctx, x.sf, s_from(ctx, ns, nn, y.pr));
    return mk_level(ctx, top, MK(n, pc1 + c, sc2), TSIZE(x.t) + sns + y.ps, s_append(ctx, x.pr, inner), y.m, y.sf);
  }
  if (LTAG(y.m) != 2 && c + sc2 <= MAXD) {
    StgClosure *inner = s_rev_onto(ctx, y.pr, s_from_rev(ctx, ns, nn, x.sf));
    return mk_level(ctx, top, MK(n, pc1, c + sc2), x.ps, x.pr, x.m, s_append(ctx, y.sf, inner));
  }
  StgClosure *items[3 * MAXD];
  if (c >= 2 && pc1 != 0 && sc2 != 0) {
    // the left suffix (back to front), the items between, the right prefix
    StgInt at = sc1;
    for (StgClosure *l = x.sf; IS_CONS(l); l = TAIL(l)) items[--at] = HEAD(l);
    at = sc1;
    for (StgInt i = 0; i < nn; i++) items[at++] = ns[i];
    s_items(y.pr, items + at);
    StgClosure *out[3];
    StgInt sz = top ? c : (TSIZE(x.t) - x.ps - mid_size(x.m)) + sns + y.ps;
    StgInt no = top ? pack_leaves(ctx, c, items, out) : pack_nodes(ctx, c, sz, items, out);
    StgClosure *m = lv_append(ctx, 0, x.m, sz, out, no, y.m);
    return mk_level(ctx, top, MK(n, pc1, sc2), x.ps, x.pr, m, y.sf);
  }
  // what is left: a side with no middle whose outer digit is empty, or a
  // single item inside
  if (LTAG(x.m) != 2) {
    for (StgInt i = nn - 1; i >= 0; i--) b = lv_cons(ctx, top, ns[i], b);
    for (StgClosure *l = x.sf; IS_CONS(l); l = TAIL(l)) b = lv_cons(ctx, top, HEAD(l), b);
    for (StgInt i = s_items(x.pr, items) - 1; i >= 0; i--) b = lv_cons(ctx, top, items[i], b);
    return b;
  }
  for (StgInt i = 0; i < nn; i++) a = lv_snoc(ctx, top, a, ns[i]);
  for (StgClosure *l = y.pr; IS_CONS(l); l = TAIL(l)) a = lv_snoc(ctx, top, a, HEAD(l));
  for (StgInt i = s_items(y.sf, items) - 1; i >= 0; i--) a = lv_snoc(ctx, top, a, items[i]);
  return a;
}

// --- the helpers generated code calls ---

// List.size, or -1 if the closure isn't a list.
int64_t unison_jit_list_size(void *list) {
  StgClosure *dq = deque_of(list);
  if (dq == NULL) return -1;
  return LTAG(dq) == 2 ? TSIZE(LW(dq, 3)) : 0;
}

// A SeqView closure: Data2 tag a b ref, whose pointers are a's, b's, ref and
// whose words are tag, a's, b's. Here for element x (a Val: a pointer, then a
// word) and the rest of the list (a Foreign), in seven words at p.
static inline void *view_at(StgWord *p, void *empty, int64_t elem_tag, int64_t left, StgClosure *x, void *rest) {
  p[0] = LF.data2_info;
  p[3] = (StgWord)LP(empty, 0);
  p[4] = elem_tag;
  if (left) {
    p[1] = (StgWord)LP(x, 0);
    p[2] = (StgWord)rest;
    p[5] = (StgWord)LP(x, 1);
    p[6] = (StgWord)-1;
  } else {
    p[1] = (StgWord)rest;
    p[2] = (StgWord)LP(x, 0);
    p[5] = (StgWord)-1;
    p[6] = (StgWord)LP(x, 1);
  }
  return (void *)((StgWord)p | LF.data2_tag);
}

// The view of a list from the left (x, rest) or the right (rest, x): the
// SeqView closure the interpreter's VWLS and VWRS build. `empty` is the
// SeqViewEmpty closure (its first field is the type's Reference).
void *unison_jit_list_view(UnisonJitCtx *ctx, void *list, void *empty, int64_t elem_tag, int64_t left) {
  StgClosure *dq = deque_of(list);
  if (dq == NULL) return NULL;
  if (LTAG(dq) != 2) return empty;
  StgClosure *nearend = LP(dq, left ? 0 : 2);
  if (IS_CONS(nearend) && IS_CONS(TAIL(nearend))) {
    // the end's digit keeps an item: one allocation for everything
    StgWord *p = list_alloc(ctx, DEEP_WORDS + 4 + 7);
    StgInt t = LW(dq, 3) - (left ? 0x101 : 0x110);
    StgClosure *rest = TAIL(nearend);
    StgClosure *nd = left ? deep_at(p, t, rest, LP(dq, 1), LP(dq, 2)) : deep_at(p, t, LP(dq, 0), LP(dq, 1), rest);
    return view_at(p + DEEP_WORDS + 4, empty, elem_tag, left, HEAD(nearend), wrap_at(p + DEEP_WORDS, nd));
  }
  StgClosure *x;
  StgClosure *nd = left ? lv_uncons(ctx, 1, dq, &x) : lv_unsnoc(ctx, 1, dq, &x);
  StgWord *p = list_alloc(ctx, 4 + 7);
  return view_at(p + 4, empty, elem_tag, left, x, wrap_at(p, nd));
}

// cons (front) or snoc of the value (u, b).
void *unison_jit_list_push(UnisonJitCtx *ctx, void *list, int64_t u, void *b, int64_t front) {
  StgClosure *dq = deque_of(list);
  // an untagged b is an unevaluated thunk, which must not go into Val's
  // strict field (see requireTagged in Codegen.hs)
  if (dq == NULL || LTAG(b) == 0) return NULL;
  if (LTAG(dq) == 2) {
    StgInt t = LW(dq, 3);
    if ((front ? TPC(t) : TSC(t)) < MAXD) {
      // room in the end's digit: one allocation for everything
      StgWord *p = list_alloc(ctx, 3 + 3 + DEEP_WORDS + 4);
      p[0] = LF.val_info; // the element: Val u b (pointer first)
      p[1] = (StgWord)b;
      p[2] = (StgWord)u;
      StgClosure *cell = scons_at(p + 3, (StgClosure *)((StgWord)p | 1), LP(dq, front ? 0 : 2));
      StgClosure *nd = front ? deep_at(p + 6, t + 0x101, cell, LP(dq, 1), LP(dq, 2))
                             : deep_at(p + 6, t + 0x110, LP(dq, 0), LP(dq, 1), cell);
      return wrap_at(p + 6 + DEEP_WORDS, nd);
    }
  }
  StgWord *p = list_alloc(ctx, 3);
  p[0] = LF.val_info;
  p[1] = (StgWord)b;
  p[2] = (StgWord)u;
  StgClosure *x = (StgClosure *)((StgWord)p | 1);
  return wrap_list(ctx, front ? lv_cons(ctx, 1, x, dq) : lv_snoc(ctx, 1, dq, x));
}

// A list literal is built with these two: the elements are added in order
// to a deque that isn't wrapped yet (NULL for the empty one), and the result
// is wrapped. Nothing else may run in between.
void *unison_jit_list_lit(UnisonJitCtx *ctx, void *acc, int64_t u, void *b) {
  StgWord *p = list_alloc(ctx, 3);
  p[0] = LF.val_info;
  p[1] = (StgWord)b;
  p[2] = (StgWord)u;
  return lv_snoc(ctx, 1, acc == NULL ? LF.nil : (StgClosure *)acc, (StgClosure *)((StgWord)p | 1));
}

void *unison_jit_list_wrap(UnisonJitCtx *ctx, void *acc) {
  return wrap_list(ctx, acc == NULL ? LF.nil : (StgClosure *)acc);
}

// List.at: Some x or None, as the interpreter's IDXS builds them. `none` is
// the None closure (its first field is the type's Reference).
void *unison_jit_list_index(UnisonJitCtx *ctx, void *list, int64_t i, void *none, int64_t some_tag) {
  StgClosure *dq = deque_of(list);
  if (dq == NULL) return NULL;
  if (LTAG(dq) != 2) return none;
  StgInt t = LW(dq, 3), n = TSIZE(t), pc = TPC(t);
  if (i < 0 || i >= n) return none;
  StgInt j = n - 1 - i;
  StgClosure *x;
  if (i < pc)
    x = s_nth(LP(dq, 0), i);
  else if (j < TSC(t))
    x = s_nth(LP(dq, 2), j);
  else {
    StgInt off;
    StgClosure *nd = mid_look(LP(dq, 1), 0, i - pc, &off);
    x = node_kids(nd)[off];
  }
  // Data1 tag v ref: pointers v's, ref; words tag, v's
  StgWord *p = list_alloc(ctx, 5);
  p[0] = LF.data1_info;
  p[1] = (StgWord)LP(x, 0);
  p[2] = (StgWord)LP(none, 0);
  p[3] = some_tag;
  p[4] = (StgWord)LP(x, 1);
  return (void *)((StgWord)p | LF.data1_tag);
}

// The Val closure at index i of a deque, for the comparison walkers.
StgClosure *unison_jit_list_at(StgClosure *dq, StgInt i) {
  if (LTAG(dq) != 2) return NULL;
  StgInt t = LW(dq, 3), n = TSIZE(t), pc = TPC(t);
  if (i < 0 || i >= n) return NULL;
  StgInt j = n - 1 - i;
  if (i < pc) return s_nth(LP(dq, 0), i);
  if (j < TSC(t)) return s_nth(LP(dq, 2), j);
  StgInt off;
  StgClosure *nd = mid_look(LP(dq, 1), 0, i - pc, &off);
  return node_kids(nd)[off];
}

// List.take (take != 0) or List.drop of n, as TAKS and DRPS do them: a
// negative n is a Nat too large to be a size.
void *unison_jit_list_cut(UnisonJitCtx *ctx, void *list, int64_t n, int64_t take) {
  StgClosure *dq = deque_of(list);
  if (dq == NULL) return NULL;
  StgClosure *r = take ? (n < 0 ? dq : dq_take(ctx, n, dq)) : (n < 0 ? LF.nil : dq_drop(ctx, n, dq));
  return r == dq ? list : wrap_list(ctx, r);
}

// SPLL (left != 0) and SPLR: the list split after its first n elements, or
// before its last n, as a SeqView of the two parts; the empty view if the
// list is shorter than n.
void *unison_jit_list_split(UnisonJitCtx *ctx, void *list, int64_t n, void *empty, int64_t elem_tag, int64_t left) {
  StgClosure *dq = deque_of(list);
  if (dq == NULL) return NULL;
  StgInt sz = LTAG(dq) == 2 ? TSIZE(LW(dq, 3)) : 0;
  if (sz < n) return empty;
  StgInt i = left ? n : sz - n;
  StgClosure *a = dq_take(ctx, i, dq), *b = dq_drop(ctx, i, dq);
  void *fa = a == dq ? list : wrap_list(ctx, a), *fb = b == dq ? list : wrap_list(ctx, b);
  StgWord *p = list_alloc(ctx, 7);
  p[0] = LF.data2_info;
  p[1] = (StgWord)fa;
  p[2] = (StgWord)fb;
  p[3] = (StgWord)LP(empty, 0);
  p[4] = elem_tag;
  p[5] = (StgWord)-1;
  p[6] = (StgWord)-1;
  return (void *)((StgWord)p | LF.data2_tag);
}

// List.++
void *unison_jit_list_append(UnisonJitCtx *ctx, void *x, void *y) {
  StgClosure *a = deque_of(x), *b = deque_of(y);
  if (a == NULL || b == NULL) return NULL;
  if (LTAG(a) != 2) return y;
  if (LTAG(b) != 2) return x;
  return wrap_list(ctx, lv_append(ctx, 1, a, 0, NULL, 0, b));
}

// --- checking the layouts ---

// A sample handed over from Haskell can be a reference to a top-level value
// (the optimizer makes constants of the samples it can), which is an
// untagged pointer to an indirection. This follows it to the constructor
// and tags the pointer the way a field holding it would be. Values on the
// Unison stack don't need this: the helpers decline a pointer without its
// tag, and the interpreter handles it.
void *unison_jit_settle(void *p) {
  StgClosure *c = (StgClosure *)p;
  for (;;) {
    if (GET_CLOSURE_TAG(c) != 0) return c;
    const StgInfoTable *it = get_itbl(c);
    if (it->type == IND || it->type == IND_STATIC) {
      c = ((StgInd *)c)->indirectee;
      continue;
    }
    // an evaluated thunk: the RTS overwrites it with BLACKHOLE and points
    // the indirectee at the value (an untagged indirectee is the TSO or
    // blocking queue of a thunk still being evaluated: leave that alone)
    if (it->type == BLACKHOLE) {
      StgClosure *ind = ((StgInd *)c)->indirectee;
      if (GET_CLOSURE_TAG(ind) == 0) return c;
      c = ind;
      continue;
    }
    if (it->type >= CONSTR && it->type <= CONSTR_NOCAF) {
      StgWord tag = it->srt + 1; // the constructor's number
      return (StgClosure *)((StgWord)c | (tag > 7 ? 7 : tag));
    }
    return c;
  }
}

static int con_is(StgClosure *c, StgWord tag, StgWord ptrs, StgWord nptrs) {
  if (LTAG(c) != tag) return 0;
  const StgInfoTable *it = get_itbl(LUN(c));
  return it->type >= CONSTR && it->type <= CONSTR_NOCAF && it->layout.payload.ptrs == ptrs &&
         it->layout.payload.nptrs == nptrs;
}

// a first constructor that has no fields (a static closure)
static int con_nullary(StgClosure *c) {
  if (LTAG(c) != 1) return 0;
  const StgInfoTable *it = get_itbl(LUN(c));
  return it->type >= CONSTR && it->type <= CONSTR_NOCAF && it->layout.payload.ptrs == 0;
}

// Whether c is the same constructor without fields as ref. Not a comparison
// of addresses: a constructor without fields has a static closure of its own,
// but so does every top-level binding that is just that constructor (empty =
// Nil), and which of them a piece of code refers to is up to the compiler.
static inline int same_con0(StgClosure *c, StgClosure *ref) {
  return LTAG(c) == LTAG(ref) && LUN(c)->header.info == LUN(ref)->header.info;
}

// the length of a digit's list, or -1 if it isn't one
static StgInt check_slist(StgClosure *l) {
  StgInt len = 0;
  for (; LTAG(l) == 2; l = LP(l, 1), len++)
    if ((StgWord)LUN(l)->header.info != LF.scons_info || len > MAXD) return -1;
  return same_con0(l, LF.snil) ? len : -1;
}

static int is_small_array(StgClosure *a) {
  const StgInfoTable *info = a->header.info;
  return info == &stg_SMALL_MUT_ARR_PTRS_FROZEN_CLEAN_info || info == &stg_SMALL_MUT_ARR_PTRS_FROZEN_DIRTY_info;
}

// the leaves under a node `depth` levels down (1: its children are elements), or -1
static StgInt check_node(StgClosure *nd, int depth, int64_t *kinds) {
  if (LTAG(nd) == 1) {
    if (depth != 1 || (StgWord)LUN(nd)->header.info != LF.n8_info) return -1;
    *kinds |= 2;
    return 8;
  }
  if (LTAG(nd) != 2 || (StgWord)LUN(nd)->header.info != LF.na_info || !is_small_array(LP(nd, 0))) return -1;
  *kinds |= 4;
  StgInt arity = node_arity(nd), total = 0;
  if (arity < 2 || arity > 8) return -1;
  for (StgInt c = 0; c < arity; c++) {
    StgInt z = depth == 1 ? 1 : check_node(node_kids(nd)[c], depth - 1, kinds);
    if (z < 0) return -1;
    total += z;
  }
  return total == LW(nd, 1) ? total : -1;
}

// a digit of a level `depth` down with the given count; its leaves, or -1
static StgInt check_digit(StgClosure *l, int depth, StgInt count, int64_t *kinds) {
  if (check_slist(l) != count) return -1;
  StgInt total = 0;
  for (; LTAG(l) == 2; l = LP(l, 1)) {
    StgInt z = depth == 0 ? (LTAG(HEAD(l)) == 1 ? 1 : -1) : check_node(HEAD(l), depth, kinds);
    if (z < 0) return -1;
    total += z;
  }
  return total;
}

// Walks a whole list checking that every constructor has the shape, the
// counts and the invariants this file assumes. Returns -1 if not, else a
// set of bits saying which constructors it met: 1 for a Deep, 2 and 4 for
// the two kinds of node, 8 for an MDeep.
int64_t unison_jit_list_check(void **elems) {
  StgClosure *c = deque_of(unison_jit_settle(elems[0]));
  if (c == NULL) return -1;
  if (LTAG(c) == 1) return same_con0(c, LF.nil) ? 0 : -1;
  int64_t kinds = 1;
  StgInt sizes[64], befores[64];
  int depth = 0;
  // down: each level's digits; then up: each level's size against its parts
  StgInt inner = 0;
  for (;; depth++) {
    if (depth >= 64) return -1;
    if (depth == 0 ? (StgWord)LUN(c)->header.info != LF.deep_info || !con_is(c, 2, 3, 1)
                   : (StgWord)LUN(c)->header.info != LF.mdeep_info || !con_is(c, 2, 3, 2))
      return -1;
    if (depth > 0) kinds |= 8;
    StgInt t = LW(c, 3);
    StgInt a = check_digit(LP(c, 0), depth, TPC(t), &kinds), b = check_digit(LP(c, 2), depth, TSC(t), &kinds);
    if (a < 0 || b < 0 || TSIZE(t) <= 0) return -1;
    if (depth > 0 && LW(c, 4) != a) return -1;
    sizes[depth] = TSIZE(t), befores[depth] = a + b;
    StgClosure *m = LP(c, 1);
    if (LTAG(m) == 1) {
      if (!same_con0(m, LF.mnil)) return -1;
      break;
    }
    // a level with a middle has an item in both digits
    if (TPC(t) == 0 || TSC(t) == 0) return -1;
    c = m;
  }
  for (; depth >= 0; depth--) {
    if (sizes[depth] != befores[depth] + inner) return -1;
    inner = sizes[depth];
  }
  return kinds;
}

// Learns the constructors from samples. elems[0] is a list of four values
// made as  x <| (empty |> a |> b |> c),  so it has a prefix holding x, a
// suffix of three and no middle; elems[1] is x (a Val); elems[2] is the empty
// list; elems[3] is a list of a thousand values added one at a time at the
// back, which has both kinds of node. info[] holds the info pointers of
// Foreign, Val, Data1 and Data2, the pointer tags of Data1 and Data2, and
// ListC's info pointer and pointer tag. Returns 1 if everything is as this
// file assumes, else a number saying which check failed.
int64_t unison_jit_list_init(void **raw, int64_t *info) {
  void *elems[4] = {unison_jit_settle(raw[0]), unison_jit_settle(raw[1]), unison_jit_settle(raw[2]), unison_jit_settle(raw[3])};
  memset(&LF, 0, sizeof LF);
  LF.foreign_info = info[0];
  LF.val_info = info[1];
  LF.data1_info = info[2];
  LF.data2_info = info[3];
  LF.data1_tag = info[4];
  LF.data2_tag = info[5];
  LF.list_info = info[6];
  LF.list_tag = info[7];
  StgClosure *f = elems[0];
  if (LTAG(f) != (int)LF.list_tag || (StgWord)LUN(f)->header.info != LF.list_info) return 2;
  const StgInfoTable *wit = get_itbl(LUN(f));
  if (!(wit->type >= CONSTR && wit->type <= CONSTR_NOCAF) || wit->layout.payload.ptrs != 1 ||
      wit->layout.payload.nptrs != 0)
    return 3;
  StgClosure *dq = LP(f, 0);
  if (!con_is(dq, 2, 3, 1)) return 4;
  LF.deep_info = (StgWord)LUN(dq)->header.info;
  if (LW(dq, 3) != MK(4, 1, 3)) return 5;
  StgClosure *pr = LP(dq, 0), *m = LP(dq, 1), *sf = LP(dq, 2);
  if (!con_is(pr, 2, 2, 0)) return 6;
  if (LUN(LP(pr, 0)) != LUN(elems[1])) return 10;
  if (!con_nullary(LP(pr, 1))) return 11;
  LF.scons_info = (StgWord)LUN(pr)->header.info;
  LF.snil = LP(pr, 1);
  if (check_slist(sf) != 3 || !con_nullary(m)) return 7;
  LF.mnil = m;
  StgClosure *e = deque_of(elems[2]);
  if (e == NULL || !con_nullary(e) || LUN(e) == LUN(m)) return 8;
  LF.nil = e;
  // a Val is a pointer and a word, tag 1
  if (!con_is(elems[1], 1, 1, 1) || (StgWord)LUN(elems[1])->header.info != LF.val_info) return 9;
  // the big sample: a middle whose digits hold nodes of eight leaves, and
  // under it a middle whose digits hold nodes with arrays
  StgClosure *big = deque_of(elems[3]);
  if (big == NULL || !con_is(big, 2, 3, 1) || TSIZE(LW(big, 3)) != 1000) return 12;
  StgClosure *m1 = LP(big, 1);
  if (!con_is(m1, 2, 3, 2)) return 13;
  LF.mdeep_info = (StgWord)LUN(m1)->header.info;
  if (!con_is(LP(m1, 0), 2, 2, 0)) return 14;
  StgClosure *n8 = HEAD(LP(m1, 0));
  if (!con_is(n8, 1, 8, 0)) return 15;
  LF.n8_info = (StgWord)LUN(n8)->header.info;
  StgClosure *m2 = LP(m1, 1);
  if (!con_is(m2, 2, 3, 2) || (StgWord)LUN(m2)->header.info != LF.mdeep_info || !con_is(LP(m2, 0), 2, 2, 0))
    return 16;
  StgClosure *na = HEAD(LP(m2, 0));
  if (!con_is(na, 2, 1, 1) || !is_small_array(LP(na, 0))) return 17;
  LF.na_info = (StgWord)LUN(na)->header.info;
  return 1;
}

// For the startup self-test: runs one helper on elems[0] with the other
// arguments in elems[1] (a closure), arg and arg2, and puts the result in
// elems[3]. Returns 0 if the helper didn't handle the case.
//   op 0: view left, 1: view right (elems[1] the empty view, arg the tag)
//   2: cons, 3: snoc (elems[1] the boxed half of the value, arg the unboxed half)
//   4: index arg (elems[1] None, arg2 the tag)
//   5: take arg, 6: drop arg
//   7: split left at arg, 8: split right (elems[1] the empty view, arg2 the tag)
//   9: append elems[0] and elems[1]
//   10: the literal of arg copies of the value (arg2, elems[1])
// (The context is a temporary one: the thread's own is made on its first real
// entry, after the settings are in place.)
int64_t unison_jit_list_test(void **elems, int64_t op, int64_t arg, int64_t arg2) {
  UnisonJitCtx tmp = {0}, *ctx = &tmp;
  ctx->cap = rts_unsafeGetMyCapability();
  void *res = NULL;
  void *e0 = unison_jit_settle(elems[0]), *e1 = unison_jit_settle(elems[1]);
  switch (op) {
    case 0:
    case 1:
      res = unison_jit_list_view(ctx, e0, e1, arg, op == 0);
      break;
    case 2:
    case 3:
      res = unison_jit_list_push(ctx, e0, arg, e1, op == 2);
      break;
    case 4:
      res = unison_jit_list_index(ctx, e0, arg, e1, arg2);
      break;
    case 5:
    case 6:
      res = unison_jit_list_cut(ctx, e0, arg, op == 5);
      break;
    case 7:
    case 8:
      res = unison_jit_list_split(ctx, e0, arg, e1, arg2, op == 7);
      break;
    case 9:
      res = unison_jit_list_append(ctx, e0, e1);
      break;
    case 10: {
      void *acc = NULL;
      for (int64_t i = 0; i < arg; i++) acc = unison_jit_list_lit(ctx, acc, arg2 + i, e1);
      res = unison_jit_list_wrap(ctx, acc);
      break;
    }
  }
  if (res == NULL) return 0;
  elems[3] = res;
  unison_jit_mark_bstk(elems, 3, 3);
  return 1;
}

// ---------------------------------------------------------------------------
// Ropes: Text and Bytes
//
// A Unison Text is a Unison.Util.Rope of chunks, held as TextC rope (one
// closure), and a Bytes is the same rope over chunks of bytes, held as
// BytesC rope. The rope is the list's finger tree with chunks for
// elements and sizes counted in characters (bytes, for a Bytes): its middle
// is the Deque's own Mid, so the functions above that take top = 0 work on it
// as they are, and only the top level, whose items are chunks, is written
// here. These helpers are ports of the Haskell operations in
// lib/unison-util-rope's Rope.hs and the Chunk instances in Unison.Util.Text
// and Unison.Util.Bytes, and take every case.
//
// The two kinds of rope differ only in the chunk: its constructor, where its
// fields are, and whether a count of elements has to be turned into a count
// of bytes through UTF-8. A RopeKind holds that, and every function here
// takes one.
//
//   Rope: Empty (tag 1), One chunk (tag 2),
//         Deep t ps pr m sf (tag 3): pointers pr, m, sf; words t, ps
//   Text chunk: Chunk count (Text array offset length), unpacked: the pointer
//         to the byte array, then the character count, byte offset and byte
//         length
//   Bytes chunk: Chunk offset size array, unpacked: the pointer to the byte
//         array, then the offset and the size
//
// t and ps are as in an MDeep, with elements for leaves. The rope's
// invariants: no chunk is empty; a rope of one chunk is One; both digits of a
// Deep have a chunk; two chunks next to each other have more than the
// threshold elements between them.
//
// Checked at startup like the list layouts (unison_jit_text_init,
// unison_jit_bytes_init).


#define ROPE_THRESHOLD (K->threshold)

// the fields of a Deep (which lv_read doesn't take: its tag isn't a list level's)
static inline void rope_read(StgClosure *c, Lv *v) {
  v->pr = LP(c, 0), v->m = LP(c, 1), v->sf = LP(c, 2);
  v->t = LW(c, 3), v->ps = LW(c, 4);
}

// the elements under a list of chunks
static inline StgInt s_sum_chunks(const RopeKind *K, StgClosure *l) {
  StgInt s = 0;
  for (; IS_CONS(l); l = TAIL(l)) s += chunk_size(K, HEAD(l));
  return s;
}

// --- building ---

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

static inline StgClosure *rope_deep(UnisonJitCtx *ctx, const RopeKind *K, StgInt t, StgInt ps, StgClosure *pr,
                                    StgClosure *m, StgClosure *sf) {
  StgWord *p = list_alloc(ctx, 6);
  p[0] = K->deep_info;
  p[1] = (StgWord)pr;
  p[2] = (StgWord)m;
  p[3] = (StgWord)sf;
  p[4] = (StgWord)t;
  p[5] = (StgWord)ps;
  return (StgClosure *)((StgWord)p | 3);
}

// a fresh byte array of n bytes
static inline StgArrBytes *new_arr(UnisonJitCtx *ctx, StgWord n) {
  StgArrBytes *arr = (StgArrBytes *)list_alloc(ctx, sizeofW(StgArrBytes) + ROUNDUP_BYTES_TO_WDS(n));
  SET_INFO((StgClosure *)arr, &stg_ARR_WORDS_info);
  arr->bytes = n;
  return arr;
}

// Slack (Unison.Util.Bytes's Chunk, and the same in Unison.Util.Text): an array
// chunk_join allocates is larger than its contents, word 0 holds its used byte
// length, the contents start at byte 8 and the chunk's cap is the array's size.
// A chunk owns the array's tail when its end is the used length; the append
// advances the used length by compare-and-swap, so of two appends to the same
// chunk (from C or from Haskell, which does the same) the first writes in place
// and the second copies. The first allocation has ROPE_SLACK_START payload
// bytes, each copy quadruples, up to ROPE_SLACK_MAX; a join of more than
// ROPE_SLACK_MAX bytes gets an exact array with no slack. The two numbers are
// Haskell's slackStart and slackMax, handed to rope_init.
#define ROPE_SLACK_START (K->slack_start)
#define ROPE_SLACK_MAX (K->slack_max)

// the capacity for a join of `need` bytes onto a chunk of capacity `cap`, or 0
static inline StgInt slack_capacity(const RopeKind *K, StgInt cap, StgInt need) {
  if (need > ROPE_SLACK_MAX) return 0;
  StgInt c = cap > 0 ? (cap - 8) * 4 : ROPE_SLACK_START;
  for (;;) {
    StgInt c1 = c < ROPE_SLACK_MAX ? c : ROPE_SLACK_MAX;
    if (c1 >= need) return 8 + c1;
    c *= 4;
  }
}

// the two chunks' elements as one chunk: in place when `a` owns its array's
// tail and has room, else in a fresh byte array (with slack if small)
static StgClosure *chunk_join(UnisonJitCtx *ctx, const RopeKind *K, StgClosure *a, StgClosure *b) {
  StgInt la = chunk_len(K, a), lb = chunk_len(K, b), cap = chunk_cap(K, a), off = chunk_off(K, a);
  StgInt count = chunk_size(K, a) + chunk_size(K, b), end = off + la;
  if (cap > 0 && end + lb <= cap) {
    StgArrBytes *arr = (StgArrBytes *)LP(a, 0);
    StgInt *used = (StgInt *)arr->payload, expected = end;
    if (*used == end && __atomic_compare_exchange_n(used, &expected, end + lb, 0, __ATOMIC_SEQ_CST, __ATOMIC_SEQ_CST)) {
      memcpy((char *)arr->payload + end, chunk_bytes(K, b), lb);
      return new_chunk_cap(ctx, K, arr, count, off, la + lb, cap);
    }
  }
  StgInt need = la + lb, ncap = slack_capacity(K, cap, need);
  if (ncap == 0) {
    StgArrBytes *arr = new_arr(ctx, need);
    memcpy(arr->payload, chunk_bytes(K, a), la);
    memcpy((char *)arr->payload + la, chunk_bytes(K, b), lb);
    return new_chunk(ctx, K, arr, count, 0, need);
  }
  StgArrBytes *arr = new_arr(ctx, ncap);
  ((StgInt *)arr->payload)[0] = 8 + need;
  memcpy((char *)arr->payload + 8, chunk_bytes(K, a), la);
  memcpy((char *)arr->payload + 8 + la, chunk_bytes(K, b), lb);
  return new_chunk_cap(ctx, K, arr, count, 8, need, ncap);
}

// the number of bytes the first k characters of UTF-8 text take
static inline StgInt utf8_prefix(const unsigned char *p, StgInt k) {
  const unsigned char *q = p;
  while (k-- > 0) q += *q < 0x80 ? 1 : *q < 0xE0 ? 2 : *q < 0xF0 ? 3 : 4;
  return q - p;
}

// the number of bytes the first k elements at p take
static inline StgInt elem_bytes(const RopeKind *K, const unsigned char *p, StgInt k) {
  return K->utf8 ? utf8_prefix(p, k) : k;
}

// the first k elements of a chunk, 0 < k < its size; and the rest
static inline StgClosure *chunk_take(UnisonJitCtx *ctx, const RopeKind *K, StgClosure *c, StgInt k) {
  return new_chunk(ctx, K, LP(c, 0), k, chunk_off(K, c), elem_bytes(K, chunk_bytes(K, c), k));
}

static inline StgClosure *chunk_drop(UnisonJitCtx *ctx, const RopeKind *K, StgClosure *c, StgInt k) {
  StgInt nb = elem_bytes(K, chunk_bytes(K, c), k);
  return new_chunk(ctx, K, LP(c, 0), chunk_size(K, c) - k, chunk_off(K, c) + nb, chunk_len(K, c) - nb);
}

// cnt chunks in order, at most a digit's worth, holding n elements (fromFwd)
static StgClosure *rope_from_fwd(UnisonJitCtx *ctx, const RopeKind *K, StgInt n, StgInt cnt, StgClosure *l) {
  if (cnt == 0) return K->empty;
  if (cnt == 1) return rope_one(ctx, K, HEAD(l));
  StgInt p = (cnt + 1) / 2;
  StgClosure *pr = s_take(ctx, p, l);
  return rope_deep(ctx, K, MK(n, p, cnt - p), s_sum_chunks(K, pr), pr, LF.mnil,
                   s_rev_onto(ctx, s_drop(p, l), LF.snil));
}

// ... back to front (fromBwd)
static StgClosure *rope_from_bwd(UnisonJitCtx *ctx, const RopeKind *K, StgInt n, StgInt cnt, StgClosure *l) {
  if (cnt == 0) return K->empty;
  if (cnt == 1) return rope_one(ctx, K, HEAD(l));
  StgInt q = cnt / 2;
  StgClosure *pr = s_rev_onto(ctx, s_drop(q, l), LF.snil);
  return rope_deep(ctx, K, MK(n, cnt - q, q), s_sum_chunks(K, pr), pr, LF.mnil, s_take(ctx, q, l));
}

// A rope of n elements from the parts of a Deep, either of whose digits may
// be empty (build): an empty digit takes a node from the middle, or half of
// the other digit when there is no middle.
static StgClosure *rope_build(UnisonJitCtx *ctx, const RopeKind *K, StgInt n, StgInt pc, StgInt ps, StgClosure *pr,
                              StgClosure *m, StgInt sc, StgClosure *sf) {
  if (pc == 0) {
    if (LTAG(m) != 2) return rope_from_bwd(ctx, K, n, sc, sf);
    StgClosure *nd;
    m = lv_uncons(ctx, 0, m, &nd);
    pc = node_arity(nd), ps = node_size(nd), pr = kids_fwd(ctx, nd, 0);
  }
  if (sc == 0) {
    if (LTAG(m) != 2) return rope_from_fwd(ctx, K, n, pc, pr);
    StgClosure *nd;
    m = lv_unsnoc(ctx, 0, m, &nd);
    sc = node_arity(nd), sf = kids_rev(ctx, nd, sc);
  }
  return rope_deep(ctx, K, MK(n, pc, sc), ps, pr, m, sf);
}

// --- adding a chunk at an end (cons', snoc') ---

// a chunk c of s elements in front of a rope
static StgClosure *rope_cons(UnisonJitCtx *ctx, const RopeKind *K, StgInt s, StgClosure *c, StgClosure *r) {
  switch (LTAG(r)) {
    case 1: return rope_one(ctx, K, c);
    case 2: {
      StgClosure *a = LP(r, 0);
      StgInt sa = chunk_size(K, a);
      if (s + sa <= ROPE_THRESHOLD) return rope_one(ctx, K, chunk_join(ctx, K, c, a));
      return rope_deep(ctx, K, MK(s + sa, 1, 1), s, scons(ctx, c, LF.snil), LF.mnil, scons(ctx, a, LF.snil));
    }
  }
  Lv v;
  rope_read(r, &v);
  StgClosure *f = HEAD(v.pr);
  if (s + chunk_size(K, f) <= ROPE_THRESHOLD)
    return rope_deep(ctx, K, v.t + (s << 8), v.ps + s, scons(ctx, chunk_join(ctx, K, c, f), TAIL(v.pr)), v.m, v.sf);
  if (TPC(v.t) < MAXD) return rope_deep(ctx, K, v.t + (s << 8) + 1, v.ps + s, scons(ctx, c, v.pr), v.m, v.sf);
  // a full prefix keeps its two outermost chunks and sheds the other eight
  StgClosure *k[MAXD];
  s_items(v.pr, k);
  StgInt keep = chunk_size(K, k[0]) + chunk_size(K, k[1]);
  StgClosure *m = lv_cons(ctx, 0, mk_na(ctx, v.ps - keep, k + 2, 8), v.m);
  return rope_deep(ctx, K, v.t + (s << 8) - 7, s + keep, scons(ctx, c, s_from(ctx, k, 2, LF.snil)), m, v.sf);
}

// ... or behind it
static StgClosure *rope_snoc(UnisonJitCtx *ctx, const RopeKind *K, StgClosure *r, StgInt s, StgClosure *c) {
  switch (LTAG(r)) {
    case 1: return rope_one(ctx, K, c);
    case 2: {
      StgClosure *a = LP(r, 0);
      StgInt sa = chunk_size(K, a);
      if (sa + s <= ROPE_THRESHOLD) return rope_one(ctx, K, chunk_join(ctx, K, a, c));
      return rope_deep(ctx, K, MK(sa + s, 1, 1), sa, scons(ctx, a, LF.snil), LF.mnil, scons(ctx, c, LF.snil));
    }
  }
  Lv v;
  rope_read(r, &v);
  StgClosure *l = HEAD(v.sf);
  if (chunk_size(K, l) + s <= ROPE_THRESHOLD)
    return rope_deep(ctx, K, v.t + (s << 8), v.ps, v.pr, v.m, scons(ctx, chunk_join(ctx, K, l, c), TAIL(v.sf)));
  if (TSC(v.t) < MAXD) return rope_deep(ctx, K, v.t + (s << 8) + 0x10, v.ps, v.pr, v.m, scons(ctx, c, v.sf));
  // the suffix runs back to front: s1, s2, then the eight to shed, last first
  StgClosure *k[MAXD], *shed[8];
  s_items(v.sf, k);
  for (int i = 0; i < 8; i++) shed[i] = k[9 - i];
  StgInt sz = TSIZE(v.t) - v.ps - mid_size(v.m) - chunk_size(K, k[0]) - chunk_size(K, k[1]);
  StgClosure *m = lv_snoc(ctx, 0, v.m, mk_na(ctx, sz, shed, 8));
  return rope_deep(ctx, K, v.t + (s << 8) - 0x70, v.ps, v.pr, m, scons(ctx, c, s_from(ctx, k, 2, LF.snil)));
}

// --- take and drop (takeR, dropR) ---
//
// The rope is cut between two chunks, by the list's code when the cut is in
// the middle, and then the part of the chunk the cut falls in is added back
// with snoc or cons, which joins it to its neighbour if the two are small.

static StgClosure *rope_take(UnisonJitCtx *ctx, const RopeKind *K, StgInt i, StgClosure *r) {
  switch (LTAG(r)) {
    case 1: return r;
    case 2: {
      StgClosure *c = LP(r, 0);
      if (i <= 0) return K->empty;
      if (i >= chunk_size(K, c)) return r;
      return rope_one(ctx, K, chunk_take(ctx, K, c, i));
    }
  }
  Lv v;
  rope_read(r, &v);
  StgInt n = TSIZE(v.t), pc = TPC(v.t), sc = TSC(v.t), ms = mid_size(v.m);
  if (i <= 0) return K->empty;
  if (i >= n) return r;
  if (i <= v.ps) {
    StgInt q = 0, sb = 0; // q whole chunks of the prefix, with sb elements, come before the cut
    for (StgClosure *l = v.pr;; l = TAIL(l), q++) {
      StgClosure *c = HEAD(l);
      StgInt s = chunk_size(K, c);
      if (sb + s < i) {
        sb += s;
        continue;
      }
      if (sb + s == i) return rope_from_fwd(ctx, K, i, q + 1, s_take(ctx, q + 1, v.pr));
      StgClosure *whole = rope_from_fwd(ctx, K, sb, q, s_take(ctx, q, v.pr));
      return rope_snoc(ctx, K, whole, i - sb, chunk_take(ctx, K, c, i - sb));
    }
  }
  if (i <= v.ps + ms) {
    StgClosure *nd;
    StgInt k;
    StgClosure *m2 = mid_take(ctx, v.m, i - v.ps, &nd, &k);
    // the first k elements of the node are kept
    StgClosure **kids = node_kids(nd);
    StgInt q = 0, sb = 0;
    for (;; q++) {
      StgClosure *c = kids[q];
      StgInt s = chunk_size(K, c);
      if (sb + s < k) {
        sb += s;
        continue;
      }
      if (sb + s == k) return rope_build(ctx, K, i, pc, v.ps, v.pr, m2, q + 1, kids_rev(ctx, nd, q + 1));
      StgClosure *whole = rope_build(ctx, K, i - (k - sb), pc, v.ps, v.pr, m2, q, kids_rev(ctx, nd, q));
      return rope_snoc(ctx, K, whole, k - sb, chunk_take(ctx, K, c, k - sb));
    }
  }
  StgInt d = n - i, cnt = sc; // d elements are to go from the back; cnt chunks of the suffix are left
  for (StgClosure *l = v.sf;; l = TAIL(l), cnt--) {
    StgClosure *c = HEAD(l);
    StgInt s = chunk_size(K, c);
    if (d >= s) {
      d -= s;
      continue;
    }
    if (d == 0) return rope_build(ctx, K, i, pc, v.ps, v.pr, v.m, cnt, l);
    StgClosure *whole = rope_build(ctx, K, i - (s - d), pc, v.ps, v.pr, v.m, cnt - 1, TAIL(l));
    return rope_snoc(ctx, K, whole, s - d, chunk_take(ctx, K, c, s - d));
  }
}

static StgClosure *rope_drop(UnisonJitCtx *ctx, const RopeKind *K, StgInt i, StgClosure *r) {
  switch (LTAG(r)) {
    case 1: return r;
    case 2: {
      StgClosure *c = LP(r, 0);
      if (i <= 0) return r;
      if (i >= chunk_size(K, c)) return K->empty;
      return rope_one(ctx, K, chunk_drop(ctx, K, c, i));
    }
  }
  Lv v;
  rope_read(r, &v);
  StgInt n = TSIZE(v.t), pc = TPC(v.t), sc = TSC(v.t), ms = mid_size(v.m);
  if (i <= 0) return r;
  if (i >= n) return K->empty;
  StgInt left = n - i;
  if (i >= v.ps + ms) {
    StgInt q = 0, sa = 0; // q whole chunks of the suffix, with sa elements, come after the cut
    for (StgClosure *l = v.sf;; l = TAIL(l), q++) {
      StgClosure *c = HEAD(l);
      StgInt s = chunk_size(K, c);
      if (sa + s < left) {
        sa += s;
        continue;
      }
      if (sa + s == left) return rope_from_bwd(ctx, K, left, q + 1, s_take(ctx, q + 1, v.sf));
      StgClosure *whole = rope_from_bwd(ctx, K, sa, q, s_take(ctx, q, v.sf));
      return rope_cons(ctx, K, left - sa, chunk_drop(ctx, K, c, s - (left - sa)), whole);
    }
  }
  if (i >= v.ps) {
    StgClosure *nd;
    StgInt k;
    StgClosure *m2 = mid_drop(ctx, v.m, i - v.ps, &nd, &k);
    // the first k elements of the node go
    StgClosure **kids = node_kids(nd);
    StgInt arity = node_arity(nd), q = 0, sb = 0;
    for (;; q++) {
      StgClosure *c = kids[q];
      StgInt s = chunk_size(K, c);
      if (k >= sb + s) {
        sb += s;
        continue;
      }
      if (k == sb) return rope_build(ctx, K, left, arity - q, node_size(nd) - sb, kids_fwd(ctx, nd, q), m2, sc, v.sf);
      StgClosure *whole = rope_build(ctx, K, left - (sb + s - k), arity - q - 1, node_size(nd) - sb - s,
                                     kids_fwd(ctx, nd, q + 1), m2, sc, v.sf);
      return rope_cons(ctx, K, sb + s - k, chunk_drop(ctx, K, c, k - sb), whole);
    }
  }
  // k elements still to drop; the prefix has cnt chunks and psz elements left
  StgInt k = i, cnt = pc, psz = v.ps;
  for (StgClosure *l = v.pr;; l = TAIL(l)) {
    StgClosure *c = HEAD(l);
    StgInt s = chunk_size(K, c);
    if (k >= s) {
      k -= s, cnt--, psz -= s;
      continue;
    }
    if (k == 0) return rope_build(ctx, K, left, cnt, psz, l, v.m, sc, v.sf);
    StgClosure *whole = rope_build(ctx, K, left - (s - k), cnt - 1, psz - s, TAIL(l), v.m, sc, v.sf);
    return rope_cons(ctx, K, s - k, chunk_drop(ctx, K, c, k), whole);
  }
}

// --- append ---

// If the two chunks that meet are small they are joined, as the last chunk of
// the left side. Then the digits that end up inside join the outer digit of a
// side that has no middle, if they fit there (the shorter side's, if they fit
// in either: that copies fewer cells), or are packed into nodes and handed to
// the list's append of two middles.
static StgClosure *rope_append(UnisonJitCtx *ctx, const RopeKind *K, StgClosure *a, StgClosure *b) {
  for (;;) {
    if (LTAG(a) == 1) return b;
    if (LTAG(b) == 1) return a;
    if (LTAG(a) == 2) return rope_cons(ctx, K, rope_size(K, a), LP(a, 0), b);
    if (LTAG(b) == 2) return rope_snoc(ctx, K, a, rope_size(K, b), LP(b, 0));
    Lv x, y;
    rope_read(a, &x);
    rope_read(b, &y);
    StgInt pc1 = TPC(x.t), sc1 = TSC(x.t), pc2 = TPC(y.t), sc2 = TSC(y.t);
    StgInt n = TSIZE(x.t) + TSIZE(y.t);
    // isf: the left side's suffix; ipr: the ipc chunks of the right side's
    // prefix that are still its own (d elements of it went to the left)
    StgClosure *isf = x.sf, *ipr = y.pr;
    StgInt ipc = pc2, d = 0;
    StgClosure *l = HEAD(x.sf), *f = HEAD(y.pr);
    if (chunk_size(K, l) + chunk_size(K, f) <= ROPE_THRESHOLD) {
      d = chunk_size(K, f);
      isf = scons(ctx, chunk_join(ctx, K, l, f), TAIL(x.sf));
      ipr = TAIL(y.pr), ipc = pc2 - 1;
    }
    StgInt c = sc1 + ipc;
    int flat1 = LTAG(x.m) != 2, flat2 = LTAG(y.m) != 2;
    if (flat1 && pc1 + c <= MAXD && !(flat2 && c + sc2 <= MAXD && sc2 + ipc < pc1 + sc1))
      return rope_deep(ctx, K, MK(n, pc1 + c, sc2), TSIZE(x.t) + y.ps, s_append(ctx, x.pr, s_rev_onto(ctx, isf, ipr)),
                       y.m, y.sf);
    if (flat2 && c + sc2 <= MAXD)
      return rope_deep(ctx, K, MK(n, pc1, c + sc2), x.ps, x.pr, x.m, s_append(ctx, y.sf, s_rev_onto(ctx, ipr, isf)));
    if (c < 2) {
      // a single chunk between two sides that can't take it: the right side
      // gets a prefix again, from its middle or its suffix
      a = rope_deep(ctx, K, x.t + (d << 8), x.ps, x.pr, x.m, isf);
      b = rope_build(ctx, K, TSIZE(y.t) - d, 0, 0, LF.snil, y.m, sc2, y.sf);
      continue;
    }
    // the left suffix (back to front) and the right prefix, as nodes of eight
    // while that leaves none or at least two for the next (packChunks)
    StgClosure *items[2 * MAXD], *out[3];
    StgInt at = sc1, no = 0;
    for (StgClosure *p = isf; IS_CONS(p); p = TAIL(p)) items[--at] = HEAD(p);
    s_items(ipr, items + sc1);
    for (StgInt done = 0; done < c;) {
      StgInt left = c - done, k = left <= 8 ? left : left == 9 ? 5 : 8, sz = 0;
      for (StgInt j = 0; j < k; j++) sz += chunk_size(K, items[done + j]);
      out[no++] = mk_na(ctx, sz, items + done, k);
      done += k;
    }
    StgInt sns = (TSIZE(x.t) - x.ps - mid_size(x.m)) + y.ps;
    StgClosure *m = lv_append(ctx, 0, x.m, sns, out, no, y.m);
    return rope_deep(ctx, K, MK(n, pc1, sc2), x.ps, x.pr, m, y.sf);
  }
}

// --- finding the chunk that holds a character (chunkAt) ---

// The chunk holding character i, 0 <= i < size, and i's offset in it.
StgClosure *unison_jit_rope_chunk_at(const RopeKind *K, StgClosure *r, StgInt i, StgInt *off) {
  if (LTAG(r) == 2) {
    *off = i;
    return LP(r, 0);
  }
  Lv v;
  rope_read(r, &v);
  if (i < v.ps) {
    for (StgClosure *l = v.pr;; l = TAIL(l)) {
      StgInt s = chunk_size(K, HEAD(l));
      if (i < s) {
        *off = i;
        return HEAD(l);
      }
      i -= s;
    }
  }
  StgInt i2 = i - v.ps;
  if (i2 < mid_size(v.m)) {
    StgInt o;
    StgClosure *nd = mid_look(v.m, -1, i2, &o);
    for (StgClosure **kids = node_kids(nd);; kids++) {
      StgInt s = chunk_size(K, *kids);
      if (o < s) {
        *off = o;
        return *kids;
      }
      o -= s;
    }
  }
  StgInt back = TSIZE(v.t) - 1 - i; // elements after the one looked for
  for (StgClosure *l = v.sf;; l = TAIL(l)) {
    StgInt s = chunk_size(K, HEAD(l));
    if (back < s) {
      *off = s - 1 - back;
      return HEAD(l);
    }
    back -= s;
  }
}


// TextC rope or BytesC rope; `same` is returned if it already holds that
// rope
static void *wrap_rope(UnisonJitCtx *ctx, const RopeKind *K, StgClosure *rope, void *same) {
  if (same != NULL && rope_of(K, same) == rope) return same;
  StgWord *p = list_alloc(ctx, 2);
  p[0] = K->wrap_info;
  p[1] = (StgWord)rope;
  return (void *)((StgWord)p | K->wrap_tag);
}

// --- the operations behind the helpers generated code calls ---
//
// Each takes the closures as generated code has them, and answers "not
// handled" (NULL, or -1) for a closure that isn't a rope of the kind.

// Text.size or Bytes.size, or -1
static inline int64_t rope_h_size(const RopeKind *K, void *x) {
  StgClosure *r = rope_of(K, x);
  return r == NULL ? -1 : rope_size(K, r);
}

// Text.++ or Bytes.++, or NULL
static inline void *rope_h_append(UnisonJitCtx *ctx, const RopeKind *K, void *x, void *y) {
  StgClosure *a = rope_of(K, x), *b = rope_of(K, y);
  if (a == NULL || b == NULL) return NULL;
  if (LTAG(a) == 1) return y;
  if (LTAG(b) == 1) return x;
  return wrap_rope(ctx, K, rope_append(ctx, K, a, b), NULL);
}

// take (take != 0) or drop of n elements. n comes from a Nat: a negative one
// is a count too large to be a size, and takes everything or drops
// everything, as in the interpreter.
static inline void *rope_h_cut(UnisonJitCtx *ctx, const RopeKind *K, void *x, int64_t n, int64_t take) {
  StgClosure *r = rope_of(K, x);
  if (r == NULL) return NULL;
  if (take) return n < 0 ? x : wrap_rope(ctx, K, rope_take(ctx, K, n, r), x);
  return wrap_rope(ctx, K, n < 0 ? K->empty : rope_drop(ctx, K, n, r), x);
}

// Equality: 1 or 0, or -1 if a closure isn't a rope of the kind. Two ropes
// are equal when they have the same elements, however they are cut into
// chunks; UTF-8 makes that the same bytes for texts too. Each side is walked
// a chunk at a time, finding the next chunk by its position.
static int64_t rope_h_eq(const RopeKind *K, void *x, void *y) {
  StgClosure *a = rope_of(K, x), *b = rope_of(K, y);
  if (a == NULL || b == NULL) return -1;
  if (a == b) return 1;
  StgInt n = rope_size(K, a);
  if (n != rope_size(K, b)) return 0;
  if (LTAG(a) == 2 && LTAG(b) == 2) {
    StgClosure *ca = LP(a, 0), *cb = LP(b, 0);
    StgInt la = chunk_len(K, ca);
    return la == chunk_len(K, cb) && memcmp(chunk_bytes(K, ca), chunk_bytes(K, cb), la) == 0;
  }
  const unsigned char *pa = NULL, *pb = NULL;
  StgInt na = 0, nb = 0, ia = 0, ib = 0, off; // bytes left in each side's chunk; elements before the next one
  for (;;) {
    if (na == 0 && ia < n) {
      StgClosure *c = unison_jit_rope_chunk_at(K, a, ia, &off);
      pa = chunk_bytes(K, c), na = chunk_len(K, c), ia += chunk_size(K, c);
    }
    if (nb == 0 && ib < n) {
      StgClosure *c = unison_jit_rope_chunk_at(K, b, ib, &off);
      pb = chunk_bytes(K, c), nb = chunk_len(K, c), ib += chunk_size(K, c);
    }
    if (na == 0 || nb == 0) return na == nb;
    StgInt k = na < nb ? na : nb;
    if (memcmp(pa, pb, k) != 0) return 0;
    pa += k, pb += k, na -= k, nb -= k;
  }
}

// --- the helpers generated code calls ---

int64_t unison_jit_text_size(void *text) { return rope_h_size(&TK, text); }
void *unison_jit_text_append(UnisonJitCtx *ctx, void *x, void *y) { return rope_h_append(ctx, &TK, x, y); }
void *unison_jit_text_cut(UnisonJitCtx *ctx, void *text, int64_t n, int64_t take) {
  return rope_h_cut(ctx, &TK, text, n, take);
}
int64_t unison_jit_text_eq(void *x, void *y) { return rope_h_eq(&TK, x, y); }

int64_t unison_jit_bytes_size(void *bytes) { return rope_h_size(&BK, bytes); }
void *unison_jit_bytes_append(UnisonJitCtx *ctx, void *x, void *y) { return rope_h_append(ctx, &BK, x, y); }
void *unison_jit_bytes_cut(UnisonJitCtx *ctx, void *bytes, int64_t n, int64_t take) {
  return rope_h_cut(ctx, &BK, bytes, n, take);
}

// Universal equality (==) on two texts or two bytes: 1 or 0, or -1 for
// anything else, which generated code leaves to the interpreter. `kinds` has
// bit 0 set to take texts and bit 1 to take bytes.
int64_t unison_jit_foreign_eq(void *x, void *y, int64_t kinds) {
  if ((kinds & 1) && rope_of(&TK, x) != NULL) return rope_h_eq(&TK, x, y);
  if ((kinds & 2) && rope_of(&BK, x) != NULL) return rope_h_eq(&BK, x, y);
  return -1;
}

// Bytes.at: Some n or None, as the interpreter's IDXB builds them. `none` is
// the None closure (its first field is the type's Reference) and `nat_tag`
// the type tag closure of a Nat.
void *unison_jit_bytes_index(UnisonJitCtx *ctx, void *bytes, int64_t i, void *none, int64_t some_tag,
                             void *nat_tag) {
  StgClosure *r = rope_of(&BK, bytes);
  if (r == NULL) return NULL;
  if (i < 0 || i >= rope_size(&BK, r)) return none;
  StgInt off;
  StgClosure *c = unison_jit_rope_chunk_at(&BK, r, i, &off);
  // Data1 tag v ref: pointers v's, ref; words tag, v's
  StgWord *p = list_alloc(ctx, 5);
  p[0] = LF.data1_info;
  p[1] = (StgWord)nat_tag;
  p[2] = (StgWord)LP(none, 0);
  p[3] = some_tag;
  p[4] = chunk_bytes(&BK, c)[off];
  return (void *)((StgWord)p | LF.data1_tag);
}

// Bytes.flatten: the same bytes in one chunk. A rope of one chunk, or none,
// is left as it is.
void *unison_jit_bytes_flatten(UnisonJitCtx *ctx, void *bytes) {
  StgClosure *r = rope_of(&BK, bytes);
  if (r == NULL) return NULL;
  if (LTAG(r) != 3) return bytes;
  StgInt n = rope_size(&BK, r), off;
  StgArrBytes *arr = new_arr(ctx, n);
  for (StgInt i = 0; i < n;) {
    StgClosure *c = unison_jit_rope_chunk_at(&BK, r, i, &off);
    StgInt s = chunk_size(&BK, c);
    memcpy((char *)arr->payload + i, chunk_bytes(&BK, c), s);
    i += s;
  }
  return wrap_rope(ctx, &BK, rope_one(ctx, &BK, new_chunk(ctx, &BK, arr, n, 0, n)), NULL);
}

// --- the startup checks ---

// a chunk's element count, or -1 if it isn't laid out as assumed
static StgInt check_chunk(const RopeKind *K, StgClosure *c) {
  if (LTAG(c) != 1 || (StgWord)LUN(c)->header.info != K->chunk_info) return -1;
  StgArrBytes *arr = (StgArrBytes *)LP(c, 0);
  StgInt off = chunk_off(K, c), len = chunk_len(K, c), count = chunk_size(K, c);
  StgInt cap = chunk_cap(K, c);
  if (arr->header.info != &stg_ARR_WORDS_info || off < 0 || len <= 0 || (StgWord)(off + len) > arr->bytes) return -1;
  if (cap != 0 && (cap != (StgInt)arr->bytes || off < 8 || off + len > ((StgInt *)arr->payload)[0])) return -1;
  return elem_bytes(K, chunk_bytes(K, c), count) == len ? count : -1;
}

// the elements under a node `depth` levels down (1: its children are chunks), or -1
static StgInt check_rope_node(const RopeKind *K, StgClosure *nd, int depth) {
  if (LTAG(nd) != 2 || (StgWord)LUN(nd)->header.info != LF.na_info || !is_small_array(LP(nd, 0))) return -1;
  StgInt arity = node_arity(nd), total = 0;
  if (arity < 2 || arity > 8) return -1;
  for (StgInt c = 0; c < arity; c++) {
    StgClosure *kid = node_kids(nd)[c];
    StgInt z = depth == 1 ? check_chunk(K, kid) : check_rope_node(K, kid, depth - 1);
    if (z < 0) return -1;
    total += z;
  }
  return total == LW(nd, 1) ? total : -1;
}

// a digit of a level `depth` down with the given count; its elements, or -1
static StgInt check_rope_digit(const RopeKind *K, StgClosure *l, int depth, StgInt count) {
  if (check_slist(l) != count) return -1;
  StgInt total = 0;
  for (; LTAG(l) == 2; l = LP(l, 1)) {
    StgInt z = depth == 0 ? check_chunk(K, HEAD(l)) : check_rope_node(K, HEAD(l), depth);
    if (z < 0) return -1;
    total += z;
  }
  return total;
}

// Checks a rope's structure: every constructor's shape, the counts and sizes
// at every level, each chunk's element count against its bytes, and the
// rope's invariants. 1 if all is as assumed.
static int64_t rope_check(const RopeKind *K, void **elems) {
  StgClosure *r = rope_of(K, unison_jit_settle(elems[0]));
  if (r == NULL) return 0;
  if (LTAG(r) == 1) return same_con0(r, K->empty);
  if (LTAG(r) == 2) return (StgWord)LUN(r)->header.info == K->one_info && check_chunk(K, LP(r, 0)) > 0;
  StgClosure *c = r;
  StgInt sizes[64], befores[64], inner = 0;
  int depth = 0;
  for (;; depth++) {
    if (depth >= 64) return 0;
    if (depth == 0 ? (StgWord)LUN(c)->header.info != K->deep_info || !con_is(c, 3, 3, 2)
                   : (StgWord)LUN(c)->header.info != LF.mdeep_info || !con_is(c, 2, 3, 2))
      return 0;
    StgInt t = LW(c, 3);
    StgInt a = check_rope_digit(K, LP(c, 0), depth, TPC(t)), b = check_rope_digit(K, LP(c, 2), depth, TSC(t));
    if (a < 0 || b < 0 || TSIZE(t) <= 0 || LW(c, 4) != a) return 0;
    sizes[depth] = TSIZE(t), befores[depth] = a + b;
    StgClosure *m = LP(c, 1);
    // the top level always has a chunk in both digits; a level below, when it has a middle
    if ((depth == 0 || LTAG(m) != 1) && (TPC(t) == 0 || TSC(t) == 0)) return 0;
    if (LTAG(m) == 1) {
      if (!same_con0(m, LF.mnil)) return 0;
      break;
    }
    c = m;
  }
  for (; depth >= 0; depth--) {
    if (sizes[depth] != befores[depth] + inner) return 0;
    inner = sizes[depth];
  }
  // no two chunks next to each other are small enough to be one
  StgInt n = rope_size(K, r), prev = ROPE_THRESHOLD + 1, off;
  for (StgInt i = 0; i < n;) {
    StgClosure *ch = unison_jit_rope_chunk_at(K, r, i, &off);
    StgInt s = chunk_size(K, ch);
    if (off != 0 || prev + s <= ROPE_THRESHOLD) return 0;
    prev = s, i += s;
  }
  return 1;
}

int64_t unison_jit_text_check(void **elems) { return rope_check(&TK, elems); }
int64_t unison_jit_bytes_check(void **elems) { return rope_check(&BK, elems); }

// Learns a kind's constructors from samples: elems[0] is a rope of the three
// elements "abc" in one chunk, elems[1] two pieces of one element more than
// the threshold appended, which is a Deep with one chunk in each digit,
// elems[2] the empty rope. info[0] is the info pointer of the closure
// holding the rope (TextC's or BytesC's), info[1]
// Rope.threshold, info[2] the offset of elems[0]'s chunk in its byte array
// (the Bytes sample is cut from a longer one, so that its offset and its size
// differ and the two fields can't be mixed up) and info[3] that closure's
// pointer tag, info[4] and info[5] the slack a joined chunk starts with and
// grows to (slackStart, slackMax). The constructors of the digits and of the
// levels below are the list's (unison_jit_list_init has run). Returns 1, or
// the number of the check that failed.
static int64_t rope_init(RopeKind *K, void **raw, int64_t *info) {
  void *elems[3] = {unison_jit_settle(raw[0]), unison_jit_settle(raw[1]), unison_jit_settle(raw[2])};
  K->threshold = info[1];
  K->slack_start = info[4];
  K->slack_max = info[5];
  if (K->threshold < 1 || K->slack_start < 1 || K->slack_max < K->slack_start) return 14;
  StgInt piece = K->threshold + 1;
  StgClosure *w = elems[0];
  K->wrap_info = info[0];
  K->wrap_tag = info[3];
  if (LTAG(w) != (int)K->wrap_tag || (StgWord)LUN(w)->header.info != K->wrap_info) return 2;
  const StgInfoTable *wit = get_itbl(LUN(w));
  if (!(wit->type >= CONSTR && wit->type <= CONSTR_NOCAF) || wit->layout.payload.ptrs != 1 ||
      wit->layout.payload.nptrs != 0)
    return 3;
  StgClosure *one = LP(w, 0);
  if (!con_is(one, 2, 1, 0)) return 4;
  K->one_info = (StgWord)LUN(one)->header.info;
  StgClosure *c = LP(one, 0);
  if (!con_is(c, 1, 1, K->chunk_words - 1)) return 5;
  K->chunk_info = (StgWord)LUN(c)->header.info;
  StgArrBytes *arr = (StgArrBytes *)LP(c, 0);
  if (arr->header.info != &stg_ARR_WORDS_info) return 6;
  if (chunk_size(K, c) != 3 || chunk_len(K, c) != 3 || chunk_off(K, c) != info[2] || (StgWord)info[2] + 3 > arr->bytes)
    return 7;
  if (memcmp(chunk_bytes(K, c), "abc", 3) != 0) return 8;
  StgClosure *deep = rope_of(K, elems[1]);
  if (deep == NULL || !con_is(deep, 3, 3, 2)) return 9;
  K->deep_info = (StgWord)LUN(deep)->header.info;
  if (LW(deep, 3) != MK(2 * piece, 1, 1) || LW(deep, 4) != piece) return 10;
  StgClosure *pr = LP(deep, 0), *sf = LP(deep, 2);
  if (!same_con0(LP(deep, 1), LF.mnil) || check_slist(pr) != 1 || check_slist(sf) != 1) return 11;
  if (!con_is(HEAD(pr), 1, 1, K->chunk_words - 1) || chunk_size(K, HEAD(pr)) != piece ||
      chunk_size(K, HEAD(sf)) != piece)
    return 13;
  StgClosure *e = rope_of(K, elems[2]);
  if (e == NULL || !con_nullary(e)) return 12;
  K->empty = e;
  return 1;
}

int64_t unison_jit_text_init(void **raw, int64_t *info) {
  memset(&TK, 0, sizeof TK);
  TK.chunk_words = 5, TK.ix_count = 1, TK.ix_cap = 2, TK.ix_off = 3, TK.ix_len = 4, TK.utf8 = 1;
  return rope_init(&TK, raw, info);
}

int64_t unison_jit_bytes_init(void **raw, int64_t *info) {
  memset(&BK, 0, sizeof BK);
  BK.chunk_words = 4, BK.ix_off = 1, BK.ix_count = 2, BK.ix_len = 2, BK.ix_cap = 3, BK.utf8 = 0;
  return rope_init(&BK, raw, info);
}

// For the startup self-test. The operation `op` on elems[0] (and elems[1]),
// with arg and arg2 its numbers (a count, a tag, or two packed as the C
// below reads them); the result goes in elems[3] and 1 is returned (0 if not
// handled), except for the operations that return a number, which return it.
// The op numbers are those of probeTexts and probeBytes in JIT/Layout.hs.
static int64_t rope_test(const RopeKind *K, void **elems, int64_t op, int64_t arg, int64_t arg2, int64_t arg3) {
  UnisonJitCtx tmp = {0}, *ctx = &tmp;
  static int trace_test = -1;
  if (trace_test < 0) trace_test = getenv("UNISON_JIT_TRACE_TEST") != NULL;
  if (trace_test) fprintf(stderr, "[test] %s op %lld arg %lld %lld %lld\n", K == &TK ? "text" : "bytes", (long long)op, (long long)arg, (long long)arg2, (long long)arg3);
  ctx->cap = rts_unsafeGetMyCapability();
  void *res = NULL;
  void *e0 = unison_jit_settle(elems[0]), *e1 = unison_jit_settle(elems[1]);
  // the constructors the results need: elems[4] None, [5] the Tuple
  // enumeration, [6] (), [7] the Either enumeration, [8..11] the Char, Nat,
  // Int and Float type tags
  void *none = unison_jit_settle(elems[4]), *pair = unison_jit_settle(elems[5]), *unit = unison_jit_settle(elems[6]), *either = unison_jit_settle(elems[7]);
  void *ctag = unison_jit_settle(elems[8]), *ntag = unison_jit_settle(elems[9]), *itag = unison_jit_settle(elems[10]), *ftag = unison_jit_settle(elems[11]);
  if (K == &TK) switch (op) {
      case 0: res = rope_h_append(ctx, K, e0, e1); break;
      case 1: res = rope_h_cut(ctx, K, e0, arg, 1); break;
      case 2: res = rope_h_cut(ctx, K, e0, arg, 0); break;
      case 3: return rope_h_size(K, e0);
      case 4: return rope_h_eq(K, e0, e1);
      case 5:
      case 6: res = unison_jit_text_uncons(ctx, e0, none, arg, pair, arg2, unit, ctag, op == 5); break;
      case 7: res = unison_jit_int_to_text(ctx, arg, 1); break;
      case 8: res = unison_jit_int_to_text(ctx, arg, 0); break;
      case 9: res = unison_jit_float_to_text(ctx, arg); break;
      case 10: res = unison_jit_text_to_num(ctx, e0, none, arg, itag, 0); break;
      case 11: res = unison_jit_text_to_num(ctx, e0, none, arg, ntag, 1); break;
      case 12: res = unison_jit_text_to_num(ctx, e0, none, arg, ftag, 2); break;
      case 13: res = unison_jit_text_pack(ctx, e0, ctag); break;
      case 14: res = unison_jit_text_unpack(ctx, e0, ctag); break;
      case 15: res = unison_jit_text_index_of(ctx, e0, e1, none, arg, ntag); break;
      case 16: return unison_jit_text_cmp(e0, e1);
      case 17: res = unison_jit_text_repeat(ctx, arg, e0); break;
      case 18: res = unison_jit_text_reverse(ctx, e0); break;
      case 19: res = unison_jit_text_case(ctx, e0, 1); break;
      case 20: res = unison_jit_text_case(ctx, e0, 0); break;
      case 21: res = unison_jit_text_to_utf8(ctx, e0); break;
      case 22: res = unison_jit_text_from_utf8(ctx, e0, either, arg); break;
      case 23: res = unison_jit_char_to_text(ctx, arg); break;
    }
  else switch (op) {
      case 0: res = rope_h_append(ctx, K, e0, e1); break;
      case 1: res = rope_h_cut(ctx, K, e0, arg, 1); break;
      case 2: res = rope_h_cut(ctx, K, e0, arg, 0); break;
      case 3: return rope_h_size(K, e0);
      case 4: return rope_h_eq(K, e0, e1);
      case 5: res = unison_jit_bytes_index(ctx, e0, arg, none, arg2, ntag); break;
      case 6: res = unison_jit_bytes_flatten(ctx, e0); break;
      case 7: res = unison_jit_bytes_pack(ctx, e0, ntag); break;
      case 8: res = unison_jit_bytes_unpack(ctx, e0, ntag); break;
      case 9: res = unison_jit_bytes_index_of(ctx, e0, e1, none, arg, ntag); break;
      case 10: return unison_jit_foreign_cmp(e0, e1, 2);
      case 11: res = unison_jit_bytes_decode_nat(ctx, e0, arg >> 1, arg & 1, none, arg2, pair, arg3, unit, ntag); break;
      case 12: res = unison_jit_bytes_encode_nat(ctx, arg, arg2 >> 1, arg2 & 1); break;
      case 13: {
        int64_t ok = unison_jit_bytes_read_ok(e0, arg, arg2 >> 1);
        return ok == 1 ? unison_jit_bytes_read_at(e0, arg, arg2 >> 1, arg2 & 1) : ok == 0 ? -1 : -2;
      }
      case 14: res = unison_jit_bytes_to_base(ctx, e0, arg); break;
      case 15: res = unison_jit_bytes_from_base(ctx, e0, arg, either, arg2); break;
    }
  if (res == NULL) return 0;
  elems[3] = res;
  unison_jit_mark_bstk(elems, 3, 3);
  return 1;
}

int64_t unison_jit_text_test(void **elems, int64_t op, int64_t arg, int64_t arg2, int64_t arg3) {
  return rope_test(&TK, elems, op, arg, arg2, arg3);
}
int64_t unison_jit_bytes_test(void **elems, int64_t op, int64_t arg, int64_t arg2, int64_t arg3) {
  return rope_test(&BK, elems, op, arg, arg2, arg3);
}

// ---------------------------------------------------------------------------
// The rest of Text and Bytes
//
// Conversions, searches, orderings and the foreign functions, as ports of
// the interpreter's primitives (Machine/Primops.hs) and foreign functions
// (Foreign/Function.hs) over the Haskell operations in Unison.Util.Text and
// Unison.Util.Bytes. Each helper takes every case it can decide exactly and
// answers "not handled" (NULL) for the rest, which generated code leaves to
// the interpreter: a text that isn't one, an element of the wrong type, a
// number written in a form the Haskell lexer might read differently, an
// encoding that isn't canonical, invalid UTF-8 (the interpreter builds the
// Failure). The results are built as the interpreter builds them, so that a
// program can't tell which side made a value:
//
//   Some v        Data1 ref tag v        (ref from the pooled None)
//   (a, b)        Data2 ref tag a (Data2 ref tag b ())   (ref from the pooled Tuple)
//   Right v       Data1 ref tag v        (ref from the pooled Either)
//   a Val         a pointer, then a word; a boxed value's word is -1
//   a new text    chunks of at most the threshold's characters over one
//                 array, as Text.fromText cuts them

// --- results ---

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

// Some v; `none` is the pooled None, whose first field is Optional's reference
static inline void *mk_some(UnisonJitCtx *ctx, void *none, StgInt some_tag, void *b, StgInt u) {
  return mk_data1(ctx, LP(none, 0), some_tag, b, u);
}

// the pair (a, b): Tuple a (Tuple b ()); `pair` is the pooled Tuple
// enumeration (its first field the reference) and `unit` the pooled ()
static inline void *mk_pair(UnisonJitCtx *ctx, void *pair, StgInt pair_tag, void *unit, void *b1, StgInt u1, void *b2,
                            StgInt u2) {
  StgClosure *ref = LP(pair, 0);
  void *inner = mk_data2(ctx, ref, pair_tag, b2, u2, unit, -1);
  return mk_data2(ctx, ref, pair_tag, b1, u1, inner, -1);
}

// a Val closure (a list element)
static inline StgClosure *mk_val(UnisonJitCtx *ctx, void *b, StgInt u) {
  StgWord *p = list_alloc(ctx, 3);
  p[0] = LF.val_info;
  p[1] = (StgWord)b;
  p[2] = u;
  return (StgClosure *)((StgWord)p | 1);
}

// --- UTF-8 ---

static inline StgInt utf8_len(unsigned char b) { return b < 0x80 ? 1 : b < 0xE0 ? 2 : b < 0xF0 ? 3 : 4; }
static inline StgInt utf8_cp_len(StgInt c) { return c < 0x80 ? 1 : c < 0x800 ? 2 : c < 0x10000 ? 3 : 4; }

// the code point at p, and its byte length (p is valid UTF-8)
static inline StgInt utf8_decode(const unsigned char *p, StgInt *n) {
  unsigned char b = p[0];
  if (b < 0x80) return *n = 1, b;
  if (b < 0xE0) return *n = 2, ((b & 0x1F) << 6) | (p[1] & 0x3F);
  if (b < 0xF0) return *n = 3, ((b & 0x0F) << 12) | ((p[1] & 0x3F) << 6) | (p[2] & 0x3F);
  return *n = 4, ((b & 0x07) << 18) | ((p[1] & 0x3F) << 12) | ((p[2] & 0x3F) << 6) | (p[3] & 0x3F);
}

// the start of the last character of p[0..len)
static inline StgInt utf8_last_start(const unsigned char *p, StgInt len) {
  StgInt i = len - 1;
  while (i > 0 && (p[i] & 0xC0) == 0x80) i--;
  return i;
}

static inline StgInt utf8_encode(StgInt c, unsigned char *o) {
  if (c < 0x80) return o[0] = c, 1;
  if (c < 0x800) return o[0] = 0xC0 | (c >> 6), o[1] = 0x80 | (c & 0x3F), 2;
  if (c < 0x10000) return o[0] = 0xE0 | (c >> 12), o[1] = 0x80 | ((c >> 6) & 0x3F), o[2] = 0x80 | (c & 0x3F), 3;
  return o[0] = 0xF0 | (c >> 18), o[1] = 0x80 | ((c >> 12) & 0x3F), o[2] = 0x80 | ((c >> 6) & 0x3F),
         o[3] = 0x80 | (c & 0x3F), 4;
}

// The number of characters in p[0..len) if it is valid UTF-8 as
// Data.Text.decodeUtf8' sees it (no overlong forms, no surrogates, nothing
// past U+10FFFF, nothing cut short), else -1.
static StgInt utf8_count_valid(const unsigned char *p, StgInt len) {
  StgInt i = 0, count = 0;
  while (i < len) {
    unsigned char b = p[i];
    StgInt n, c;
    if (b < 0x80) n = 1, c = b;
    else if (b < 0xC2) return -1;
    else if (b < 0xE0) n = 2, c = b & 0x1F;
    else if (b < 0xF0) n = 3, c = b & 0x0F;
    else if (b < 0xF5) n = 4, c = b & 0x07;
    else return -1;
    if (i + n > len) return -1;
    for (StgInt j = 1; j < n; j++) {
      if ((p[i + j] & 0xC0) != 0x80) return -1;
      c = (c << 6) | (p[i + j] & 0x3F);
    }
    if ((n == 3 && c < 0x800) || (n == 4 && (c < 0x10000 || c > 0x10FFFF)) || (c >= 0xD800 && c <= 0xDFFF)) return -1;
    i += n, count++;
  }
  return count;
}

// --- building texts and bytes ---

// A text over the bytes [off, off + len) of arr, which are valid UTF-8, in
// chunks of at most the threshold's characters: what Text.fromText makes
// (T.chunksOf threshold, then snoc after snoc).
static StgClosure *text_over(UnisonJitCtx *ctx, StgArrBytes *arr, StgInt off, StgInt len) {
  const RopeKind *K = &TK;
  StgClosure *r = K->empty;
  const unsigned char *p = (const unsigned char *)arr->payload;
  StgInt i = off, end = off + len;
  while (i < end) {
    StgInt start = i, count = 0;
    while (i < end && count < K->threshold) i += utf8_len(p[i]), count++;
    r = rope_snoc(ctx, K, r, count, new_chunk(ctx, K, arr, count, start, i - start));
  }
  return r;
}

// a text of the UTF-8 bytes at p, copied
static void *text_copy(UnisonJitCtx *ctx, const unsigned char *p, StgInt len) {
  if (len == 0) return wrap_rope(ctx, &TK, TK.empty, NULL);
  StgArrBytes *arr = new_arr(ctx, len);
  memcpy(arr->payload, p, len);
  return wrap_rope(ctx, &TK, text_over(ctx, arr, 0, len), NULL);
}

// a bytes of one chunk holding the n bytes at p (Bytes.fromWord8s)
static void *bytes_copy(UnisonJitCtx *ctx, const unsigned char *p, StgInt n) {
  if (n == 0) return wrap_rope(ctx, &BK, BK.empty, NULL);
  StgArrBytes *arr = new_arr(ctx, n);
  memcpy(arr->payload, p, n);
  return wrap_rope(ctx, &BK, rope_one(ctx, &BK, new_chunk(ctx, &BK, arr, n, 0, n)), NULL);
}

// The bytes of a rope in one piece: a chunk's own bytes when it has one
// chunk, else a copy in malloc'd memory, which the caller frees (*heap).
static const unsigned char *rope_flat(const RopeKind *K, StgClosure *r, StgInt *len, unsigned char **heap) {
  *heap = NULL;
  if (LTAG(r) == 1) return *len = 0, (const unsigned char *)"";
  if (LTAG(r) == 2) {
    StgClosure *c = LP(r, 0);
    *len = chunk_len(K, c);
    return chunk_bytes(K, c);
  }
  StgInt n = rope_size(K, r), total = 0, off;
  for (StgInt i = 0; i < n;) {
    StgClosure *c = unison_jit_rope_chunk_at(K, r, i, &off);
    total += chunk_len(K, c), i += chunk_size(K, c);
  }
  unsigned char *buf = malloc(total > 0 ? total : 1);
  StgInt at = 0;
  for (StgInt i = 0; i < n;) {
    StgClosure *c = unison_jit_rope_chunk_at(K, r, i, &off);
    memcpy(buf + at, chunk_bytes(K, c), chunk_len(K, c));
    at += chunk_len(K, c), i += chunk_size(K, c);
  }
  *len = total;
  return *heap = buf;
}

// --- walking a list ---

static void node_each(StgClosure *nd, int depth, ItemFn fn, void *env) {
  StgClosure **k = node_kids(nd);
  StgInt n = node_arity(nd);
  for (StgInt i = 0; i < n; i++)
    if (depth == 1) fn(k[i], env);
    else node_each(k[i], depth - 1, fn, env);
}

static inline void item_each(StgClosure *x, int depth, ItemFn fn, void *env) {
  if (depth == 0) fn(x, env);
  else node_each(x, depth, fn, env);
}

// the elements of a deque level in order: `depth` is how many levels of
// nodes its items hold (0 at the top)
void unison_jit_lv_each(StgClosure *c, int top, int depth, ItemFn fn, void *env) {
  Lv v;
  if (!lv_read(c, top, &v)) return;
  for (StgClosure *l = v.pr; IS_CONS(l); l = TAIL(l)) item_each(HEAD(l), depth, fn, env);
  unison_jit_lv_each(v.m, 0, depth + 1, fn, env);
  StgClosure *k[MAXD];
  StgInt n = s_items(v.sf, k);
  for (StgInt i = n - 1; i >= 0; i--) item_each(k[i], depth, fn, env);
}

// --- Text.uncons, Text.unsnoc ---

// Some (c, rest) (front) or Some (rest, c), None for the empty text, NULL if
// not a text. `char_tag` is a Char's type tag closure.
void *unison_jit_text_uncons(UnisonJitCtx *ctx, void *text, void *none, int64_t some_tag, void *pair, int64_t pair_tag,
                             void *unit, void *char_tag, int64_t front) {
  const RopeKind *K = &TK;
  StgClosure *r = rope_of(K, text);
  if (r == NULL) return NULL;
  if (LTAG(r) == 1) return none;
  StgInt n = rope_size(K, r), off, clen, cp;
  StgClosure *c, *rest;
  if (front) {
    c = unison_jit_rope_chunk_at(K, r, 0, &off);
    cp = utf8_decode(chunk_bytes(K, c), &clen);
    rest = rope_drop(ctx, K, 1, r);
  } else {
    c = unison_jit_rope_chunk_at(K, r, n - 1, &off);
    const unsigned char *p = chunk_bytes(K, c);
    cp = utf8_decode(p + utf8_last_start(p, chunk_len(K, c)), &clen);
    rest = rope_take(ctx, K, n - 1, r);
  }
  void *wrest = wrap_rope(ctx, K, rest, NULL);
  void *p = front ? mk_pair(ctx, pair, pair_tag, unit, char_tag, cp, wrest, -1)
                  : mk_pair(ctx, pair, pair_tag, unit, wrest, -1, char_tag, cp);
  return mk_some(ctx, none, some_tag, p, -1);
}

// --- numbers to text ---

// Int.toText / Nat.toText: `show`
void *unison_jit_int_to_text(UnisonJitCtx *ctx, int64_t n, int64_t is_signed) {
  char buf[32];
  int len = is_signed ? snprintf(buf, sizeof buf, "%lld", (long long)n) : snprintf(buf, sizeof buf, "%llu", (unsigned long long)n);
  return text_copy(ctx, (const unsigned char *)buf, len);
}

// Haskell's `show` for a Double: the shortest digits that read back as the
// number (the nearest of them when several do), written as d.ddd for
// 0.1 <= |x| < 10^7 and as d.ddde<exp> otherwise, with at least one digit
// after the point. Returns the length.
static int hs_show_double(double x, char *out) {
  if (isnan(x)) return sprintf(out, "NaN");
  if (isinf(x)) return sprintf(out, x < 0 ? "-Infinity" : "Infinity");
  int neg = signbit(x) != 0;
  double ax = fabs(x);
  if (ax == 0) return sprintf(out, neg ? "-0.0" : "0.0");
  char digits[24] = "";
  int e10 = 0, nd = 0;
  for (int p = 1; p <= 17 && nd == 0; p++) {
    char buf[40];
    snprintf(buf, sizeof buf, "%.*e", p - 1, ax);
    // mantissa digits and exponent
    unsigned long long m = 0;
    int e = 0;
    const char *q = buf;
    for (; *q && *q != 'e'; q++)
      if (*q >= '0' && *q <= '9') m = m * 10 + (*q - '0');
    if (*q == 'e') e = atoi(q + 1);
    unsigned long long pow10 = 1;
    for (int i = 1; i < p; i++) pow10 *= 10;
    // m is the nearest p-digit decimal. If it doesn't read back as x, the
    // neighbour on x's side of m is the next nearest, then the other one:
    // the first of the three that reads back is what Haskell's floatToDigits
    // picks (the nearest of the shortest). Which side x lies on is read off
    // its exact expansion: if truncating it to p digits gives m, x >= m.
    char exact[64];
    snprintf(exact, sizeof exact, "%.30e", ax);
    unsigned long long trunc = 0;
    int seen = 0, ee = 0;
    for (const char *q = exact; *q && *q != 'e'; q++)
      if (*q >= '0' && *q <= '9' && seen < p) trunc = trunc * 10 + (*q - '0'), seen++;
    ee = atoi(strchr(exact, 'e') + 1);
    int side = (ee == e && trunc == m) ? 1 : -1;
    unsigned long long cands[3] = {m, m + side, m - side};
    for (int k = 0; k < 3 && nd == 0; k++) {
      unsigned long long c = cands[k];
      int ce = e;
      if (c >= pow10 * 10) c /= 10, ce++; // 999 + 1
      if (c < pow10 && p > 1) continue;    // a shorter string: an earlier p would have found it
      char str[48], ds[24];
      int l = snprintf(ds, sizeof ds, "%llu", c);
      snprintf(str, sizeof str, "%c.%se%d", ds[0], l > 1 ? ds + 1 : "0", ce);
      if (strtod(str, NULL) != ax) continue;
      nd = l, e10 = ce;
      memcpy(digits, ds, l + 1);
    }
  }
  if (nd == 0) nd = snprintf(digits, sizeof digits, "%.17g", ax); // not reached
  while (nd > 1 && digits[nd - 1] == '0') digits[--nd] = 0;
  int e = e10 + 1; // x = 0.d1d2... * 10^e
  char *o = out;
  if (neg) *o++ = '-';
  if (ax >= 0.1 && ax < 1e7) {
    if (e <= 0) {
      o += sprintf(o, "0.");
      for (int i = 0; i < -e; i++) *o++ = '0';
      o += sprintf(o, "%s", digits);
    } else {
      for (int i = 0; i < e; i++) *o++ = i < nd ? digits[i] : '0';
      *o++ = '.';
      if (nd > e) o += sprintf(o, "%s", digits + e);
      else *o++ = '0';
    }
  } else {
    *o++ = digits[0];
    *o++ = '.';
    if (nd > 1) o += sprintf(o, "%s", digits + 1);
    else *o++ = '0';
    o += sprintf(o, "e%d", e - 1);
  }
  *o = 0;
  return (int)(o - out);
}

void *unison_jit_float_to_text(UnisonJitCtx *ctx, int64_t bits) {
  double x;
  memcpy(&x, &bits, 8);
  char buf[64];
  int len = hs_show_double(x, buf);
  return text_copy(ctx, (const unsigned char *)buf, len);
}

// --- text to numbers ---

// Text.toInt (kind 0), Text.toNat (1), Text.toFloat (2): Some n or None, as
// the interpreter's readMaybe decides. Only the plain forms are decided here:
// an optional sign and digits (a point and an exponent for floats), with no
// other character; a text with anything else, or of more than 64 characters,
// is left to the interpreter (NULL), whose lexer also takes spaces, hex,
// parentheses and more. `tag` is the result's type tag closure.
void *unison_jit_text_to_num(UnisonJitCtx *ctx, void *text, void *none, int64_t some_tag, void *tag, int64_t kind) {
  const RopeKind *K = &TK;
  StgClosure *r = rope_of(K, text);
  if (r == NULL) return NULL;
  StgInt n = rope_size(K, r);
  if (n == 0) return none;
  if (n > 64) return NULL;
  unsigned char buf[65 * 4];
  StgInt len = 0, off;
  for (StgInt i = 0; i < n;) {
    StgClosure *c = unison_jit_rope_chunk_at(K, r, i, &off);
    memcpy(buf + len, chunk_bytes(K, c), chunk_len(K, c));
    len += chunk_len(K, c), i += chunk_size(K, c);
  }
  buf[len] = 0;
  StgInt i = 0;
  int neg = 0, plus = 0;
  if (buf[i] == '-') neg = 1, i++;
  else if (buf[i] == '+') plus = 1, i++;
  StgInt digits_at = i;
  while (i < len && buf[i] >= '0' && buf[i] <= '9') i++;
  StgInt ndig = i - digits_at;
  if (kind == 2) {
    // [-]digits[.digits][(e|E)[+|-]digits]: what both C and the Haskell lexer read
    if (plus) return none; // Read Double has no unary plus
    if (ndig == 0) return NULL; // "NaN", "Infinity", or nothing: the interpreter decides
    if (i < len && buf[i] == '.') {
      StgInt j = ++i;
      while (i < len && buf[i] >= '0' && buf[i] <= '9') i++;
      if (i == j) return none; // "1." reads as nothing
    }
    if (i < len && (buf[i] == 'e' || buf[i] == 'E')) {
      StgInt j = ++i;
      if (i < len && (buf[i] == '+' || buf[i] == '-')) i++, j++;
      while (i < len && buf[i] >= '0' && buf[i] <= '9') i++;
      if (i == j) return NULL;
    }
    if (i != len) return NULL;
    double d = strtod((const char *)buf, NULL);
    int64_t bits;
    memcpy(&bits, &d, 8);
    return mk_some(ctx, none, some_tag, tag, bits);
  }
  if (i != len) return NULL;    // something other than digits: the interpreter decides
  if (ndig == 0) return none;   // a sign alone
  if (plus && kind == 1) return none; // Text.toNat has no unary plus
  unsigned long long mag = 0;
  for (StgInt j = digits_at; j < len; j++) {
    unsigned d = buf[j] - '0';
    if (mag > (ULLONG_MAX - d) / 10) return none; // past every range
    mag = mag * 10 + d;
  }
  if (kind == 1) {
    if (neg && mag != 0) return none;
    return mk_some(ctx, none, some_tag, tag, (int64_t)mag);
  }
  if (neg ? mag > (unsigned long long)1 << 63 : mag > (unsigned long long)INT64_MAX) return none;
  return mk_some(ctx, none, some_tag, tag, neg ? (int64_t)(0 - mag) : (int64_t)mag);
}

// --- Text.fromCharList, Text.toCharList ---

typedef struct {
  void *tag;     // the elements' required type tag, or NULL
  StgInt count;  // elements seen
  StgInt bytes;  // their UTF-8 bytes (chars)
  int bad;       // an element of another type, or out of range
  unsigned char *out; // where to write (second pass)
  StgInt limit;  // the largest allowed value (bytes)
} PackEnv;

static void pack_measure(StgClosure *x, void *env) {
  PackEnv *e = env;
  StgInt u = LW(x, 1);
  if (LP(x, 0) != e->tag || u < 0 || u > e->limit) e->bad = 1;
  else e->count++, e->bytes += e->limit == 255 ? 1 : utf8_cp_len(u);
}

static void pack_write(StgClosure *x, void *env) {
  PackEnv *e = env;
  StgInt u = LW(x, 1);
  if (e->limit == 255) e->out[e->count++] = (unsigned char)u;
  else e->bytes += utf8_encode(u, e->out + e->bytes);
}

// Text.fromCharList: a text of the list's characters, as Text.pack makes it;
// NULL if not a list or an element isn't a Char.
void *unison_jit_text_pack(UnisonJitCtx *ctx, void *list, void *char_tag) {
  StgClosure *dq = deque_of(list);
  if (dq == NULL) return NULL;
  PackEnv e = {char_tag, 0, 0, 0, NULL, 0x10FFFF};
  unison_jit_lv_each(dq, 1, 0, pack_measure, &e);
  if (e.bad) return NULL;
  if (e.count == 0) return wrap_rope(ctx, &TK, TK.empty, NULL);
  StgArrBytes *arr = new_arr(ctx, e.bytes);
  e.out = (unsigned char *)arr->payload, e.bytes = 0;
  unison_jit_lv_each(dq, 1, 0, pack_write, &e);
  return wrap_rope(ctx, &TK, text_over(ctx, arr, 0, e.bytes), NULL);
}

// Bytes.fromList: one chunk of the list's Nats; NULL if an element isn't a
// Nat below 256 (the interpreter raises the error)
void *unison_jit_bytes_pack(UnisonJitCtx *ctx, void *list, void *nat_tag) {
  StgClosure *dq = deque_of(list);
  if (dq == NULL) return NULL;
  PackEnv e = {nat_tag, 0, 0, 0, NULL, 255};
  unison_jit_lv_each(dq, 1, 0, pack_measure, &e);
  if (e.bad) return NULL;
  if (e.count == 0) return wrap_rope(ctx, &BK, BK.empty, NULL);
  StgArrBytes *arr = new_arr(ctx, e.count);
  e.out = (unsigned char *)arr->payload, e.count = 0;
  unison_jit_lv_each(dq, 1, 0, pack_write, &e);
  StgInt n = e.count;
  return wrap_rope(ctx, &BK, rope_one(ctx, &BK, new_chunk(ctx, &BK, arr, n, 0, n)), NULL);
}

// Text.toCharList (chars) or Bytes.toList (bytes): a list of the elements
// as unboxed values with the given type tag
static void *rope_unpack(UnisonJitCtx *ctx, const RopeKind *K, void *x, void *tag) {
  StgClosure *r = rope_of(K, x);
  if (r == NULL) return NULL;
  StgClosure *acc = LF.nil;
  StgInt n = rope_size(K, r), off;
  for (StgInt i = 0; i < n;) {
    StgClosure *c = unison_jit_rope_chunk_at(K, r, i, &off);
    const unsigned char *p = chunk_bytes(K, c), *end = p + chunk_len(K, c);
    while (p < end) {
      StgInt v, l = 1;
      if (K->utf8) v = utf8_decode(p, &l);
      else v = *p;
      acc = lv_snoc(ctx, 1, acc, mk_val(ctx, tag, v));
      p += l;
    }
    i += chunk_size(K, c);
  }
  return wrap_list(ctx, acc);
}

void *unison_jit_text_unpack(UnisonJitCtx *ctx, void *text, void *char_tag) { return rope_unpack(ctx, &TK, text, char_tag); }
void *unison_jit_bytes_unpack(UnisonJitCtx *ctx, void *bytes, void *nat_tag) { return rope_unpack(ctx, &BK, bytes, nat_tag); }

// --- indexOf ---

// Text.indexOf (the position in characters) or Bytes.indexOf: Some i or
// None. An empty text needle finds position 0; an empty bytes needle is left
// to the interpreter.
static void *rope_index_of(UnisonJitCtx *ctx, const RopeKind *K, void *needle, void *hay, void *none, StgInt some_tag,
                           void *nat_tag) {
  StgClosure *a = rope_of(K, needle), *b = rope_of(K, hay);
  if (a == NULL || b == NULL) return NULL;
  if (rope_size(K, a) == 0) return K->utf8 ? mk_some(ctx, none, some_tag, nat_tag, 0) : NULL;
  StgInt la, lb;
  unsigned char *ha, *hb;
  const unsigned char *pa = rope_flat(K, a, &la, &ha), *pb = rope_flat(K, b, &lb, &hb);
  const unsigned char *at = la <= lb ? memmem(pb, lb, pa, la) : NULL;
  void *res = none;
  if (at != NULL) {
    StgInt ix = 0;
    if (K->utf8) {
      for (const unsigned char *q = pb; q < at; q++) ix += (*q & 0xC0) != 0x80;
    } else ix = at - pb;
    res = mk_some(ctx, none, some_tag, nat_tag, ix);
  }
  free(ha), free(hb);
  return res;
}

void *unison_jit_text_index_of(UnisonJitCtx *ctx, void *needle, void *hay, void *none, int64_t some_tag, void *nat_tag) {
  return rope_index_of(ctx, &TK, needle, hay, none, some_tag, nat_tag);
}
void *unison_jit_bytes_index_of(UnisonJitCtx *ctx, void *needle, void *hay, void *none, int64_t some_tag, void *nat_tag) {
  return rope_index_of(ctx, &BK, needle, hay, none, some_tag, nat_tag);
}

// --- ordering ---

// compare: -1, 0 or 1 (by code points, which is the byte order of UTF-8,
// and by bytes for Bytes; a prefix comes first), or 2 if a closure isn't a
// rope of the kind
static int64_t rope_cmp(const RopeKind *K, void *x, void *y) {
  StgClosure *a = rope_of(K, x), *b = rope_of(K, y);
  if (a == NULL || b == NULL) return 2;
  if (a == b) return 0;
  StgInt n = rope_size(K, a), m = rope_size(K, b);
  const unsigned char *pa = NULL, *pb = NULL;
  StgInt na = 0, nb = 0, ia = 0, ib = 0, off;
  for (;;) {
    if (na == 0 && ia < n) {
      StgClosure *c = unison_jit_rope_chunk_at(K, a, ia, &off);
      pa = chunk_bytes(K, c), na = chunk_len(K, c), ia += chunk_size(K, c);
    }
    if (nb == 0 && ib < m) {
      StgClosure *c = unison_jit_rope_chunk_at(K, b, ib, &off);
      pb = chunk_bytes(K, c), nb = chunk_len(K, c), ib += chunk_size(K, c);
    }
    if (na == 0 || nb == 0) return na == nb ? 0 : na == 0 ? -1 : 1;
    StgInt k = na < nb ? na : nb;
    int d = memcmp(pa, pb, k);
    if (d != 0) return d < 0 ? -1 : 1;
    pa += k, pb += k, na -= k, nb -= k;
  }
}

int64_t unison_jit_text_cmp(void *x, void *y) { return rope_cmp(&TK, x, y); }

// Universal comparison on two texts or two bytes (kinds as in
// unison_jit_foreign_eq): -1, 0, 1, or 2 for anything else
int64_t unison_jit_foreign_cmp(void *x, void *y, int64_t kinds) {
  if ((kinds & 1) && rope_of(&TK, x) != NULL) return rope_cmp(&TK, x, y);
  if ((kinds & 2) && rope_of(&BK, x) != NULL) return rope_cmp(&BK, x, y);
  return 2;
}

// --- the Text foreign functions ---

// Char.toText
void *unison_jit_char_to_text(UnisonJitCtx *ctx, int64_t c) {
  unsigned char buf[4];
  return text_copy(ctx, buf, utf8_encode(c, buf));
}

// Text.repeat, as Util.Text.replicate: fewer than the threshold's characters
// in all is one chunk, else the two halves appended
static StgClosure *text_rep(UnisonJitCtx *ctx, StgInt n, StgClosure *t, StgInt size) {
  const RopeKind *K = &TK;
  if (size * n < K->threshold) {
    if (size * n == 0) return K->empty;
    StgInt len;
    unsigned char *heap;
    const unsigned char *p = rope_flat(K, t, &len, &heap);
    StgArrBytes *arr = new_arr(ctx, len * n);
    for (StgInt i = 0; i < n; i++) memcpy((char *)arr->payload + i * len, p, len);
    free(heap);
    return rope_one(ctx, K, new_chunk(ctx, K, arr, size * n, 0, len * n));
  }
  if (n == 1) return t;
  return rope_append(ctx, K, text_rep(ctx, n / 2, t, size), text_rep(ctx, n - n / 2, t, size));
}

void *unison_jit_text_repeat(UnisonJitCtx *ctx, int64_t n, void *text) {
  StgClosure *r = rope_of(&TK, text);
  if (r == NULL) return NULL;
  StgInt size = rope_size(&TK, r);
  if (n < 0 || (size > 0 && n > ((StgInt)1 << 40) / size)) return NULL; // a size that can't be built
  if (size == 0 || n == 0) return wrap_rope(ctx, &TK, TK.empty, NULL);
  if (n == 1) return text;
  return wrap_rope(ctx, &TK, text_rep(ctx, n, r, size), NULL);
}

// a chunk's characters in reverse order, in a fresh array
static StgClosure *chunk_reverse(UnisonJitCtx *ctx, StgClosure *c) {
  const RopeKind *K = &TK;
  StgInt len = chunk_len(K, c);
  const unsigned char *p = chunk_bytes(K, c);
  StgArrBytes *arr = new_arr(ctx, len);
  unsigned char *o = (unsigned char *)arr->payload + len;
  for (StgInt i = 0; i < len;) {
    StgInt l = utf8_len(p[i]);
    o -= l;
    memcpy(o, p + i, l);
    i += l;
  }
  return new_chunk(ctx, K, arr, chunk_size(K, c), 0, len);
}

// Text.reverse, as the rope's: each chunk reversed, consed in front of the
// chunks before it
void *unison_jit_text_reverse(UnisonJitCtx *ctx, void *text) {
  const RopeKind *K = &TK;
  StgClosure *r = rope_of(K, text);
  if (r == NULL) return NULL;
  if (LTAG(r) == 1) return text;
  if (LTAG(r) == 2) return wrap_rope(ctx, K, rope_one(ctx, K, chunk_reverse(ctx, LP(r, 0))), NULL);
  StgClosure *acc = K->empty;
  StgInt n = rope_size(K, r), off;
  for (StgInt i = 0; i < n;) {
    StgClosure *c = unison_jit_rope_chunk_at(K, r, i, &off);
    acc = rope_cons(ctx, K, chunk_size(K, c), chunk_reverse(ctx, c), acc);
    i += chunk_size(K, c);
  }
  return wrap_rope(ctx, K, acc, NULL);
}

// Text.toUppercase / toLowercase for a text of ASCII only (Data.Text's case
// mapping of anything else is Unicode's, and may change the length): the
// chunks mapped one by one and snoc'd, as the rope's map does. NULL if any
// character is beyond ASCII.
void *unison_jit_text_case(UnisonJitCtx *ctx, void *text, int64_t upper) {
  const RopeKind *K = &TK;
  StgClosure *r = rope_of(K, text);
  if (r == NULL) return NULL;
  StgInt n = rope_size(K, r), off;
  for (StgInt i = 0; i < n;) {
    StgClosure *c = unison_jit_rope_chunk_at(K, r, i, &off);
    if (chunk_len(K, c) != chunk_size(K, c)) return NULL;
    i += chunk_size(K, c);
  }
  StgClosure *acc = K->empty;
  for (StgInt i = 0; i < n;) {
    StgClosure *c = unison_jit_rope_chunk_at(K, r, i, &off);
    StgInt len = chunk_len(K, c);
    const unsigned char *p = chunk_bytes(K, c);
    StgArrBytes *arr = new_arr(ctx, len);
    unsigned char *o = (unsigned char *)arr->payload;
    for (StgInt j = 0; j < len; j++) o[j] = upper ? toupper(p[j]) : tolower(p[j]);
    acc = rope_snoc(ctx, K, acc, len, new_chunk(ctx, K, arr, len, 0, len));
    i += len;
  }
  return wrap_rope(ctx, K, acc, NULL);
}

// Text.toUtf8: the same arrays as chunks of bytes, snoc'd in order
void *unison_jit_text_to_utf8(UnisonJitCtx *ctx, void *text) {
  StgClosure *r = rope_of(&TK, text);
  if (r == NULL) return NULL;
  StgClosure *acc = BK.empty;
  StgInt n = rope_size(&TK, r), off;
  for (StgInt i = 0; i < n;) {
    StgClosure *c = unison_jit_rope_chunk_at(&TK, r, i, &off);
    StgInt len = chunk_len(&TK, c);
    acc = rope_snoc(ctx, &BK, acc, len, new_chunk(ctx, &BK, LP(c, 0), len, chunk_off(&TK, c), len));
    i += chunk_size(&TK, c);
  }
  return wrap_rope(ctx, &BK, acc, NULL);
}

// Text.fromUtf8: Right text over a copy of the bytes, as Text.fromText cuts
// it; NULL for invalid UTF-8 (the interpreter builds the Failure). `either`
// is the pooled Either enumeration.
void *unison_jit_text_from_utf8(UnisonJitCtx *ctx, void *bytes, void *either, int64_t right_tag) {
  StgClosure *r = rope_of(&BK, bytes);
  if (r == NULL) return NULL;
  StgInt len;
  unsigned char *heap;
  const unsigned char *p = rope_flat(&BK, r, &len, &heap);
  StgInt count = utf8_count_valid(p, len);
  void *res = NULL;
  if (count >= 0) res = mk_data1(ctx, LP(either, 0), right_tag, text_copy(ctx, p, len), -1);
  free(heap);
  return res;
}

// --- the Bytes foreign functions ---

// the byte at position i of a rope of bytes
static inline unsigned char byte_at(StgClosure *r, StgInt i) {
  StgInt off;
  StgClosure *c = unison_jit_rope_chunk_at(&BK, r, i, &off);
  return chunk_bytes(&BK, c)[off];
}

// the `width`-byte number at position i, big- or little-endian
static inline uint64_t read_be_le(StgClosure *r, StgInt i, StgInt width, int be) {
  uint64_t v = 0;
  for (StgInt j = 0; j < width; j++) v |= (uint64_t)byte_at(r, i + j) << (8 * (be ? width - 1 - j : j));
  return v;
}

// Bytes.decodeNat16be and the others: Some (n, rest) or None, as
// Bytes.decodeNat* give them
void *unison_jit_bytes_decode_nat(UnisonJitCtx *ctx, void *bytes, int64_t width, int64_t be, void *none, int64_t some_tag,
                                  void *pair, int64_t pair_tag, void *unit, void *nat_tag) {
  StgClosure *r = rope_of(&BK, bytes);
  if (r == NULL) return NULL;
  if (rope_size(&BK, r) < width) return none;
  uint64_t v = read_be_le(r, 0, width, be);
  void *rest = wrap_rope(ctx, &BK, rope_drop(ctx, &BK, width, r), bytes);
  return mk_some(ctx, none, some_tag, mk_pair(ctx, pair, pair_tag, unit, nat_tag, v, rest, -1), -1);
}

// Bytes.encodeNat16be and the others: one chunk
void *unison_jit_bytes_encode_nat(UnisonJitCtx *ctx, int64_t n, int64_t width, int64_t be) {
  unsigned char buf[8];
  for (StgInt j = 0; j < width; j++) buf[j] = (unsigned char)((uint64_t)n >> (8 * (be ? width - 1 - j : j)));
  return bytes_copy(ctx, buf, width);
}

// Bytes.read16be and the others, and Bytes.at as Bytes.read, in two steps:
// whether the bytes reach `width` bytes from position i (1; 0 if not, when
// the interpreter raises the exception; -1 if not a bytes), then the number
int64_t unison_jit_bytes_read_ok(void *bytes, int64_t i, int64_t width) {
  StgClosure *r = rope_of(&BK, bytes);
  if (r == NULL) return -1;
  return i >= 0 && i <= rope_size(&BK, r) - width;
}

int64_t unison_jit_bytes_read_at(void *bytes, int64_t i, int64_t width, int64_t be) {
  return (int64_t)read_be_le(rope_of(&BK, bytes), i, width, be);
}

static const char B16[] = "0123456789abcdef";
static const char B32[] = "ABCDEFGHIJKLMNOPQRSTUVWXYZ234567";
static const char B64[] = "ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz0123456789+/";
static const char B64URL[] = "ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz0123456789-_";

// Bytes.toBase16 / toBase32 / toBase64 / toBase64UrlUnpadded (base 16, 32,
// 64, 65). Base 16 encodes chunk by chunk and snocs, as the Haskell does;
// the others encode the whole as one chunk.
void *unison_jit_bytes_to_base(UnisonJitCtx *ctx, void *bytes, int64_t base) {
  StgClosure *r = rope_of(&BK, bytes);
  if (r == NULL) return NULL;
  if (base == 16) {
    StgClosure *acc = BK.empty;
    StgInt n = rope_size(&BK, r), off;
    for (StgInt i = 0; i < n;) {
      StgClosure *c = unison_jit_rope_chunk_at(&BK, r, i, &off);
      StgInt len = chunk_len(&BK, c);
      const unsigned char *p = chunk_bytes(&BK, c);
      StgArrBytes *arr = new_arr(ctx, 2 * len);
      unsigned char *o = (unsigned char *)arr->payload;
      for (StgInt j = 0; j < len; j++) o[2 * j] = B16[p[j] >> 4], o[2 * j + 1] = B16[p[j] & 15];
      acc = rope_snoc(ctx, &BK, acc, 2 * len, new_chunk(ctx, &BK, arr, 2 * len, 0, 2 * len));
      i += len;
    }
    return wrap_rope(ctx, &BK, acc, NULL);
  }
  StgInt len;
  unsigned char *heap;
  const unsigned char *p = rope_flat(&BK, r, &len, &heap);
  StgInt out_len = base == 32 ? (len + 4) / 5 * 8 : base == 64 ? (len + 2) / 3 * 4 : (len * 4 + 2) / 3;
  unsigned char *out = malloc(out_len > 0 ? out_len : 1), *o = out;
  if (base == 32) {
    for (StgInt i = 0; i < len; i += 5) {
      uint64_t v = 0;
      StgInt k = len - i < 5 ? len - i : 5;
      for (StgInt j = 0; j < 5; j++) v = (v << 8) | (j < k ? p[i + j] : 0);
      StgInt chars = (k * 8 + 4) / 5;
      for (StgInt j = 0; j < 8; j++) *o++ = j < chars ? B32[(v >> (35 - 5 * j)) & 31] : '=';
    }
  } else {
    const char *alpha = base == 64 ? B64 : B64URL;
    for (StgInt i = 0; i < len; i += 3) {
      StgInt k = len - i < 3 ? len - i : 3;
      uint32_t v = 0;
      for (StgInt j = 0; j < 3; j++) v = (v << 8) | (j < k ? p[i + j] : 0);
      StgInt chars = k + 1;
      for (StgInt j = 0; j < 4; j++) {
        if (j < chars) *o++ = alpha[(v >> (18 - 6 * j)) & 63];
        else if (base == 64) *o++ = '=';
      }
    }
  }
  void *res = bytes_copy(ctx, out, o - out);
  free(out), free(heap);
  return res;
}

static int base_value(const char *alpha, int n, unsigned char c) {
  const char *q = memchr(alpha, c, n);
  return q == NULL ? -1 : (int)(q - alpha);
}

// Bytes.fromBase16 / 32 / 64 / 64UrlUnpadded: Right bytes for canonical
// input (the alphabet exactly, full padding where the encoding has it, no
// stray bits), NULL for anything else, which the interpreter decides and
// describes. `either` is the pooled Either enumeration.
void *unison_jit_bytes_from_base(UnisonJitCtx *ctx, void *bytes, int64_t base, void *either, int64_t right_tag) {
  StgClosure *r = rope_of(&BK, bytes);
  if (r == NULL) return NULL;
  StgInt len;
  unsigned char *heap;
  const unsigned char *p = rope_flat(&BK, r, &len, &heap);
  unsigned char *out = malloc(len > 0 ? len : 1), *o = out;
  int ok = 1;
  if (base == 16) {
    if (len % 2) ok = 0;
    for (StgInt i = 0; ok && i < len; i += 2) {
      int a = base_value(B16, 16, p[i]), b = base_value(B16, 16, p[i + 1]);
      if (a < 0 || b < 0) ok = 0;
      else *o++ = (a << 4) | b;
    }
  } else if (base == 32) {
    if (len % 8) ok = 0;
    for (StgInt i = 0; ok && i < len; i += 8) {
      uint64_t v = 0;
      StgInt chars = 8;
      while (chars > 0 && p[i + chars - 1] == '=') chars--;
      if (i + 8 < len && chars < 8) ok = 0; // padding only at the end
      StgInt k = chars * 5 / 8;             // bytes
      if (chars == 1 || chars == 3 || chars == 6) ok = 0;
      for (StgInt j = 0; ok && j < 8; j++) {
        int d = j < chars ? base_value(B32, 32, p[i + j]) : 0;
        if (d < 0) ok = 0;
        v = (v << 5) | d;
      }
      if (ok && (v & ((1ULL << (40 - 8 * k)) - 1)) != 0) ok = 0; // stray bits
      for (StgInt j = 0; ok && j < k; j++) *o++ = (unsigned char)(v >> (32 - 8 * j));
    }
  } else {
    const char *alpha = base == 64 ? B64 : B64URL;
    StgInt chars = len;
    if (base == 64) {
      if (len % 4) ok = 0;
      while (chars > 0 && p[chars - 1] == '=') chars--;
      if (len - chars > 2) ok = 0;
    } else if (len % 4 == 1) ok = 0;
    for (StgInt i = 0; ok && i < chars; i += 4) {
      StgInt n = chars - i < 4 ? chars - i : 4;
      if (n == 1) ok = 0;
      uint32_t v = 0;
      for (StgInt j = 0; ok && j < 4; j++) {
        int d = j < n ? base_value(alpha, 64, p[i + j]) : 0;
        if (d < 0) ok = 0;
        v = (v << 6) | d;
      }
      StgInt k = n - 1;
      if (ok && (v & ((1u << (24 - 8 * k)) - 1)) != 0) ok = 0;
      for (StgInt j = 0; ok && j < k; j++) *o++ = (unsigned char)(v >> (16 - 8 * j));
    }
  }
  void *res = ok ? mk_data1(ctx, LP(either, 0), right_tag, bytes_copy(ctx, out, o - out), -1) : NULL;
  free(out), free(heap);
  return res;
}

// ---------------------------------------------------------------------------
// Partial applications
//
// The Name instruction: a function value with more arguments captured. The
// closure is PAp cix comb (useg, bseg) with the first two unpacked; its
// pointers are the reference, the entry, and the two boxes around the
// segment's arrays, and its words the rest of cix and comb. The new closure
// is a copy with longer arrays: the new arguments first, the last one at
// index 0 (as the interpreter's augSeg lays them out), then the old ones.

static StgWord PF_pap_info, PF_pap_tag;
enum { PAP_USEG = 2, PAP_BSEG = 3, PAP_WORDS = 9 };
#define NAME_MAX_ARGS 4

void unison_jit_closure_init(int64_t *info) { PF_pap_info = info[0], PF_pap_tag = info[1]; }

// NULL if `fun` isn't a PAp.
void *unison_jit_name(UnisonJitCtx *ctx, void *fun, int64_t n, int64_t u0, void *b0, int64_t u1, void *b1,
                      int64_t u2, void *b2, int64_t u3, void *b3) {
  if (LTAG(fun) != (int)PF_pap_tag || (StgWord)LUN(fun)->header.info != PF_pap_info) return NULL;
  StgArrBytes *us = (StgArrBytes *)LP(fun, PAP_USEG);
  StgMutArrPtrs *bs = (StgMutArrPtrs *)LP(fun, PAP_BSEG);
  StgWord old = bs->ptrs, total = old + n;
  StgWord cards = ROUNDUP_BYTES_TO_WDS(mutArrPtrsCards(total));
  StgWord bytes_words = sizeofW(StgArrBytes) + total, ptrs_words = sizeofW(StgMutArrPtrs) + total + cards;
  StgWord *p = list_alloc(ctx, 1 + PAP_WORDS + bytes_words + ptrs_words);
  StgArrBytes *nus = (StgArrBytes *)(p + 1 + PAP_WORDS);
  StgMutArrPtrs *nbs = (StgMutArrPtrs *)((StgWord *)nus + bytes_words);
  // the closure: a copy, but for the two arrays
  memcpy(p, LUN(fun), (1 + PAP_WORDS) * sizeof(StgWord));
  p[1 + PAP_USEG] = (StgWord)nus;
  p[1 + PAP_BSEG] = (StgWord)nbs;
  SET_INFO((StgClosure *)nus, &stg_ARR_WORDS_info);
  nus->bytes = total * sizeof(StgWord);
  SET_INFO((StgClosure *)nbs, &stg_MUT_ARR_PTRS_FROZEN_CLEAN_info);
  nbs->ptrs = total;
  nbs->size = total + cards;
  memset(&nbs->payload[total], 0, cards * sizeof(StgWord));
  int64_t u[NAME_MAX_ARGS] = {u0, u1, u2, u3};
  void *b[NAME_MAX_ARGS] = {b0, b1, b2, b3};
  for (int64_t k = 0; k < n; k++) {
    nus->payload[n - 1 - k] = u[k];
    nbs->payload[n - 1 - k] = (StgClosure *)b[k];
  }
  memcpy(&nus->payload[n], us->payload, old * sizeof(StgWord));
  memcpy(&nbs->payload[n], bs->payload, old * sizeof(StgWord));
  return (void *)((StgWord)p | PF_pap_tag);
}

// For the startup self-test: Name on elems[0] with `n` arguments, the k-th
// being the word 100 + k and the closure elems[1]; the result goes in elems[3].
int64_t unison_jit_name_test(void **elems, int64_t n) {
  UnisonJitCtx tmp = {0}, *ctx = &tmp;
  ctx->cap = rts_unsafeGetMyCapability();
  void *res = unison_jit_name(ctx, elems[0], n, 100, elems[1], 101, elems[1], 102, elems[1], 103, elems[1]);
  if (res == NULL) return 0;
  elems[3] = res;
  unison_jit_mark_bstk(elems, 3, 3);
  return 1;
}

// The calling thread's C stack: its highest address and its size, so that the
// stack floor is top - size. Returns 0 when the platform can't say.
static int thread_stack_bounds(int64_t *top, int64_t *size) {
#if defined(__APPLE__)
  pthread_t self = pthread_self();
  *top = (int64_t)pthread_get_stackaddr_np(self);
  *size = (int64_t)pthread_get_stacksize_np(self);
  return 1;
#elif defined(_WIN32)
  ULONG_PTR lo, hi;
  GetCurrentThreadStackLimits(&lo, &hi);
  *top = (int64_t)hi;
  *size = (int64_t)(hi - lo);
  return 1;
#elif defined(__linux__)
  // glibc: the attributes of the running thread, the main thread included
  // (for it, the size is the stack rlimit)
  pthread_attr_t attr;
  if (pthread_getattr_np(pthread_self(), &attr) != 0) return 0;
  void *addr;
  size_t sz;
  int ok = pthread_attr_getstack(&attr, &addr, &sz) == 0;
  pthread_attr_destroy(&attr);
  if (!ok) return 0;
  *top = (int64_t)addr + (int64_t)sz;
  *size = (int64_t)sz;
  return 1;
#else
  (void)top;
  (void)size;
  return 0;
#endif
}

// One context per OS thread. A Haskell thread stays on one OS thread for the
// whole of an unsafe foreign call, and everything that touches the context
// happens inside unison_jit_enter, so this is safe. Contexts are never freed;
// there are only as many as the runtime has worker threads.
static _Thread_local UnisonJitCtx *thread_ctx = NULL;

static UnisonJitCtx *get_ctx(void) {
  if (thread_ctx == NULL) {
    UnisonJitCtx *ctx = calloc(1, sizeof(UnisonJitCtx));
    ctx->stress_poll = stress_poll;
    ctx->stress_poll_left = stress_poll;
    ctx->stress_callee = stress_callee;
    ctx->stress_callee_left = stress_callee;
    ctx->max_frames = cstack_budget / MIN_NATIVE_FRAME + 16;
    ctx->frames = calloc(ctx->max_frames, 3 * sizeof(int64_t));
    int64_t top, size;
    if (thread_stack_bounds(&top, &size)) {
      ctx->stack_floor = top - size + CSTACK_RESERVE;
    } else {
      // no floor: the budget alone bounds native recursion
      ctx->stack_floor = 0;
      if (trace) fprintf(stderr, "[jit] new context: the thread's stack bounds are unknown on this platform\n");
    }
    if (trace)
      fprintf(stderr, "[jit] new context: thread stack %lld bytes, budget %lld, %lld frame records\n",
              (long long)size, (long long)cstack_budget, (long long)ctx->max_frames);
    thread_ctx = ctx;
  }
  return thread_ctx;
}

typedef int64_t (*UnisonNativeFn)(UnisonJitCtx *ctx, int64_t ap, int64_t fp, int64_t sp);

// Runs a native function. The arrays arrive as pointers to their first element,
// which is how GHC passes MutableByteArray# and MutableArray# to foreign calls
// (an unsafe call, so they can't move). The results go in the spare words past
// the last slot of the unboxed stack: out[0..2] hold the new ap, fp and sp, and
// out[3] the number of frame records. Up to UNISON_JIT_INLINE_FRAMES records
// follow from out[4] on, three words each; if there are more, out[4] is instead
// a malloc'd copy of them all, which the caller frees. The boxed stack's spare
// slots hold the handler environment: bstk[stack_size] is the one to run with,
// and holds the current one when this returns; the slots after it hold the
// environments of the Local frame records (see unison_jit_exit_frame). Returns
// the status.
//
// This is the round trip's fixed cost, so it does as little as it can: one
// thread-local lookup for the context, one for the capability, no allocation.
int64_t unison_jit_enter(UnisonNativeFn fn, int64_t *ustk, void **bstk, void **pool, int64_t stack_size,
                         int64_t ap, int64_t fp, int64_t sp) {
  int64_t *out = ustk + stack_size;
  {
    StgArrBytes *arr = (StgArrBytes *)((StgWord *)ustk - sizeofW(StgArrBytes));
    if (__builtin_expect(arr->bytes < (StgWord)((stack_size + UNISON_JIT_OUT_WORDS) * 8), 0)) {
      fprintf(stderr, "[jit] the unboxed stack has no room for results (%lld bytes, %lld slots)\n",
              (long long)arr->bytes, (long long)stack_size);
      abort();
    }
    StgMutArrPtrs *barr = (StgMutArrPtrs *)((StgWord *)bstk - sizeofW(StgMutArrPtrs));
    if (__builtin_expect(barr->ptrs < (StgWord)(stack_size + UNISON_JIT_OUT_PTRS), 0)) {
      fprintf(stderr, "[jit] the boxed stack has no room for results (%lld elements, %lld slots)\n",
              (long long)barr->ptrs, (long long)stack_size);
      abort();
    }
  }
  UnisonJitCtx *ctx = get_ctx();
  Capability *cap = rts_unsafeGetMyCapability();
  void *henv_in = bstk[stack_size];
  ctx->ustk = ustk;
  ctx->bstk = bstk;
  ctx->pool = pool;
  ctx->henv = henv_in;
  ctx->n_locals = 0;
  ctx->n_local_ptrs = 0;
  ctx->stack_size = stack_size;
  ctx->hplim = hplim_of(cap);
  ctx->max_sp = sp;
  ctx->n_frames = 0;
  ctx->cap = cap;
  ctx->bump = bump_alloc;
  ctx->slow_words = 0;
  if (bump_alloc) {
    alloc_refresh(ctx, alloc_budget);
  } else {
    // every allocation goes through allocate(), which counts the words in
    // hp from 0 for the poll's test against budget_end
    ctx->hp = ctx->hp_lim = NULL;
    ctx->budget_end = (void *)(uintptr_t)(alloc_budget * (int64_t)sizeof(StgWord));
  }
  // The budget counts down from here, but never below the thread's floor.
  int64_t here = (int64_t)&ctx;
  int64_t limit = here - cstack_budget;
  ctx->cstack_limit = limit > ctx->stack_floor ? limit : ctx->stack_floor;
  if (__builtin_expect(trace, 0))
    fprintf(stderr, "[jit] enter %p ap/fp/sp %lld/%lld/%lld hplim %p stress %lld/%lld (global %lld)\n", (void *)fn,
            (long long)ap, (long long)fp, (long long)sp, *ctx->hplim, (long long)ctx->stress_poll,
            (long long)ctx->stress_poll_left, (long long)stress_poll);
  int64_t status = fn(ctx, ap, fp, sp);
  if (ctx->bump) {
    // the block's free pointer catches up, and the thread's allocation
    // counter is charged for the words allocated inline
    alloc_sync(ctx);
    int64_t inline_words = alloc_budget - budget_left(ctx) - ctx->slow_words;
    StgTSO *tso = reg_table(cap)->rCurrentTSO;
    if (tso != NULL && inline_words > 0) tso->alloc_limit -= inline_words * (int64_t)sizeof(StgWord);
    ctx->hp = ctx->hp_lim = NULL;
  }
  // Under the stress modes: a function that leaves with the same exit a
  // million times in a row, without having moved the stack, is being asked to
  // run again and again and getting nowhere (a poll that always fires, say).
  // Say what the context looks like, once. (Without the stack condition this
  // would fire for any function a benchmark loop calls a million times.)
  if (__builtin_expect(stress_poll > 0 || stress_callee > 0 || trace, 0)) {
    static _Thread_local UnisonNativeFn last_fn;
    static _Thread_local int64_t last_status, repeats;
    if (fn == last_fn && status == last_status && status > 0 && ctx->sp == sp) {
      if (++repeats == 1000000)
        fprintf(stderr,
                "[jit] %p left with exit %lld a million times in a row: hplim %p, budget left %lld, stress poll %lld/%lld, "
                "C stack here %p, limit %p, floor %p, sp %lld, frames %lld\n",
                (void *)fn, (long long)status, *ctx->hplim, (long long)budget_left(ctx), (long long)ctx->stress_poll_left,
                (long long)ctx->stress_poll, (void *)&ctx, (void *)ctx->cstack_limit, (void *)ctx->stack_floor,
                (long long)sp, (long long)ctx->n_frames);
    } else {
      last_fn = fn;
      last_status = status;
      repeats = 0;
    }
  }
  static int trace_yield = -1;
  if (trace_yield < 0) trace_yield = getenv("UNISON_JIT_TRACE_YIELD") != NULL;
  if (__builtin_expect(trace_yield, 0) && status == 0 && ctx->sp >= 0 && ctx->sp < stack_size) {
    // the yielded value: its boxed pointer's tag and closure type
    StgClosure *b = (StgClosure *)bstk[ctx->sp];
    const StgInfoTable *it = GET_CLOSURE_TAG(b) || b ? get_itbl(UNTAG_CLOSURE(b)) : NULL;
    fprintf(stderr, "[jit] yield at sp %lld: b %p tag %d type %d u %lld\n", (long long)ctx->sp, (void *)b,
            (int)GET_CLOSURE_TAG(b), it ? (int)it->type : -1, (long long)ustk[ctx->sp]);
    trace_fields(b, 1);
  }
  int64_t n = ctx->n_frames;
  out[0] = ctx->ap;
  out[1] = ctx->fp;
  out[2] = ctx->sp;
  out[3] = n;
  if (__builtin_expect(n > 0, 0)) {
    if (n > ctx->max_frames) {
      fprintf(stderr, "[jit] frame record buffer overflowed (%lld records)\n", (long long)n);
      abort();
    }
    size_t bytes = n * 3 * sizeof(int64_t);
    if (n <= UNISON_JIT_INLINE_FRAMES) {
      memcpy(out + 4, ctx->frames, bytes);
    } else {
      int64_t *copy = malloc(bytes);
      memcpy(copy, ctx->frames, bytes);
      out[4] = (int64_t)copy;
    }
    if (trace) fprintf(stderr, "[jit] %lld frame records\n", (long long)n);
  }
  // Native code stores into bstk without a write barrier. Tell the GC which
  // parts changed, once, now that it is about to be allowed to run again.
  int64_t hi = ctx->max_sp < stack_size - 1 ? ctx->max_sp : stack_size - 1;
  if (hi >= fp + 1) unison_jit_mark_bstk(bstk, fp + 1 < 0 ? 0 : fp + 1, hi);
  // The environment native code was running with (switched, after an exit
  // from inside a handler case), and the ones its Local records refer to.
  if (ctx->henv != henv_in || ctx->n_local_ptrs > 0) {
    bstk[stack_size] = ctx->henv;
    unison_jit_mark_bstk(bstk, stack_size, stack_size + ctx->n_local_ptrs);
  }
  return status;
}

// ---------------------------------------------------------------------------
// Arrays and Refs
//
// The builtins over MutableArray, ImmutableArray, MutableByteArray and
// ImmutableByteArray (Data.Primitive's arrays, wrapped as Foreign
// (WrapMutableArray arr) and so on), and over Ref (Foreign (WrapIORef mv))
// and Ticket (Foreign (WrapTicket val)). Ports of the Haskell operations in
// Unison.Runtime.Foreign.Function and Machine/Primops.hs, with the same
// bounds arithmetic (Word64, wrapping) so that exactly the same calls fail;
// a failing call is left to the interpreter, which raises the exception.
//
// Each Wrap constructor's info pointer and pointer tag come from the layout
// probe (unison_jit_array_init). A Wrap holds the unlifted array (or MutVar#,
// or the ticket's value) as its one field: Data.Primitive's boxes are
// unpacked into the strict field. WrapPtr holds an Addr#, a non-pointer.


int64_t unison_jit_array_init(void **raw, int64_t *info) {
  WrapInfo *ws[] = {&AK.marray, &AK.array, &AK.mbarray, &AK.barray, &AK.ioref, &AK.ticket, &AK.ptr};
  for (int i = 0; i < 7; i++) ws[i]->info = info[2 * i], ws[i]->tag = info[2 * i + 1];
  AK.empty_val = unison_jit_settle(raw[0]);
  AK.ok = LF.foreign_info != 0;
  return AK.ok;
}

// Foreign (Wrap field), in four words.
static void *wrap_word(UnisonJitCtx *ctx, const WrapInfo *w, StgWord field) {
  StgWord *p = list_alloc(ctx, 4);
  p[0] = w->info;
  p[1] = field;
  p[2] = LF.foreign_info;
  p[3] = (StgWord)p | w->tag;
  return (void *)((StgWord)(p + 2) | 7);
}

// checkBoundsPrim: fails when off + esz > size or off > size, in Word64
static inline int bytes_ok(StgWord size, StgWord off, StgWord esz) { return !(off + esz > size || off > size); }

// --- byte arrays ---

// kind 0: MutableByteArray, 1: ImmutableByteArray
static inline StgArrBytes *barray_of(void *x, int64_t kind) {
  return (StgArrBytes *)wrapped_of(x, kind == 0 ? &AK.mbarray : &AK.barray);
}

// Byte array size, reads and writes are generated code (Codegen.byteArray,
// byteArrayElem): a wrapper check, a bounds check and a load or store.

int64_t unison_jit_barray_copy(void *dst, int64_t doff, void *src, int64_t soff, int64_t l, int64_t kind) {
  StgArrBytes *d = barray_of(dst, 0), *s = barray_of(src, kind);
  if (d == NULL || s == NULL) return 0;
  if (l == 0) return 1;
  if (!bytes_ok(d->bytes, (StgWord)doff + (StgWord)l, 0) || !bytes_ok(s->bytes, (StgWord)soff + (StgWord)l, 0)) return 0;
  memmove((unsigned char *)d->payload + doff, (const unsigned char *)s->payload + soff, (size_t)l);
  return 1;
}

// freeze! (in place: the same array under the immutable wrapper) and
// freeze (a copy of a slice; length 0 gives an empty array).
void *unison_jit_barray_freeze(UnisonJitCtx *ctx, void *x, int64_t off, int64_t len, int64_t in_place) {
  StgArrBytes *a = barray_of(x, 0);
  if (a == NULL) return NULL;
  if (in_place) return wrap_word(ctx, &AK.barray, (StgWord)a);
  if (len != 0 && !bytes_ok(a->bytes, (StgWord)off + (StgWord)len, 0)) return NULL;
  StgArrBytes *b = new_arr(ctx, len == 0 ? 0 : (StgWord)len);
  if (len != 0) memcpy(b->payload, (const unsigned char *)a->payload + off, (size_t)len);
  return wrap_word(ctx, &AK.barray, (StgWord)b);
}

// ImmutableByteArray.toBytes: a Bytes of one chunk over the array's slice
// (Bytes.fromByteArray: snoc onto the empty rope).
void *unison_jit_barray_to_bytes(UnisonJitCtx *ctx, void *x, int64_t off, int64_t len) {
  StgArrBytes *a = barray_of(x, 1);
  if (a == NULL) return NULL;
  if (len == 0) return wrap_rope(ctx, &BK, BK.empty, NULL);
  if (!bytes_ok(a->bytes, (StgWord)off + (StgWord)len, 0)) return NULL;
  StgClosure *c = new_chunk(ctx, &BK, a, len, off, len);
  return wrap_rope(ctx, &BK, rope_snoc(ctx, &BK, BK.empty, len, c), NULL);
}

// ImmutableByteArray.fromBytes (Bytes.toByteArray): the chunk's own array
// when the rope is one chunk covering all of it, else a copy.
void *unison_jit_barray_from_bytes(UnisonJitCtx *ctx, void *bytes) {
  StgClosure *r = rope_of(&BK, bytes);
  if (r == NULL) return NULL;
  if (LTAG(r) == 2) {
    StgClosure *c = LP(r, 0);
    StgArrBytes *a = (StgArrBytes *)LP(c, 0);
    if (chunk_off(&BK, c) == 0 && (StgWord)chunk_len(&BK, c) == a->bytes) return wrap_word(ctx, &AK.barray, (StgWord)a);
  }
  StgInt n;
  unsigned char *heap;
  const unsigned char *p = rope_flat(&BK, r, &n, &heap);
  StgArrBytes *b = new_arr(ctx, (StgWord)n);
  memcpy(b->payload, p, (size_t)n);
  free(heap);
  return wrap_word(ctx, &AK.barray, (StgWord)b);
}

// Scope.bytearray / bytearrayOf and the pinned variants (also the IO ones):
// n bytes, filled with `fill` when fill >= 0 (newByteArray leaves them as
// they are). A pinned array comes from the RTS's pinned allocator, as
// newPinnedByteArray# does, 16-byte aligned.
void *unison_jit_barray_new(UnisonJitCtx *ctx, int64_t n, int64_t fill, int64_t pinned) {
  if (!AK.ok || n < 0) return NULL;
  StgArrBytes *a;
  if (pinned) {
    StgWord words = sizeofW(StgArrBytes) + ROUNDUP_BYTES_TO_WDS(n);
    if (ctx->bump) {
      int64_t left = budget_left(ctx) - (int64_t)words;
      alloc_sync(ctx);
      a = (StgArrBytes *)allocatePinned((Capability *)ctx->cap, words, 16, sizeofW(StgArrBytes) * sizeof(W_));
      ctx->slow_words += (int64_t)words;
      alloc_refresh(ctx, left);
    } else {
      a = (StgArrBytes *)allocatePinned((Capability *)ctx->cap, words, 16, sizeofW(StgArrBytes) * sizeof(W_));
    }
    SET_INFO((StgClosure *)a, &stg_ARR_WORDS_info);
    a->bytes = (StgWord)n;
  } else {
    a = new_arr(ctx, (StgWord)n);
  }
  if (fill >= 0 && n > 0) memset(a->payload, (int)fill, (size_t)n);
  return wrap_word(ctx, &AK.mbarray, (StgWord)a);
}

// PinnedByteArray.contents: the address of the bytes, as a Ptr.
void *unison_jit_barray_contents(UnisonJitCtx *ctx, void *x) {
  StgArrBytes *a = barray_of(x, 0);
  if (a == NULL) return NULL;
  return wrap_word(ctx, &AK.ptr, (StgWord)a->payload);
}

// FFI.Ptr.Ptr.get and getAt: an address read through a pointer, as a Ptr.
void *unison_jit_wrap_ptr(UnisonJitCtx *ctx, int64_t addr) { return wrap_word(ctx, &AK.ptr, (StgWord)addr); }

// --- pointer arrays ---

// kind 0: MutableArray, 1: ImmutableArray
static inline StgMutArrPtrs *parray_of(void *x, int64_t kind) {
  return (StgMutArrPtrs *)wrapped_of(x, kind == 0 ? &AK.marray : &AK.array);
}

// a fresh array of n elements, every one `init`, with the given info
// (mutable dirty, or frozen); the card table zeroed as newArray# does
static StgMutArrPtrs *parray_new(UnisonJitCtx *ctx, StgWord n, StgClosure *init, const StgInfoTable *info) {
  StgWord cards = mutArrPtrsCardTableSize(n);
  StgMutArrPtrs *a = (StgMutArrPtrs *)list_alloc(ctx, sizeofW(StgMutArrPtrs) + n + cards);
  SET_INFO((StgClosure *)a, info);
  a->ptrs = n;
  a->size = n + cards;
  for (StgWord i = 0; i < n; i++) a->payload[i] = init;
  memset(&a->payload[n], 0, cards * sizeof(W_));
  return a;
}

// after writes to dst[from .. to]: the dirty info and the cards, as
// copyMutableArray# does
static inline void parray_dirty(StgMutArrPtrs *a, StgWord from, StgWord to) {
  SET_INFO((StgClosure *)a, &stg_MUT_ARR_PTRS_DIRTY_info);
  for (StgWord c = from >> MUT_ARR_PTRS_CARD_BITS; c <= to >> MUT_ARR_PTRS_CARD_BITS; c++) *mutArrPtrsCard(a, c) = 1;
}

// Scope.array / arrayOf (and IO's): n copies of the value, or of the
// runtime's empty value. The value is one Val shared by every element, as
// newArray n v shares it.
void *unison_jit_parray_new(UnisonJitCtx *ctx, int64_t n, int64_t has_init, int64_t u, void *b) {
  if (!AK.ok || n < 0) return NULL;
  StgClosure *init = has_init ? mk_val(ctx, b, u) : AK.empty_val;
  StgMutArrPtrs *a = parray_new(ctx, (StgWord)n, init, &stg_MUT_ARR_PTRS_DIRTY_info);
  return wrap_word(ctx, &AK.marray, (StgWord)a);
}

// copyTo!: dst a MutableArray, src mutable (kind 0) or immutable (1);
// checkBounds on doff + l - 1 and soff + l - 1
int64_t unison_jit_parray_copy(void *dst, int64_t doff, void *src, int64_t soff, int64_t l, int64_t kind) {
  StgMutArrPtrs *d = parray_of(dst, 0), *s = parray_of(src, kind);
  if (d == NULL || s == NULL) return 0;
  if (l == 0) return 1;
  StgWord dl = (StgWord)doff + (StgWord)l - 1, sl = (StgWord)soff + (StgWord)l - 1;
  if (!(dl < d->ptrs) || !(sl < s->ptrs)) return 0;
  memmove(&d->payload[doff], &s->payload[soff], (size_t)l * sizeof(W_));
  parray_dirty(d, (StgWord)doff, dl);
  return 1;
}

// freeze! and freeze, as for byte arrays
void *unison_jit_parray_freeze(UnisonJitCtx *ctx, void *x, int64_t off, int64_t len, int64_t in_place) {
  StgMutArrPtrs *a = parray_of(x, 0);
  if (a == NULL) return NULL;
  if (in_place) {
    SET_INFO((StgClosure *)a, &stg_MUT_ARR_PTRS_FROZEN_DIRTY_info);
    return wrap_word(ctx, &AK.array, (StgWord)a);
  }
  if (len == 0) return wrap_word(ctx, &AK.array, (StgWord)parray_new(ctx, 0, AK.empty_val, &stg_MUT_ARR_PTRS_FROZEN_DIRTY_info));
  StgWord last = (StgWord)off + (StgWord)len - 1;
  if (!(last < a->ptrs)) return NULL;
  StgMutArrPtrs *b = parray_new(ctx, (StgWord)len, AK.empty_val, &stg_MUT_ARR_PTRS_FROZEN_DIRTY_info);
  memcpy(b->payload, &a->payload[off], (size_t)len * sizeof(W_));
  return wrap_word(ctx, &AK.array, (StgWord)b);
}

// --- Refs and tickets ---

// Scope.ref / IO.ref: a fresh IORef holding a fresh Val
void *unison_jit_ref_new(UnisonJitCtx *ctx, int64_t u, void *b) {
  if (!AK.ok) return NULL;
  StgClosure *v = mk_val(ctx, b, u);
  StgMutVar *mv = (StgMutVar *)list_alloc(ctx, sizeofW(StgMutVar));
  SET_INFO((StgClosure *)mv, &stg_MUT_VAR_DIRTY_info);
  mv->var = v;
  return wrap_word(ctx, &AK.ioref, (StgWord)mv);
}

// Ref.readForCas: the current value, as a ticket (WrapTicket's field is the
// value itself: Ticket is a newtype over it)
void *unison_jit_ref_read_for_cas(UnisonJitCtx *ctx, void *ref) {
  StgMutVar *mv = wrapped_of(ref, &AK.ioref);
  if (mv == NULL) return NULL;
  StgClosure *v = (StgClosure *)__atomic_load_n((StgWord *)&mv->var, __ATOMIC_ACQUIRE);
  return wrap_word(ctx, &AK.ticket, (StgWord)v);
}

// Ref.cas: casMutVar# on the ticket's value and a fresh Val; 1 or 0 for
// the boolean, -1 when the arguments aren't what they should be
int64_t unison_jit_ref_cas(UnisonJitCtx *ctx, void *ref, void *ticket, int64_t u, void *b) {
  StgMutVar *mv = wrapped_of(ref, &AK.ioref);
  StgWord old = wrapped_word(ticket, &AK.ticket);
  if (mv == NULL || old == 0) return -1;
  StgClosure *v = mk_val(ctx, b, u);
  if (!__atomic_compare_exchange_n((StgWord *)&mv->var, &old, (StgWord)v, 0, __ATOMIC_ACQ_REL, __ATOMIC_ACQUIRE)) return 0;
  if (GET_INFO((StgClosure *)mv) == &stg_MUT_VAR_CLEAN_info) dirty_MUT_VAR(reg_table(ctx->cap), mv, (StgClosure *)old);
  return 1;
}

// ---------------------------------------------------------------------------
// Universal.murmurHashUntyped: the hash itself is in jit_helpers.c (compiled
// to IR for generated code, and into the library for the startup check);
// here are its table, its trace output and the startup functions.

// why a value was left to the interpreter, under UNISON_JIT_TRACE_TEST
void unison_jit_murmur_fail(const char *what, StgClosure *c) {
  static int trace_test = -1;
  if (trace_test < 0) trace_test = getenv("UNISON_JIT_TRACE_TEST") != NULL;
  if (trace_test) {
    const StgInfoTable *it = c != NULL ? get_itbl(LUN(c)) : NULL;
    fprintf(stderr, "[test] murmur: not handled: %s, closure %p tag %d type %d\n", what, (void *)c, c ? (int)LTAG(c) : -1,
            it ? (int)it->type : -1);
  }
}

// Startup: elems[0] is a type tag closure (for its info pointer); info[0..6]
// the info pointers of the data constructors with 0 to 6 fields, info[7]
// DataG's.
int64_t unison_jit_murmur_init(void **elems, int64_t *info) {
  HK.ok = 0;
  StgClosure *t = unison_jit_settle(elems[0]);
  if (LTAG(t) == 0) return 0;
  HK.utag_info = (StgWord)LUN(t)->header.info;
  for (int n = 0; n <= DATA_MAX; n++) HK.data_info[n] = info[n];
  HK.datag_info = info[DATA_MAX + 1];
  HK.ok = LF.foreign_info != 0;
  return HK.ok;
}

// Startup check: the hash of the value in elems[0] (boxed), or of the unboxed
// value `u` with the type tag in elems[0] when `unboxed` is set. Writes the
// hash through `out` and returns whether the helper handled the value.
int64_t unison_jit_murmur_test_ffi(void **elems, int64_t u, int64_t unboxed, int64_t *out) {
  StgClosure *b = unison_jit_settle(elems[0]);
  static int trace_test = -1;
  if (trace_test < 0) trace_test = getenv("UNISON_JIT_TRACE_TEST") != NULL;
  if (trace_test) {
    const StgInfoTable *it = LTAG(b) ? get_itbl(LUN(b)) : NULL;
    fprintf(stderr, "[test] murmur: closure %p tag %d type %d ptrs %d nptrs %d info %p unboxed %lld u %lld\n", (void *)b, (int)LTAG(b),
            it ? (int)it->type : -1, it ? (int)it->layout.payload.ptrs : -1, it ? (int)it->layout.payload.nptrs : -1,
            LTAG(b) ? (void *)LUN(b)->header.info : NULL, (long long)unboxed, (long long)u);
    fflush(stderr);
  }
  JitNat r = unison_jit_murmur(unboxed ? u : -1, b);
  *out = r.v;
  return r.ok;
}

// ---------------------------------------------------------------------------
// Universal.== and Universal.compare (jit_helpers.c): the facts table, the
// trace output and the startup functions.

void unison_jit_cmp_fail(const char *what, StgClosure *c) {
  static int trace_test = -1;
  if (trace_test < 0) trace_test = getenv("UNISON_JIT_TRACE_TEST") != NULL;
  if (trace_test) {
    const StgInfoTable *it = c != NULL && LTAG(c) ? get_itbl(LUN(c)) : NULL;
    fprintf(stderr, "[test] compare: not handled: %s, closure %p tag %d type %d\n", what, (void *)c, c ? (int)LTAG(c) : -1,
            it ? (int)it->type : -1);
  }
}

// Startup: info[0] the PAp info pointer, info[1] and info[2] the packed
// tags of boxed Int and Nat, info[3] the type part of Any's packed tag.
int64_t unison_jit_cmp_init(int64_t *info) {
  CK.pap_info = info[0];
  CK.int_tag = info[1];
  CK.nat_tag = info[2];
  CK.any_type = info[3];
  CK.ok = HK.ok && LF.foreign_info != 0 && LF.val_info != 0;
  return CK.ok;
}

// Startup check: the two values are elems[0] and elems[1] (type tag closures
// with u1, u2 for unboxed ones: bits 0 and 1 of `flags`); bit 2 asks for
// equality instead of ordering. Writes the answer and returns whether the
// helper handled the pair.
int64_t unison_jit_cmp_test_ffi(void **elems, int64_t u1, int64_t u2, int64_t flags, int64_t *out) {
  StgClosure *b1 = unison_jit_settle(elems[0]), *b2 = unison_jit_settle(elems[1]);
  JitNat r = (flags & 4) ? unison_jit_ueq(u1, b1, u2, b2, 3) : unison_jit_ucmp(u1, b1, u2, b2, 3);
  *out = r.v;
  return r.ok;
}

// ---------------------------------------------------------------------------
// The handler environment: learns HEnv, IntMap, STRef and GAffine from
// samples and checks unison_jit_resolve_dyn on them. elems: [0] an HEnv
// whose aenv has keys 3, 10 and 77 and an empty denv, [1] that aenv, [2] an
// HEnv with the same aenv and key 10 in denv, [3] the STRef box of key 3's
// ref, [4] the value that ref holds, [5] the value key 10's ref holds,
// [6] an empty map, [7] an Affine closure over key 3's ref, [8] its set.
// Returns 1, or the number of the check that failed.

static int im_find(StgClosure *m, int64_t key, StgClosure **out, int depth); // jit_helpers.c, included below

static int is_mutvar(StgClosure *c) {
  if (c == NULL || LTAG(c) != 0) return 0;
  const StgInfoTable *info = GET_INFO(c);
  return info == &stg_MUT_VAR_CLEAN_info || info == &stg_MUT_VAR_DIRTY_info;
}

int64_t unison_jit_env_init(void **elems) {
  EK.ok = 0;
  StgClosure *h = unison_jit_settle(elems[0]), *aenv = unison_jit_settle(elems[1]), *h2 = unison_jit_settle(elems[2]);
  StgClosure *box = unison_jit_settle(elems[3]), *v3 = unison_jit_settle(elems[4]), *v10 = unison_jit_settle(elems[5]);
  StgClosure *empty = unison_jit_settle(elems[6]), *aff = unison_jit_settle(elems[7]), *set = unison_jit_settle(elems[8]);
  if (LTAG(h) == 0 || LTAG(aenv) == 0 || LTAG(h2) == 0 || LTAG(box) == 0 || LTAG(empty) == 0 || LTAG(aff) == 0 || LTAG(set) == 0) return 2;
  const StgInfoTable *hi = get_itbl(LUN(h));
  if (hi->layout.payload.ptrs != 2 || hi->layout.payload.nptrs != 0) return 3;
  if (LUN(LP(h, 0)) != LUN(aenv)) return 4;
  EK.henv_info = (StgWord)LUN(h)->header.info;
  EK.henv_tag = LTAG(h);
  // the constructors: the empty map is Nil (no fields; GHC pads a nullary
  // constructor with one non-pointer word); the root of a three-key map is
  // a Bin (the two subtrees, then the prefix word, or prefix and mask
  // before containers 0.7); its leftmost descendant is a Tip
  const StgInfoTable *ni = get_itbl(LUN(empty));
  if (ni->layout.payload.ptrs != 0 || ni->layout.payload.nptrs > 1) return 5;
  EK.nil_tag = LTAG(empty);
  const StgInfoTable *bi = get_itbl(LUN(aenv));
  if (bi->layout.payload.ptrs != 2 || bi->layout.payload.nptrs < 1 || bi->layout.payload.nptrs > 2) return 6;
  EK.bin_tag = LTAG(aenv);
  StgClosure *t = aenv;
  for (int i = 0; i < 64 && LTAG(t) == EK.bin_tag; i++) t = unison_jit_settle(LP(t, 0));
  if (LTAG(t) == 0 || LTAG(t) == EK.bin_tag) return 7;
  const StgInfoTable *ti = get_itbl(LUN(t));
  if (ti->layout.payload.ptrs != 1 || ti->layout.payload.nptrs != 1) return 8;
  EK.tip_tag = LTAG(t);
  if (EK.tip_tag == EK.bin_tag || EK.tip_tag == EK.nil_tag || EK.bin_tag == EK.nil_tag) return 9;
  // the ref: an STRef box around a MutVar#
  const StgInfoTable *si = get_itbl(LUN(box));
  if (LTAG(box) != 1 || si->layout.payload.ptrs != 1 || si->layout.payload.nptrs != 0) return 10;
  if (!is_mutvar(LP(box, 0))) return 11;
  EK.stref_info = (StgWord)LUN(box)->header.info;
  // the Affine closure: the set, the map, the MutVar# itself
  const StgInfoTable *ai = get_itbl(LUN(aff));
  if (ai->layout.payload.ptrs != 3 || ai->layout.payload.nptrs != 0) return 12;
  if (LUN(LP(aff, 0)) != LUN(set) || LUN(LP(aff, 1)) != LUN(aenv) || LP(aff, 2) != LP(box, 0)) return 13;
  EK.affine_info = (StgWord)LUN(aff)->header.info;
  EK.affine_tag = LTAG(aff);
  // the empty map is a nullary constructor, so its static closure, which
  // an InLocal's new HEnv can refer to without allocating it
  EK.empty_denv = empty;
  EK.ok = 1;
  // the lookups, through the walk itself
  StgClosure *f;
  if (im_find(aenv, 3, &f, 0) != 1 || LUN(f) != LUN(box)) return 14;
  if (im_find(aenv, 10, &f, 0) != 1 || im_find(aenv, 77, &f, 0) != 1) return 15;
  if (im_find(aenv, 1, &f, 0) != 0 || im_find(aenv, 11, &f, 0) != 0 || im_find(aenv, 100, &f, 0) != 0 || im_find(empty, 3, &f, 0) != 0) return 16;
  if (LUN(unison_jit_resolve_dyn(h, 3)) != LUN(v3) || LUN(unison_jit_resolve_dyn(h, 10)) != LUN(v10)) return 17;
  if (unison_jit_resolve_dyn(h, 4) != NULL || unison_jit_resolve_dyn(h, 0) != NULL) return 18;
  if (unison_jit_resolve_dyn(h2, 10) != NULL || LUN(unison_jit_resolve_dyn(h2, 3)) != LUN(v3)) return 19;
  if (unison_jit_resolve_dyn(aenv, 3) != NULL) return 20;
  return 1;
}

// SetAff: stores an affine handler's next state in the ref inside the
// Affine closure, through the runtime's barrier. 0 when `aff` isn't an
// Affine closure (the interpreter runs the instruction then).
int64_t unison_jit_set_aff(UnisonJitCtx *ctx, void *aff, void *v) {
  StgClosure *a = aff;
  if (!EK.ok) return 0;
  if (LTAG(a) == 0) a = unison_jit_settle(a);
  if (LTAG(a) != EK.affine_tag || (StgWord)LUN(a)->header.info != EK.affine_info) return 0;
  unison_jit_write_mutvar(ctx, (StgMutVar *)LP(a, 2), (StgClosure *)v);
  return 1;
}

// InLocal: the environment an affine handler's case runs in, HEnv of the
// Affine closure's aenv and the empty denv, as the interpreter builds it
// (Machine.hs). NULL when `aff` isn't an Affine closure (the interpreter
// runs the instruction then). The generated code stores the result in
// ctx->henv, and the old one in a register for the Yield and for the Local
// record of an exit in between.
void *unison_jit_local_env(UnisonJitCtx *ctx, void *aff) {
  StgClosure *a = aff;
  if (!EK.ok) return NULL;
  if (LTAG(a) == 0) a = unison_jit_settle(a);
  if (LTAG(a) != EK.affine_tag || (StgWord)LUN(a)->header.info != EK.affine_info) return NULL;
  StgWord *h = nat_alloc(ctx, 3);
  h[0] = EK.henv_info;
  h[1] = (StgWord)LP(a, 1);
  h[2] = (StgWord)EK.empty_denv;
  return (void *)((StgWord)h | (StgWord)EK.henv_tag);
}

// ---------------------------------------------------------------------------
// Closure layout probe (decision D7)

// elems[0] is a sample closure; elems[1..n-1] are the values in its pointer
// fields. Fills out[] with: info pointer, pointer tag, closure type, pointer
// field count, non-pointer field count, constructor tag, then for each
// payload word its raw value, then for each pointer field the index in elems
// of the object it points to (or -1). Returns the payload word count.
int64_t unison_jit_probe(StgClosure **elems, int64_t n, int64_t *out) {
  StgClosure *tagged = elems[0];
  StgClosure *c = UNTAG_CLOSURE(tagged);
  const StgInfoTable *itbl = get_itbl(c);
  int64_t ptrs = itbl->layout.payload.ptrs;
  int64_t nptrs = itbl->layout.payload.nptrs;
  out[0] = (int64_t)c->header.info;
  out[1] = GET_CLOSURE_TAG(tagged);
  out[2] = itbl->type;
  out[3] = ptrs;
  out[4] = nptrs;
  out[5] = itbl->srt;
  int64_t total = ptrs + nptrs;
  for (int64_t i = 0; i < total; i++) out[6 + i] = (int64_t)c->payload[i];
  for (int64_t i = 0; i < ptrs; i++) {
    StgClosure *field = UNTAG_CLOSURE(c->payload[i]);
    out[6 + total + i] = -1;
    for (int64_t k = 1; k < n; k++)
      if (UNTAG_CLOSURE(elems[k]) == field) out[6 + total + i] = k;
  }
  return total;
}

// For tracing: the current value of HpLim, as native code would see it.
int64_t unison_jit_hplim_value(void) { return (int64_t)*unison_jit_hplim_address(); }

// ---- Exits
//
// The write-back routine generated code calls at every exit and unwind,
// unison_jit_exit_frame, is IR (cbits/jit_exit.c, embedded as jit_exit.ll
// and compiled by the JIT at startup): it needs the preserve_most
// convention, which only clang would give a C copy. This is what it calls
// when an exit needs more Local frame records than the out-area holds.
void unison_jit_exit_fail(void) {
  fprintf(stderr, "[jit] more Local frame records than the out-area holds\n");
  abort();
}

// ---------------------------------------------------------------------------
// Records ("records" in jit_internal.h): the facts table's startup function,
// the trace output and the update helper. The position lookup is in
// jit_helpers.c.

void unison_jit_rec_fail(const char *what, StgClosure *c) {
  static int trace_test = -1;
  if (trace_test < 0) trace_test = getenv("UNISON_JIT_TRACE_TEST") != NULL;
  if (trace_test) {
    const StgInfoTable *it = c != NULL && LTAG(c) ? get_itbl(LUN(c)) : NULL;
    fprintf(stderr, "[test] record: not handled: %s, closure %p tag %d type %d\n", what, (void *)c, c ? (int)LTAG(c) : -1,
            it ? (int)it->type : -1);
  }
}

// Startup: learns the record layout from samples and checks the lookup.
// elems: [0] a record of the fields "a" and "bc" (field numbers info[0] <
// info[1], shape reference info[2]) holding the Vals [1] and [2]; [3] a
// record with no fields; [4] a record with three fields (numbers info[3] <
// info[4] < info[5] in slots 0, 1, 2). Returns 1, or the number of the
// check that failed.
int64_t unison_jit_record_init(void **elems, int64_t *info) {
  RK.ok = 0;
  if (LF.val_info == 0) return 2;
  StgClosure *r = unison_jit_settle(elems[0]), *v0 = unison_jit_settle(elems[1]), *v1 = unison_jit_settle(elems[2]);
  StgClosure *r0 = unison_jit_settle(elems[3]), *r3 = unison_jit_settle(elems[4]);
  if (LTAG(r) != 7 || LTAG(r0) != 7 || LTAG(r3) != 7 || LTAG(v0) == 0 || LTAG(v1) == 0) return 3;
  const StgInfoTable *ri = get_itbl(LUN(r));
  if (ri->layout.payload.ptrs != 4 || ri->layout.payload.nptrs != 5) return 4;
  if (LUN(r0)->header.info != LUN(r)->header.info || LUN(r3)->header.info != LUN(r)->header.info) return 5;
  // the words: the shape reference, the names' and the values' offsets and lengths
  if (LW(r, REC_REF) != info[2] || LW(r, REC_NOFF) != 0 || LW(r, REC_NLEN) != 2 || LW(r, REC_VOFF) != 0 || LW(r, REC_VLEN) != 2) return 6;
  if (LW(r0, REC_NLEN) != 0 || LW(r0, REC_VLEN) != 0 || LW(r3, REC_NLEN) != 3 || LW(r3, REC_VLEN) != 3) return 7;
  // the values: an Array# of Val closures
  StgMutArrPtrs *vals = (StgMutArrPtrs *)LP(r, REC_VALS);
  if (get_itbl((StgClosure *)vals)->type < MUT_ARR_PTRS_CLEAN || get_itbl((StgClosure *)vals)->type > MUT_ARR_PTRS_FROZEN_CLEAN) return 8;
  if (vals->ptrs != 2) return 9;
  StgClosure *e0 = unison_jit_settle(vals->payload[0]), *e1 = unison_jit_settle(vals->payload[1]);
  if (LTAG(e0) != 1 || LTAG(e1) != 1 || (StgWord)LUN(e0)->header.info != LF.val_info || (StgWord)LUN(e1)->header.info != LF.val_info) return 10;
  // the same values as the Vals handed over (the builder copies them)
  if (LW(e0, 1) != LW(v0, 1) || LW(e1, 1) != LW(v1, 1) || LUN(LP(e0, 0)) != LUN(LP(v0, 0)) || LUN(LP(e1, 0)) != LUN(LP(v1, 0))) return 11;
  // the names: an Array# of Data.Text, "a" then "bc"
  StgMutArrPtrs *names = (StgMutArrPtrs *)LP(r, REC_NAMES);
  if (get_itbl((StgClosure *)names)->type < MUT_ARR_PTRS_CLEAN || get_itbl((StgClosure *)names)->type > MUT_ARR_PTRS_FROZEN_CLEAN || names->ptrs != 2) return 12;
  StgClosure *ta = unison_jit_settle(names->payload[0]), *tb = unison_jit_settle(names->payload[1]);
  if (LTAG(ta) == 0 || LTAG(tb) == 0 || LUN(ta)->header.info != LUN(tb)->header.info) return 13;
  const StgInfoTable *ti = get_itbl(LUN(ta));
  if (ti->layout.payload.ptrs != 1 || ti->layout.payload.nptrs != 2) return 14;
  if (get_itbl(LP(ta, 0))->type != ARR_WORDS || LW(ta, 2) != 1 || LW(tb, 2) != 2) return 15;
  const unsigned char *pa = (const unsigned char *)((StgArrBytes *)LP(ta, 0))->payload + LW(ta, 1);
  const unsigned char *pb = (const unsigned char *)((StgArrBytes *)LP(tb, 0))->payload + LW(tb, 1);
  if (pa[0] != 'a' || pb[0] != 'b' || pb[1] != 'c') return 16;
  // the field numbers and the slots: two ByteArray#s of words, the numbers
  // ascending with the slot of each at the same index
  StgArrBytes *keys = (StgArrBytes *)LUN(unison_jit_settle(LP(r, REC_KEYS))), *slots = (StgArrBytes *)LUN(unison_jit_settle(LP(r, REC_SLOTS)));
  StgArrBytes *keys0 = (StgArrBytes *)LUN(unison_jit_settle(LP(r0, REC_KEYS))), *keys3 = (StgArrBytes *)LUN(unison_jit_settle(LP(r3, REC_KEYS)));
  if (get_itbl((StgClosure *)keys)->type != ARR_WORDS || get_itbl((StgClosure *)slots)->type != ARR_WORDS || get_itbl((StgClosure *)keys0)->type != ARR_WORDS) return 17;
  if (keys->bytes != 2 * sizeof(StgWord) || slots->bytes != 2 * sizeof(StgWord) || keys0->bytes != 0 || keys3->bytes != 3 * sizeof(StgWord)) return 18;
  // the sample's numbers are given ascending (info[0] < info[1]), so the slots are 0, 1
  const int64_t *kw = (const int64_t *)keys->payload, *sw = (const int64_t *)slots->payload;
  if (kw[0] != info[0] || kw[1] != info[1] || sw[0] != 0 || sw[1] != 1) return 19;
  RK.record_info = (StgWord)LUN(r)->header.info;
  RK.text_info = (StgWord)LUN(ta)->header.info;
  RK.ok = 1;
  // the lookup, through the helper itself
  if (unison_jit_rec_pos(r, info[0]) != 0 || unison_jit_rec_pos(r, info[1]) != 1) return 20;
  if (unison_jit_rec_pos(r, info[0] + info[1] + 1) != -1 || unison_jit_rec_pos(r, info[0] - 1) != -1 || unison_jit_rec_pos(r0, info[0]) != -1) return 21;
  if (unison_jit_rec_pos(r3, info[3]) != 0 || unison_jit_rec_pos(r3, info[4]) != 1 || unison_jit_rec_pos(r3, info[5]) != 2 || unison_jit_rec_pos(r3, info[5] + 1) != -1) return 22;
  if (!is_record(r) || !is_record(r0) || is_record(v0) || is_record(ta)) return 23;
  return 1;
}

// RecSet: a copy of the record with the field's slot holding (u, b), or NULL
// when the closure isn't a record or has no such field (the interpreter runs
// the instruction then). The copy shares the shape; its values are a new
// frozen Array# of the old elements with one new Val. An untagged pointer
// must not go into a Val's strict field (see requireTagged in Codegen.hs).
void *unison_jit_rec_set(UnisonJitCtx *ctx, void *rec, int64_t field, int64_t u, void *b) {
  if (!is_record(rec) || LTAG(b) == 0) return NULL;
  int64_t pos = unison_jit_rec_pos(rec, field);
  if (pos < 0) return NULL;
  StgClosure *c = rec;
  StgMutArrPtrs *old = (StgMutArrPtrs *)LP(c, REC_VALS);
  StgWord voff = LW(c, REC_VOFF), n = LW(c, REC_VLEN);
  if ((StgWord)pos >= n) return NULL;
  StgClosure *nv = mk_val(ctx, b, u);
  StgMutArrPtrs *a = parray_new(ctx, n, nv, &stg_MUT_ARR_PTRS_FROZEN_CLEAN_info);
  for (StgWord i = 0; i < n; i++)
    if (i != (StgWord)pos) a->payload[i] = old->payload[voff + i];
  StgWord *p = list_alloc(ctx, 10);
  p[0] = RK.record_info;
  p[1] = (StgWord)LP(c, REC_NAMES);
  p[2] = (StgWord)LP(c, REC_KEYS);
  p[3] = (StgWord)LP(c, REC_SLOTS);
  p[4] = (StgWord)a;
  p[5] = LW(c, REC_REF);
  p[6] = LW(c, REC_NOFF);
  p[7] = LW(c, REC_NLEN);
  p[8] = 0;
  p[9] = n;
  return (void *)((StgWord)p | 7);
}

// The helpers that generated code calls as IR, compiled here into the
// library as well (they read the tables above, which are private to this
// file); cbits/gen-helpers.sh compiles the same file on its own to IR.
#include "jit_helpers.c"
