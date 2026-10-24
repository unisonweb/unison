// The JIT's C runtime: the machinery, and the pieces of the GHC runtime it needs. See
// unison-runtime/src/Unison/Runtime/JIT/design.md.
//
// This file is entering native code (the trampoline's C side, the contexts, the C stack
// bounds), allocation, the partial-application helpers, the handler environment, the closure
// layout probe and the exits. The native builtins are one file per family, included at the
// end of this file so that the whole runtime is one translation unit (what the files share
// stays static and inlinable; jit_builtins.h declares it): jit_builtins_math.c, _lists.c, _ropes.c (the finger tree
// behind Text and Bytes), _text.c, _bytes.c, _arrays.c (arrays and refs), _compare.c (the
// facts tables of the hash and the universal comparison, whose code is jit_helpers.c) and
// _records.c.
//
// The build doesn't know this file depends on what it includes (the headers and the
// jit_builtins_*.c files): after changing any of them, `touch` this file before building, or
// the stale object stays in the binary (see development.md, "Building").

// glibc: pthread_getattr_np (thread_stack_bounds) is a GNU extension
#ifndef _GNU_SOURCE
#define _GNU_SOURCE
#endif
#include "Rts.h"
#include "jit_rt.h"
#define UNISON_JIT_TABLES_HERE
#include "jit_internal.h"
#include "jit_builtins.h"

#if defined(_WIN32)
#define WIN32_LEAN_AND_MEAN
#define NOMINMAX
#define _WIN32_WINNT 0x0602  // GetCurrentThreadStackLimits is Windows 8
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
static int64_t jit_trace = 0;
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
  jit_trace = trace_on;
  const char *bump = getenv("UNISON_JIT_BUMP");
  bump_alloc = !(bump != NULL && strcmp(bump, "0") == 0);
}

// memmem, which Windows' C library lacks: the first occurrence of needle
// in hay, or NULL. An empty needle is found at the start.
static const unsigned char *mem_find(const unsigned char *hay, StgInt hlen, const unsigned char *needle, StgInt nlen) {
  if (nlen == 0) return hay;
  if (nlen > hlen) return NULL;
  for (const unsigned char *p = hay, *end = hay + hlen - nlen + 1; p < end; p++) {
    p = memchr(p, needle[0], (size_t)(end - p));
    if (p == NULL) return NULL;
    if (memcmp(p, needle, (size_t)nlen) == 0) return p;
  }
  return NULL;
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


// The slow path for an object whose size a program chose (a new array, a
// repeated text): allocateMightFail instead of allocate(), so that a size
// no heap can hold comes back as NULL for the caller to hand to the
// interpreter, which raises HeapOverflow as newByteArray# would, where
// allocate() exits the process (security review, 2026-10-24). The budget
// is charged only for what was handed out.
void *unison_jit_alloc_words_mayfail(UnisonJitCtx *ctx, int64_t n) {
  if (!ctx->bump) {
    void *p = allocateMightFail((Capability *)ctx->cap, n);
    if (p != NULL) ctx->hp = (void *)((uintptr_t)ctx->hp + (uintptr_t)n * sizeof(StgWord));
    return p;
  }
  int64_t left = budget_left(ctx);
  alloc_sync(ctx);
  void *p = allocateMightFail((Capability *)ctx->cap, n);
  if (p != NULL) ctx->slow_words += n, left -= n;
  alloc_refresh(ctx, left);
  return p;
}

// Writes a MutVar# (an IORef) the way compiled Haskell does: the store,
// then the runtime's barrier, which moves a clean variable onto the
// mutable list. Cold code in C.
void unison_jit_write_mutvar(UnisonJitCtx *ctx, StgMutVar *mv, StgClosure *v) {
  StgClosure *old = mv->var;
  __atomic_thread_fence(__ATOMIC_RELEASE);
  mv->var = v;
  // the register table follows the function table at the start of a Capability
  // (see unison_jit_hplim_address)
  StgRegTable *reg = (StgRegTable *)((char *)ctx->cap + sizeof(StgFunTable));
  // only a clean variable needs the barrier, as compiled code tests before
  // the call (the barrier tests again; this keeps the cold call off a dirty var)
  if (GET_INFO((StgClosure *)mv) == &stg_MUT_VAR_CLEAN_info) dirty_MUT_VAR(reg, mv, old);
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

void unison_jit_closure_init(int64_t *info) { PF_pap_info = info[0], PF_pap_tag = info[1]; }

// NULL if `fun` isn't a PAp. The `n` new arguments are on the Unison stack,
// a word and a pointer each, at slots base .. base + n - 1, where the
// generated code put them; it passes them this way rather than as C
// arguments so that there is no limit on their number. (A partial
// application of a function known when the code is generated is built
// inline instead: Codegen's genNameInline.)
void *unison_jit_name(UnisonJitCtx *ctx, void *fun, int64_t n, int64_t base) {
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
  for (int64_t k = 0; k < n; k++) {
    nus->payload[n - 1 - k] = ctx->ustk[base + k];
    nbs->payload[n - 1 - k] = (StgClosure *)ctx->bstk[base + k];
  }
  memcpy(&nus->payload[n], us->payload, old * sizeof(StgWord));
  memcpy(&nbs->payload[n], bs->payload, old * sizeof(StgWord));
  return (void *)((StgWord)p | PF_pap_tag);
}

// For the startup self-test: Name on elems[0] with `n` arguments (at most
// 8), the k-th being the word 100 + k and the closure elems[1]; the result
// goes in elems[3]. The arguments are put on a stack of the test's own.
int64_t unison_jit_name_test(void **elems, int64_t n) {
  UnisonJitCtx tmp = {0}, *ctx = &tmp;
  int64_t u[8];
  void *b[8];
  if (n > 8) return 0;
  for (int64_t k = 0; k < n; k++) u[k] = 100 + k, b[k] = elems[1];
  ctx->ustk = u, ctx->bstk = b;
  ctx->cap = rts_unsafeGetMyCapability();
  void *res = unison_jit_name(ctx, elems[0], n, 0);
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
      if (jit_trace) fprintf(stderr, "[jit] new context: the thread's stack bounds are unknown on this platform\n");
    }
    if (jit_trace)
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
  if (__builtin_expect(jit_trace, 0))
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
  if (__builtin_expect(stress_poll > 0 || stress_callee > 0 || jit_trace, 0)) {
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
      if (copy == NULL) {
        fprintf(stderr, "[jit] out of memory copying %lld frame records\n", (long long)n);
        abort();
      }
      memcpy(copy, ctx->frames, bytes);
      out[4] = (int64_t)copy;
    }
    if (jit_trace) fprintf(stderr, "[jit] %lld frame records\n", (long long)n);
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
// Closure layout probe

// elems[0] is a sample closure; elems[1..n-1] are the values in its pointer
// fields. Fills out[] with: info pointer, pointer tag, closure type, pointer
// field count, non-pointer field count, constructor tag, then for each
// payload word its raw value, then for each pointer field the index in elems
// of the object it points to (or -1). Returns the payload word count.
// Writes at most `cap` words of out; the return value says how many a
// complete answer takes (6 + 2 * payload words at most), so a caller with a
// small buffer can tell it was cut short.
int64_t unison_jit_probe(StgClosure **elems, int64_t n, int64_t *out, int64_t cap) {
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
  if (6 + total + ptrs > cap) return total;
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
  fprintf(stderr, "[jit] an exit needs more frame records than the context holds (Local records past the out-area, or more records than the C stack budget allows)\n");
  abort();
}

// The native builtins, one file per family, as part of this translation unit: what they
// share stays static and inlinable (jit_builtins.h declares it). The order follows the
// dependencies: the ropes are deques of chunks, text and bytes are rope operations, and
// records allocate pointer arrays.
#include "jit_builtins_math.c"
#include "jit_builtins_lists.c"
#include "jit_builtins_ropes.c"
#include "jit_builtins_text.c"
#include "jit_builtins_bytes.c"
#include "jit_builtins_arrays.c"
#include "jit_builtins_compare.c"
#include "jit_builtins_records.c"
#include "jit_helpers.c"
