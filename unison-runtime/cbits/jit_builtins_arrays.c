// Part of the JIT's C runtime, included by jit_rt.c (the files are one translation unit): see
// jit_rt.c for the whole, jit_builtins.h for what the parts share, and
// unison-runtime/src/Unison/Runtime/JIT/design.md for the design.
#include "jit_builtins.h"

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

