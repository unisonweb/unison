// Helpers that generated code calls as LLVM IR rather than as C.
//
// This file is compiled twice: into the library as part of jit_rt.c, which
// includes it at its end, and by clang on its own into cbits/jit_helpers.ll
// (cbits/gen-helpers.sh), which the JIT adds as a module of its own at
// startup. Generated code resolves `unison_jit_murmur`, `unison_jit_ueq`
// and `unison_jit_ucmp` to the IR copies, which the JIT compiles for the same target as the caller,
// so the two agree on how the `{ i64, i64 }` result comes back on every
// platform; the C copy is what the startup self-check runs and the fallback
// under UNISON_JIT_DISABLE=irhelpers. Whatever is here must keep the IR
// platform-independent: see jit_internal.h and "The IR helpers" in
// design.md. The tables and walkers it needs are exported from jit_rt.c.
//
// Universal.murmurHashUntyped
//
// The interpreter reflects the value into an ANF Value tree (Value.value) and
// hashes the tree with MurmurHash64A, one 64-bit word at a time, a small
// constructor number ahead of each node (Unison.Runtime.ANF.MurmurHash.Untyped).
// This walks the closures directly and feeds the same words to the same
// accumulator, so it gives the same hash without building the tree. Data
// constructors hash their tag (the low 16 bits of the packed tag) and fields,
// not their type reference; unboxed values hash as the literals the reflection
// turns them into; Text, Bytes, lists, arrays and byte arrays hash their
// contents. Anything else (a function, a map, a link, quoted code, a
// continuation, a big number, an unevaluated pointer) is left to the
// interpreter: the helper says "not handled".
//
// The seed (0xdeadbeef) and the finalization are the murmur-hash library's
// (Data.Digest.Murmur64: hash64 = hash64End . hash64Add x . Hash64 defaultSeed).

#include "jit_internal.h"

#define MURMUR_M 0xc6a4a7935bd1e995ULL

// the library's step: the accumulator is multiplied, then xored with the
// mixed word (Data.Digest.Murmur64.hash64AddWord64)
static inline uint64_t mm_add(uint64_t h, uint64_t k) {
  k *= MURMUR_M;
  k ^= k >> 47;
  k *= MURMUR_M;
  return (h * MURMUR_M) ^ k;
}

static inline uint64_t mm_end(uint64_t h) {
  h ^= h >> 47;
  h *= MURMUR_M;
  h ^= h >> 47;
  return h;
}

#define MURMUR_SEED 0xdeadbeefULL

typedef struct {
  uint64_t h;
  int ok;
} MH;

static inline void mh_word(MH *s, uint64_t k) { s->h = mm_add(s->h, k); }

static void mh_closure(MH *s, StgClosure *c);

// a Val's two halves: an unboxed value (b is a type tag) or a boxed one

static void mh_val(MH *s, StgWord u, StgClosure *b) {
  if (!s->ok) return;
  if (LTAG(b) == 0) {
    unison_jit_murmur_fail("untagged value", b);
    s->ok = 0;
    return;
  }
  if ((StgWord)LUN(b)->header.info == HK.utag_info) {
    // the literal the reflection makes of it: BLit (Pos/Neg/Char/Float)
    int kind = LTAG(LP(b, 0)); // CharTag 1, FloatTag 2, IntTag 3, NatTag 4
    mh_word(s, 4);
    switch (kind) {
      case 1: mh_word(s, 12); mh_word(s, u); break;
      case 2: mh_word(s, 13); mh_word(s, u); break;
      case 3:
        if ((int64_t)u >= 0) mh_word(s, 10), mh_word(s, u);
        else mh_word(s, 11), mh_word(s, (uint64_t)(-(int64_t)u));
        break;
      case 4: mh_word(s, 10); mh_word(s, u); break;
      default: unison_jit_murmur_fail("type tag", b); s->ok = 0;
    }
    return;
  }
  mh_closure(s, b);
}

// a Val closure (a list or array element)
static void mh_val_closure(StgClosure *item, void *env) {
  MH *s = env;
  if (LTAG(item) != 1 || (StgWord)LUN(item)->header.info != LF.val_info) {
    unison_jit_murmur_fail("list element", item);
    s->ok = 0;
    return;
  }
  mh_val(s, LW(item, 1), LP(item, 0));
}

// the code points of UTF-8 bytes
static void mh_utf8(MH *s, const unsigned char *p, const unsigned char *e) {
  while (p < e) {
    uint64_t cp;
    if (p[0] < 0x80) cp = p[0], p += 1;
    else if (p[0] < 0xE0) cp = ((p[0] & 0x1Fu) << 6) | (p[1] & 0x3Fu), p += 2;
    else if (p[0] < 0xF0) cp = ((p[0] & 0x0Fu) << 12) | ((p[1] & 0x3Fu) << 6) | (p[2] & 0x3Fu), p += 3;
    else cp = ((p[0] & 0x07u) << 18) | ((p[1] & 0x3Fu) << 12) | ((p[2] & 0x3Fu) << 6) | (p[3] & 0x3Fu), p += 4;
    mh_word(s, cp);
  }
}

// the code points of a text, chunk by chunk
static void mh_text(MH *s, StgClosure *r) {
  const RopeKind *K = &TK;
  StgInt n = rope_size(K, r), off;
  for (StgInt i = 0; i < n;) {
    StgClosure *c = unison_jit_rope_chunk_at(K, r, i, &off);
    const unsigned char *p = chunk_bytes(K, c);
    mh_utf8(s, p, p + chunk_len(K, c));
    i += chunk_size(K, c);
  }
}

// a record: 5, the code points of each field name in order, then the values
static void mh_record(MH *s, StgClosure *c) {
  mh_word(s, 5);
  StgMutArrPtrs *names = (StgMutArrPtrs *)LP(c, REC_NAMES);
  StgWord noff = LW(c, REC_NOFF), nlen = LW(c, REC_NLEN);
  for (StgWord i = 0; i < nlen && s->ok; i++) {
    StgClosure *t = names->payload[noff + i];
    if (LTAG(t) == 0) t = unison_jit_settle(t);
    if (LTAG(t) == 0 || (StgWord)LUN(t)->header.info != RK.text_info) {
      unison_jit_murmur_fail("field name", t);
      s->ok = 0;
      return;
    }
    const unsigned char *p = (const unsigned char *)((StgArrBytes *)LP(t, 0))->payload + LW(t, 1);
    mh_utf8(s, p, p + LW(t, 2));
  }
  StgMutArrPtrs *vals = (StgMutArrPtrs *)LP(c, REC_VALS);
  StgWord voff = LW(c, REC_VOFF), vlen = LW(c, REC_VLEN);
  for (StgWord i = 0; i < vlen && s->ok; i++) {
    StgClosure *v = vals->payload[voff + i];
    mh_val_closure(LTAG(v) == 0 ? unison_jit_settle(v) : v, s);
  }
}

static void mh_bytes(MH *s, StgClosure *r) {
  const RopeKind *K = &BK;
  StgInt n = rope_size(K, r), off;
  for (StgInt i = 0; i < n;) {
    StgClosure *c = unison_jit_rope_chunk_at(K, r, i, &off);
    const unsigned char *p = chunk_bytes(K, c);
    StgInt len = chunk_len(K, c);
    for (StgInt k = 0; k < len; k++) mh_word(s, p[k]);
    i += chunk_size(K, c);
  }
}

// A constructor with three or more fields, out of line so that the common
// shapes stay straight-line code in mh_closure (which mh_val inlines).
__attribute__((noinline)) static void mh_data_wide(MH *s, StgClosure *c, int arity) {
  mh_word(s, 2);
  if (arity <= DATA_MAX) {
    mh_word(s, DATA_TAG(c, arity) & 0xFFFF);
    for (int j = 0; j < arity && s->ok; j++) mh_val(s, DATA_U(c, arity, j), DATA_B(c, arity, j));
    return;
  }
  // DataG: the arrays hold the fields in reverse
  mh_word(s, LW(c, 3) & 0xFFFF);
  StgArrBytes *us = (StgArrBytes *)LP(c, 0);
  StgMutArrPtrs *bs = (StgMutArrPtrs *)LP(c, 1);
  StgWord n = bs->ptrs;
  for (StgWord j = 0; j < n && s->ok; j++) {
    StgWord i = n - 1 - j;
    mh_val(s, ((StgWord *)us->payload)[i], bs->payload[i]);
  }
}

static void mh_closure(MH *s, StgClosure *c) {
  if (!s->ok) return;
  int tag = LTAG(c);
  if (tag == 0) {
    unison_jit_murmur_fail("untagged closure", c);
    s->ok = 0;
    return;
  }
  StgWord info = (StgWord)LUN(c)->header.info;
  int arity = data_arity(info);
  if (arity == 0) {
    mh_word(s, 2);
    mh_word(s, DATA_TAG(c, 0) & 0xFFFF);
  } else if (arity == 1) {
    mh_word(s, 2);
    mh_word(s, DATA_TAG(c, 1) & 0xFFFF);
    mh_val(s, DATA_U(c, 1, 0), DATA_B(c, 1, 0));
  } else if (arity == 2) {
    mh_word(s, 2);
    mh_word(s, DATA_TAG(c, 2) & 0xFFFF);
    mh_val(s, DATA_U(c, 2, 0), DATA_B(c, 2, 0));
    mh_val(s, DATA_U(c, 2, 1), DATA_B(c, 2, 1));
  } else if (arity > 0) {
    mh_data_wide(s, c, arity);
  } else if (RK.ok && info == RK.record_info) {
    mh_record(s, c);
  } else if (is_builtin_info(info)) {
    StgClosure *r;
    if ((r = rope_of(&TK, c)) != NULL) {
      mh_word(s, 4);
      mh_word(s, 1);
      mh_text(s, r);
    } else if ((r = rope_of(&BK, c)) != NULL) {
      mh_word(s, 4);
      mh_word(s, 5);
      mh_bytes(s, r);
    } else if ((r = deque_of(c)) != NULL) {
      mh_word(s, 4);
      mh_word(s, 2);
      unison_jit_lv_each(r, 1, 0, mh_val_closure, s);
    } else if (AK.ok && wrapped_of(c, &AK.barray) != NULL) {
      StgArrBytes *a = wrapped_of(c, &AK.barray);
      mh_word(s, 4);
      mh_word(s, 8);
      for (StgWord i = 0; i < a->bytes; i++) mh_word(s, ((unsigned char *)a->payload)[i]);
    } else if (AK.ok && wrapped_of(c, &AK.array) != NULL) {
      StgMutArrPtrs *a = wrapped_of(c, &AK.array);
      mh_word(s, 4);
      mh_word(s, 9);
      for (StgWord i = 0; i < a->ptrs && s->ok; i++) mh_val_closure(a->payload[i], s);
    } else {
      unison_jit_murmur_fail("foreign", c);
      s->ok = 0; // a map, a link, a big number, quoted code...: the interpreter's
    }
  } else {
    unison_jit_murmur_fail("closure kind", c);
    s->ok = 0; // a function, a continuation, a black hole
  }
}

static JitNat mh_run(StgWord u, StgClosure *b) {
  if (!HK.ok) return (JitNat){0, 0};
  MH s = {MURMUR_SEED, 1};
  mh_val(&s, u, b);
  if (!s.ok) return (JitNat){0, 0};
  return (JitNat){1, (int64_t)mm_end(s.h)};
}

// Generated code calls this with a stack slot's two halves.
JitNat unison_jit_murmur(int64_t u, void *b) { return mh_run((StgWord)u, (StgClosure *)b); }


// ---------------------------------------------------------------------------
// Ability requests: the handler for an ability number, from the handler
// environment ("The environment in the context" under "Ability requests" in design.md).

// Whether `key` is in the map: 1 and the value through `out`, 0 when it is
// absent, -1 when a node on the way isn't an evaluated IntMap constructor.
// Every node is visited rather than following the prefix bits, which makes
// the walk independent of the prefix encoding (it changed in containers
// 0.7); the maps hold a handful of handlers.
static int im_find(StgClosure *m, int64_t key, StgClosure **out, int depth) {
  int t = LTAG(m);
  if (t == EK.nil_tag) return 0;
  if (t == EK.tip_tag) {
    if (LW(m, 1) != key) return 0;
    *out = LP(m, 0);
    return 1;
  }
  if (t != EK.bin_tag || depth > 64) return -1;
  int r = im_find(LP(m, 0), key, out, depth + 1);
  if (r != 0) return r;
  return im_find(LP(m, 1), key, out, depth + 1);
}

void *unison_jit_resolve_dyn(void *henv, int64_t key) {
  StgClosure *h = henv, *found;
  if (!EK.ok) return NULL;
  // the interpreter hands over whatever pointer it holds: an evaluated
  // record may be behind an indirection
  if (LTAG(h) == 0) h = unison_jit_settle(h);
  if (LTAG(h) != EK.henv_tag || (StgWord)LUN(h)->header.info != EK.henv_info) return NULL;
  // the record's fields are lazy: an evaluated one may still be reached
  // through an indirection (a CAF for the empty map, say)
  StgClosure *aenv = LP(h, 0), *denv = LP(h, 1);
  if (LTAG(aenv) == 0) aenv = unison_jit_settle(aenv);
  if (LTAG(denv) == 0) denv = unison_jit_settle(denv);
  // a non-affine handler for the key shadows an affine one
  if (LTAG(denv) != EK.nil_tag && im_find(denv, key, &found, 0) != 0) return NULL;
  if (im_find(aenv, key, &found, 0) != 1) return NULL;
  // the Tip's value field is lazy: the pointer to the ref may be untagged
  if (LTAG(found) == 0) found = unison_jit_settle(found);
  if (LTAG(found) != 1 || (StgWord)LUN(found)->header.info != EK.stref_info) return NULL;
  StgMutVar *mv = (StgMutVar *)LP(found, 0);
  StgClosure *v = (StgClosure *)__atomic_load_n((StgWord *)&mv->var, __ATOMIC_ACQUIRE);
  // the interpreter writes the ref with whatever pointer it holds, which
  // may be an untagged pointer to an evaluated closure, or an indirection
  return LTAG(v) == 0 ? unison_jit_settle(v) : v;
}

// ---------------------------------------------------------------------------
// Records: the slot of a field ("records" in jit_internal.h).

int64_t unison_jit_rec_pos(void *rec, int64_t field) {
  // the two ByteArray#s are strict, evaluated fields; the GC never leaves an
  // indirection to an ARR_WORDS in a strict field of a value it has built,
  // but settle anyway in case a pointer is still untagged
  StgClosure *kc = LP(rec, REC_KEYS), *sc = LP(rec, REC_SLOTS);
  if (LTAG(kc) == 0) kc = unison_jit_settle(kc);
  if (LTAG(sc) == 0) sc = unison_jit_settle(sc);
  StgArrBytes *keys = (StgArrBytes *)LUN(kc), *slots = (StgArrBytes *)LUN(sc);
  const int64_t *k = (const int64_t *)keys->payload;
  int64_t lo = 0, hi = (int64_t)(keys->bytes / sizeof(int64_t));
  while (lo < hi) {
    int64_t mid = lo + (hi - lo) / 2, km = k[mid];
    if (km < field) lo = mid + 1;
    else if (km > field) hi = mid;
    else return ((const int64_t *)slots->payload)[mid];
  }
  return -1;
}

// ---------------------------------------------------------------------------
// Universal.== and Universal.compare
//
// The interpreter's `Eq Val` and `Ord Val` (compareVal False, Unison.Runtime.Stack),
// quirks included, since the output must be the same:
//
// - A Val is unboxed (its boxed half is a type tag closure: Char 1, Float 2,
//   Int 3, Nat 4 by pointer tag) or boxed. Equal unboxed values have matching
//   tags (Int and Nat match each other) and equal words; an unboxed value is
//   never equal to a boxed one and orders below it. Int/Int is signed,
//   Nat/Nat unsigned, a Nat against a negative Int is greater and otherwise
//   unsigned, Float/Float is compareAsFloat on the bits (signed when either
//   is positive, else with the sign bits cleared and backwards), and any
//   other pair is signed on the words, after the tags only under tyEq.
// - Data orders by constructor index (the low 16 bits of the packed tag),
//   then field count, then fields; equality compares the whole packed tag,
//   type included, except that one-field constructors with the boxed Int
//   and Nat tags match each other. The fields of an Any compare with tyEq;
//   under tyEq, different packed tags compare the type references, which
//   is left to the interpreter.
// - Different closure kinds order as closureNum (PAp 0, data 1, Foreign 3,
//   record 7) and are never equal. A PAp compares its CombIx (reference
//   pointer, group, index) then its arguments; different reference pointers
//   are left to the interpreter.
// - Records: equal when their shapes are equal (the shape reference is
//   interned per set of field names, so the references say so) and their
//   values are, slot by slot. Ordering compares the lengths then the slots;
//   under tyEq (inside an Any) two records of different shapes order by
//   their field names, which is left to the interpreter.
// - Foreign values: two texts or two bytes through the rope helpers, two
//   lists or two arrays element by element with a prefix smaller, two byte
//   arrays by their bytes then their lengths, two refs or mutable arrays
//   equal when they are the same object and not orderable. A map, two
//   different foreign kinds, anything else: the interpreter's.
// - The same closure is equal to itself (the interpreter compares bits).

typedef struct {
  int ok;
  int64_t kinds; // bit 0: texts handled here, bit 1: bytes (UNISON_JIT_DISABLE=text,bytes)
} UC;

#define UK_PAP 0
#define UK_DATA 1
#define UK_FOREIGN 3
#define UK_RECORD 7

static int uc_kind(UC *s, StgClosure *c) {
  StgWord info = (StgWord)LUN(c)->header.info;
  if (data_arity(info) >= 0) return UK_DATA;
  if (is_builtin_info(info)) return UK_FOREIGN;
  if (CK.pap_info != 0 && info == CK.pap_info) return UK_PAP;
  if (RK.ok && info == RK.record_info) return UK_RECORD;
  unison_jit_cmp_fail("closure kind", c);
  s->ok = 0;
  return -1;
}

// a constructor's (or a PAp's) tag and fields: the words and pointers of
// the fields in order, or in reverse when they come from segments
typedef struct {
  StgWord tag, n;
  const StgWord *us;
  StgClosure *const *bs;
  int rev;
  StgWord uw[DATA_MAX];
  StgClosure *bp[DATA_MAX];
} Fields;

// the fields of a segment: the ByteArray# and the Array# (the closure's own
// fields; never thunks)
static int uc_segs(UC *s, StgClosure *useg, StgClosure *bseg, Fields *f) {
  (void)s;
  StgArrBytes *us = (StgArrBytes *)useg;
  StgMutArrPtrs *bs = (StgMutArrPtrs *)bseg;
  f->n = bs->ptrs;
  f->us = (const StgWord *)us->payload;
  f->bs = (StgClosure *const *)bs->payload;
  f->rev = 1;
  return 1;
}

// three to six fields, out of line (see mh_data_wide)
__attribute__((noinline)) static int uc_fields_wide(StgClosure *c, int arity, Fields *f) {
  f->tag = DATA_TAG(c, arity);
  f->n = arity;
  for (int j = 0; j < arity; j++) {
    f->uw[j] = DATA_U(c, arity, j);
    f->bp[j] = DATA_B(c, arity, j);
  }
  return 1;
}

static int uc_fields(UC *s, StgClosure *c, Fields *f) {
  StgWord info = (StgWord)LUN(c)->header.info;
  f->us = f->uw;
  f->bs = f->bp;
  f->rev = 0;
  int arity = data_arity(info);
  if (arity == 0) {
    f->tag = DATA_TAG(c, 0);
    f->n = 0;
    return 1;
  }
  if (arity == 1) {
    f->tag = DATA_TAG(c, 1);
    f->n = 1;
    f->uw[0] = DATA_U(c, 1, 0);
    f->bp[0] = DATA_B(c, 1, 0);
    return 1;
  }
  if (arity == 2) {
    f->tag = DATA_TAG(c, 2);
    f->n = 2;
    f->uw[0] = DATA_U(c, 2, 0);
    f->bp[0] = DATA_B(c, 2, 0);
    f->uw[1] = DATA_U(c, 2, 1);
    f->bp[1] = DATA_B(c, 2, 1);
    return 1;
  }
  if (arity <= DATA_MAX) return uc_fields_wide(c, arity, f);
  // DataG tag useg bseg ref
  f->tag = LW(c, 3);
  return uc_segs(s, LP(c, 0), LP(c, 1), f);
}

static inline StgWord fld_u(const Fields *f, StgWord i) { return f->us[f->rev ? f->n - 1 - i : i]; }
static inline StgClosure *fld_b(const Fields *f, StgWord i) { return f->bs[f->rev ? f->n - 1 - i : i]; }

static inline int64_t cmp_i(int64_t a, int64_t b) { return a < b ? -1 : a > b ? 1 : 0; }
static inline int64_t cmp_u(StgWord a, StgWord b) { return a < b ? -1 : a > b ? 1 : 0; }

// the kind of an unboxed value's type tag closure, or 0 for a boxed value
static inline int utag_kind(StgClosure *b) {
  if ((StgWord)LUN(b)->header.info != HK.utag_info) return 0;
  return LTAG(LP(b, 0));
}

static int64_t uc_unboxed(StgWord u1, int k1, StgWord u2, int k2, int tyeq) {
  if (k1 == 3 && k2 == 3) return cmp_i((int64_t)u1, (int64_t)u2);
  if (k1 == 4 && k2 == 4) return cmp_u(u1, u2);
  if (k1 == 4 && k2 == 3) return (int64_t)u2 < 0 ? 1 : cmp_u(u1, u2);
  if (k1 == 3 && k2 == 4) return (int64_t)u1 < 0 ? -1 : cmp_u(u1, u2);
  if (k1 == 2 && k2 == 2) {
    // compareAsFloat: positive floats order as their bits; two non-positive
    // ones by magnitude, backwards (-0.0 and 0.0 both clear to 0)
    int64_t i = (int64_t)u1, j = (int64_t)u2;
    if (i > 0 || j > 0) return cmp_i(i, j);
    return cmp_i(j & 0x7FFFFFFFFFFFFFFFLL, i & 0x7FFFFFFFFFFFFFFFLL);
  }
  if (tyeq && k1 != k2) return cmp_i(k1, k2);
  return cmp_i((int64_t)u1, (int64_t)u2);
}

static int64_t uc_closure(UC *s, StgClosure *c1, StgClosure *c2, int tyeq);

static int64_t uc_val(UC *s, StgWord u1, StgClosure *b1, StgWord u2, StgClosure *b2, int tyeq) {
  if (!s->ok) return 0;
  if (LTAG(b1) == 0 || LTAG(b2) == 0) {
    unison_jit_cmp_fail("untagged value", LTAG(b1) == 0 ? b1 : b2);
    s->ok = 0;
    return 0;
  }
  int k1 = utag_kind(b1), k2 = utag_kind(b2);
  if (k1 && k2) return uc_unboxed(u1, k1, u2, k2, tyeq);
  if (k1) return -1;
  if (k2) return 1;
  if (b1 == b2) return 0;
  return uc_closure(s, b1, b2, tyeq);
}

// a Val closure (a list or array element): pointer b, then word u
static int uc_elem(UC *s, StgClosure *item, StgWord *u, StgClosure **b) {
  if (LTAG(item) != 1 || (StgWord)LUN(item)->header.info != LF.val_info) {
    unison_jit_cmp_fail("list element", item);
    s->ok = 0;
    return 0;
  }
  *u = LW(item, 1);
  *b = LP(item, 0);
  return 1;
}

// element i of a record's values
static inline StgClosure *rec_val(StgClosure *c, StgWord i) {
  StgClosure *v = ((StgMutArrPtrs *)LP(c, REC_VALS))->payload[LW(c, REC_VOFF) + i];
  return LTAG(v) == 0 ? unison_jit_settle(v) : v;
}

static int64_t uc_elems(UC *s, StgClosure *x1, StgClosure *x2, int tyeq) {
  StgWord u1, u2;
  StgClosure *b1, *b2;
  if (!uc_elem(s, x1, &u1, &b1) || !uc_elem(s, x2, &u2, &b2)) return 0;
  return uc_val(s, u1, b1, u2, b2, tyeq);
}

static int64_t uc_fieldwise(UC *s, const Fields *f1, const Fields *f2, int tyeq) {
  if (f1->n != f2->n) return cmp_u(f1->n, f2->n);
  for (StgWord i = 0; i < f1->n && s->ok; i++) {
    int64_t r = uc_val(s, fld_u(f1, i), fld_b(f1, i), fld_u(f2, i), fld_b(f2, i), tyeq);
    if (r != 0) return r;
  }
  return 0;
}

// the two deques element by element, then by length (liftCompare)
static int64_t uc_lists(UC *s, StgClosure *d1, StgClosure *d2, int tyeq) {
  StgInt n1 = LTAG(d1) == 2 ? TSIZE(LW(d1, 3)) : 0, n2 = LTAG(d2) == 2 ? TSIZE(LW(d2, 3)) : 0;
  StgInt n = n1 < n2 ? n1 : n2;
  for (StgInt i = 0; i < n && s->ok; i++) {
    int64_t r = uc_elems(s, unison_jit_list_at(d1, i), unison_jit_list_at(d2, i), tyeq);
    if (r != 0) return r;
  }
  return cmp_i(n1, n2);
}

static int64_t uc_arrays(UC *s, StgMutArrPtrs *a1, StgMutArrPtrs *a2, int tyeq) {
  StgWord n = a1->ptrs < a2->ptrs ? a1->ptrs : a2->ptrs;
  for (StgWord i = 0; i < n && s->ok; i++) {
    int64_t r = uc_elems(s, a1->payload[i], a2->payload[i], tyeq);
    if (r != 0) return r;
  }
  return cmp_u(a1->ptrs, a2->ptrs);
}

// Data.Primitive's Ord ByteArray: the common prefix, then the lengths
static int64_t uc_barrays(StgArrBytes *a1, StgArrBytes *a2) {
  const unsigned char *p1 = (const unsigned char *)a1->payload, *p2 = (const unsigned char *)a2->payload;
  StgWord n = a1->bytes < a2->bytes ? a1->bytes : a2->bytes;
  for (StgWord i = 0; i < n; i++)
    if (p1[i] != p2[i]) return p1[i] < p2[i] ? -1 : 1;
  return cmp_u(a1->bytes, a2->bytes);
}

static int64_t uc_closure(UC *s, StgClosure *c1, StgClosure *c2, int tyeq) {
  int k1 = uc_kind(s, c1), k2 = s->ok ? uc_kind(s, c2) : -1;
  if (!s->ok) return 0;
  if (k1 != k2) return cmp_i(k1, k2);
  if (k1 == UK_DATA) {
    Fields f1, f2;
    if (!uc_fields(s, c1, &f1) || !uc_fields(s, c2, &f2)) return 0;
    if (tyeq && f1.tag != f2.tag) {
      unison_jit_cmp_fail("type references", c1);
      s->ok = 0;
      return 0;
    }
    int64_t r = cmp_u(f1.tag & 0xFFFF, f2.tag & 0xFFFF);
    if (r != 0) return r;
    return uc_fieldwise(s, &f1, &f2, tyeq || (f1.tag >> 16) == CK.any_type);
  }
  if (k1 == UK_RECORD) {
    if (tyeq && LW(c1, REC_REF) != LW(c2, REC_REF)) {
      unison_jit_cmp_fail("record shapes", c1);
      s->ok = 0;
      return 0;
    }
    // compareValList: the lengths, then the slots
    StgWord n1 = LW(c1, REC_VLEN), n2 = LW(c2, REC_VLEN);
    if (n1 != n2) return cmp_u(n1, n2);
    for (StgWord i = 0; i < n1 && s->ok; i++) {
      int64_t r = uc_elems(s, rec_val(c1, i), rec_val(c2, i), tyeq);
      if (r != 0) return r;
    }
    return 0;
  }
  if (k1 == UK_PAP) {
    // PAp (CIx ref grp w) comb useg bseg: pointers ref, entry, the arrays; words grp, w
    if (LP(c1, 0) != LP(c2, 0)) {
      unison_jit_cmp_fail("function references", c1);
      s->ok = 0;
      return 0;
    }
    int64_t r = cmp_u(LW(c1, 4), LW(c2, 4));
    if (r != 0) return r;
    r = cmp_u(LW(c1, 5), LW(c2, 5));
    if (r != 0) return r;
    Fields f1, f2;
    if (!uc_segs(s, LP(c1, 2), LP(c1, 3), &f1) || !uc_segs(s, LP(c2, 2), LP(c2, 3), &f2)) return 0;
    return uc_fieldwise(s, &f1, &f2, tyeq);
  }
  // two foreign values
  StgClosure *t1 = rope_of(&TK, c1), *t2 = rope_of(&TK, c2);
  if (t1 != NULL || t2 != NULL) {
    if (t1 != NULL && t2 != NULL && (s->kinds & 1)) return unison_jit_foreign_cmp(c1, c2, 1);
    unison_jit_cmp_fail("text against something else", t1 != NULL ? c2 : c1);
    s->ok = 0;
    return 0;
  }
  StgClosure *y1 = rope_of(&BK, c1), *y2 = rope_of(&BK, c2);
  if (y1 != NULL || y2 != NULL) {
    if (y1 != NULL && y2 != NULL && (s->kinds & 2)) return unison_jit_foreign_cmp(c1, c2, 2);
    unison_jit_cmp_fail("bytes against something else", y1 != NULL ? c2 : c1);
    s->ok = 0;
    return 0;
  }
  StgClosure *d1 = deque_of(c1), *d2 = deque_of(c2);
  if (d1 != NULL && d2 != NULL) return uc_lists(s, d1, d2, tyeq);
  if (AK.ok) {
    StgMutArrPtrs *a1 = wrapped_of(c1, &AK.array), *a2 = wrapped_of(c2, &AK.array);
    if (a1 != NULL && a2 != NULL) return uc_arrays(s, a1, a2, tyeq);
    StgArrBytes *b1 = wrapped_of(c1, &AK.barray), *b2 = wrapped_of(c2, &AK.barray);
    if (b1 != NULL && b2 != NULL) return uc_barrays(b1, b2);
  }
  unison_jit_cmp_fail("foreign", c1); // a map, a ref, two different kinds, ...
  s->ok = 0;
  return 0;
}

// --- equality ---

static int ue_closure(UC *s, StgClosure *c1, StgClosure *c2);

static int ue_val(UC *s, StgWord u1, StgClosure *b1, StgWord u2, StgClosure *b2) {
  if (!s->ok) return 0;
  if (LTAG(b1) == 0 || LTAG(b2) == 0) {
    unison_jit_cmp_fail("untagged value", LTAG(b1) == 0 ? b1 : b2);
    s->ok = 0;
    return 0;
  }
  int k1 = utag_kind(b1), k2 = utag_kind(b2);
  if (k1 && k2) return (k1 == k2 || (k1 == 3 && k2 == 4) || (k1 == 4 && k2 == 3)) && u1 == u2;
  if (k1 || k2) return 0;
  if (b1 == b2) return 1;
  return ue_closure(s, b1, b2);
}

static int ue_elems(UC *s, StgClosure *x1, StgClosure *x2) {
  StgWord u1, u2;
  StgClosure *b1, *b2;
  if (!uc_elem(s, x1, &u1, &b1) || !uc_elem(s, x2, &u2, &b2)) return 0;
  return ue_val(s, u1, b1, u2, b2);
}

static int ue_fieldwise(UC *s, const Fields *f1, const Fields *f2) {
  if (f1->n != f2->n) return 0;
  for (StgWord i = 0; i < f1->n && s->ok; i++)
    if (!ue_val(s, fld_u(f1, i), fld_b(f1, i), fld_u(f2, i), fld_b(f2, i))) return 0;
  return s->ok;
}

static int ue_closure(UC *s, StgClosure *c1, StgClosure *c2) {
  int k1 = uc_kind(s, c1), k2 = s->ok ? uc_kind(s, c2) : -1;
  if (!s->ok) return 0;
  if (k1 != k2) return 0;
  if (k1 == UK_DATA) {
    Fields f1, f2;
    if (!uc_fields(s, c1, &f1) || !uc_fields(s, c2, &f2)) return 0;
    if (f1.n == 1 && f2.n == 1) {
      // matchTags: boxed Int and Nat literals match each other
      if (!(f1.tag == f2.tag || (f1.tag == CK.int_tag && f2.tag == CK.nat_tag) || (f1.tag == CK.nat_tag && f2.tag == CK.int_tag)))
        return 0;
    } else if (f1.tag != f2.tag)
      return 0;
    return ue_fieldwise(s, &f1, &f2);
  }
  if (k1 == UK_RECORD) {
    if (LW(c1, REC_REF) != LW(c2, REC_REF) || LW(c1, REC_VLEN) != LW(c2, REC_VLEN)) return 0;
    for (StgWord i = 0; i < LW(c1, REC_VLEN) && s->ok; i++)
      if (!ue_elems(s, rec_val(c1, i), rec_val(c2, i))) return 0;
    return s->ok;
  }
  if (k1 == UK_PAP) {
    if (LP(c1, 0) != LP(c2, 0)) {
      unison_jit_cmp_fail("function references", c1);
      s->ok = 0;
      return 0;
    }
    if (LW(c1, 4) != LW(c2, 4) || LW(c1, 5) != LW(c2, 5)) return 0;
    Fields f1, f2;
    if (!uc_segs(s, LP(c1, 2), LP(c1, 3), &f1) || !uc_segs(s, LP(c2, 2), LP(c2, 3), &f2)) return 0;
    return ue_fieldwise(s, &f1, &f2);
  }
  StgClosure *t1 = rope_of(&TK, c1), *t2 = rope_of(&TK, c2);
  if (t1 != NULL || t2 != NULL) {
    if (t1 != NULL && t2 != NULL && (s->kinds & 1)) return unison_jit_foreign_eq(c1, c2, 1) == 1;
    unison_jit_cmp_fail("text against something else", t1 != NULL ? c2 : c1);
    s->ok = 0;
    return 0;
  }
  StgClosure *y1 = rope_of(&BK, c1), *y2 = rope_of(&BK, c2);
  if (y1 != NULL || y2 != NULL) {
    if (y1 != NULL && y2 != NULL && (s->kinds & 2)) return unison_jit_foreign_eq(c1, c2, 2) == 1;
    unison_jit_cmp_fail("bytes against something else", y1 != NULL ? c2 : c1);
    s->ok = 0;
    return 0;
  }
  StgClosure *d1 = deque_of(c1), *d2 = deque_of(c2);
  if (d1 != NULL && d2 != NULL) {
    StgInt n1 = LTAG(d1) == 2 ? TSIZE(LW(d1, 3)) : 0, n2 = LTAG(d2) == 2 ? TSIZE(LW(d2, 3)) : 0;
    if (n1 != n2) return 0;
    for (StgInt i = 0; i < n1 && s->ok; i++)
      if (!ue_elems(s, unison_jit_list_at(d1, i), unison_jit_list_at(d2, i))) return 0;
    return s->ok;
  }
  if (AK.ok) {
    StgMutArrPtrs *a1 = wrapped_of(c1, &AK.array), *a2 = wrapped_of(c2, &AK.array);
    if (a1 != NULL && a2 != NULL) {
      if (a1->ptrs != a2->ptrs) return 0;
      for (StgWord i = 0; i < a1->ptrs && s->ok; i++)
        if (!ue_elems(s, a1->payload[i], a2->payload[i])) return 0;
      return s->ok;
    }
    StgArrBytes *b1 = wrapped_of(c1, &AK.barray), *b2 = wrapped_of(c2, &AK.barray);
    if (b1 != NULL && b2 != NULL) {
      if (b1->bytes != b2->bytes) return 0;
      const unsigned char *p1 = (const unsigned char *)b1->payload, *p2 = (const unsigned char *)b2->payload;
      for (StgWord i = 0; i < b1->bytes; i++)
        if (p1[i] != p2[i]) return 0;
      return 1;
    }
    // the same object: refs and mutable arrays
    const WrapInfo *same[3] = {&AK.ioref, &AK.marray, &AK.mbarray};
    for (int i = 0; i < 3; i++) {
      StgWord w1 = wrapped_word(c1, same[i]), w2 = wrapped_word(c2, same[i]);
      if (w1 != 0 && w2 != 0) return w1 == w2;
    }
  }
  unison_jit_cmp_fail("foreign", c1);
  s->ok = 0;
  return 0;
}

JitNat unison_jit_ucmp(int64_t u1, void *b1, int64_t u2, void *b2, int64_t kinds) {
  if (!CK.ok) return (JitNat){0, 0};
  UC s = {1, kinds};
  int64_t r = uc_val(&s, (StgWord)u1, (StgClosure *)b1, (StgWord)u2, (StgClosure *)b2, 0);
  return s.ok ? (JitNat){1, r} : (JitNat){0, 0};
}

JitNat unison_jit_ueq(int64_t u1, void *b1, int64_t u2, void *b2, int64_t kinds) {
  if (!CK.ok) return (JitNat){0, 0};
  UC s = {1, kinds};
  int r = ue_val(&s, (StgWord)u1, (StgClosure *)b1, (StgWord)u2, (StgClosure *)b2);
  return s.ok ? (JitNat){1, r} : (JitNat){0, 0};
}
