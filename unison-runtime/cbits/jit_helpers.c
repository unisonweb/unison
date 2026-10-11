// Helpers that generated code calls as LLVM IR rather than as C.
//
// This file is compiled twice: into the library as part of jit_rt.c, which
// includes it at its end, and by clang on its own into cbits/jit_helpers.ll
// (cbits/gen-helpers.sh), which the JIT adds as a module of its own at
// startup. Generated code resolves `unison_jit_murmur`
// to the IR copy, which the JIT compiles for the same target as the caller,
// so the two agree on how the `{ i64, i64 }` result comes back on every
// platform; the C copy is what the startup self-check runs and the fallback
// under UNISON_JIT_DISABLE=irhelpers. Whatever is here must keep the IR
// platform-independent: see jit_internal.h and "The IR helpers" in
// internals.md. The tables and walkers it needs are exported from jit_rt.c.
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

// the code points of a text, chunk by chunk
static void mh_text(MH *s, StgClosure *r) {
  const RopeKind *K = &TK;
  StgInt n = rope_size(K, r), off;
  for (StgInt i = 0; i < n;) {
    StgClosure *c = unison_jit_rope_chunk_at(K, r, i, &off);
    const unsigned char *p = chunk_bytes(K, c), *e = p + chunk_len(K, c);
    while (p < e) {
      uint64_t cp;
      if (p[0] < 0x80) cp = p[0], p += 1;
      else if (p[0] < 0xE0) cp = ((p[0] & 0x1Fu) << 6) | (p[1] & 0x3Fu), p += 2;
      else if (p[0] < 0xF0) cp = ((p[0] & 0x0Fu) << 12) | ((p[1] & 0x3Fu) << 6) | (p[2] & 0x3Fu), p += 3;
      else cp = ((p[0] & 0x07u) << 18) | ((p[1] & 0x3Fu) << 12) | ((p[2] & 0x3Fu) << 6) | (p[3] & 0x3Fu), p += 4;
      mh_word(s, cp);
    }
    i += chunk_size(K, c);
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

static void mh_closure(MH *s, StgClosure *c) {
  if (!s->ok) return;
  int tag = LTAG(c);
  if (tag == 0) {
    unison_jit_murmur_fail("untagged closure", c);
    s->ok = 0;
    return;
  }
  StgWord info = (StgWord)LUN(c)->header.info;
  if (info == HK.enum_info) {
    mh_word(s, 2);
    mh_word(s, LW(c, 1) & 0xFFFF);
  } else if (info == LF.data1_info) {
    // Data1 ref tag (Val u b): pointers ref, b; words tag, u
    mh_word(s, 2);
    mh_word(s, LW(c, 2) & 0xFFFF);
    mh_val(s, LW(c, 3), LP(c, 1));
  } else if (info == LF.data2_info) {
    mh_word(s, 2);
    mh_word(s, LW(c, 3) & 0xFFFF);
    mh_val(s, LW(c, 4), LP(c, 1));
    mh_val(s, LW(c, 5), LP(c, 2));
  } else if (info == HK.datag_info) {
    // DataG ref tag (useg, bseg): the segments hold the fields in reverse
    // the segments' boxes may be static indirections (a constant array GHC
    // floated to a CAF): follow them; an unevaluated one is the interpreter's
    StgClosure *ubox = unison_jit_settle(LP(c, 1)), *bbox = unison_jit_settle(LP(c, 2));
    if (LTAG(ubox) == 0 || LTAG(bbox) == 0) {
      unison_jit_murmur_fail("segment box", LTAG(ubox) == 0 ? ubox : bbox);
      s->ok = 0;
      return;
    }
    mh_word(s, 2);
    mh_word(s, LW(c, 3) & 0xFFFF);
    StgArrBytes *us = (StgArrBytes *)LP(ubox, 0);
    StgMutArrPtrs *bs = (StgMutArrPtrs *)LP(bbox, 0);
    StgWord n = bs->ptrs;
    for (StgWord j = 0; j < n && s->ok; j++) {
      StgWord i = n - 1 - j;
      mh_val(s, ((StgWord *)us->payload)[i], bs->payload[i]);
    }
  } else if (info == LF.foreign_info) {
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
// environment ("The handler environment" in internals.md).

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
