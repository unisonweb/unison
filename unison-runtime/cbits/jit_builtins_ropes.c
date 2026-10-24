// Part of the JIT's C runtime, included by jit_rt.c (the files are one translation unit): see
// jit_rt.c for the whole, jit_builtins.h for what the parts share, and
// unison-runtime/src/Unison/Runtime/JIT/design.md for the design.
#include "jit_builtins.h"

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
  if (trace_test < 0) trace_test = debug_getenv("UNISON_JIT_TRACE_TEST") != NULL;
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

