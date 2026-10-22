// Part of the JIT's C runtime, included by jit_rt.c (the files are one translation unit): see
// jit_rt.c for the whole, jit_builtins.h for what the parts share, and
// unison-runtime/src/Unison/Runtime/JIT/design.md for the design.
#include "jit_builtins.h"

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








// --- building ---



#define DEEP_WORDS 5
static inline StgClosure *deep_at(StgWord *p, StgInt t, StgClosure *pr, StgClosure *m, StgClosure *sf) {
  p[0] = LF.deep_info;
  p[1] = (StgWord)pr;
  p[2] = (StgWord)m;
  p[3] = (StgWord)sf;
  p[4] = (StgWord)t;
  return (StgClosure *)((StgWord)p | 2);
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

