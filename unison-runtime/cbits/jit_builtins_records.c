// Part of the JIT's C runtime, included by jit_rt.c (the files are one translation unit): see
// jit_rt.c for the whole, jit_builtins.h for what the parts share, and
// unison-runtime/src/Unison/Runtime/JIT/design.md for the design.
#include "jit_builtins.h"

// ---------------------------------------------------------------------------
// Records ("records" in jit_internal.h): the facts table's startup function,
// the trace output and the update helper. The position lookup is in
// jit_helpers.c.

void unison_jit_rec_fail(const char *what, StgClosure *c) {
  static int trace_test = -1;
  if (trace_test < 0) trace_test = debug_getenv("UNISON_JIT_TRACE_TEST") != NULL;
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
