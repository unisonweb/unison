// Part of the JIT's C runtime, included by jit_rt.c (the files are one translation unit): see
// jit_rt.c for the whole, jit_builtins.h for what the parts share, and
// unison-runtime/src/Unison/Runtime/JIT/design.md for the design.
#include "jit_builtins.h"

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

