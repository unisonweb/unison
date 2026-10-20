// Part of the JIT's C runtime, included by jit_rt.c (the files are one translation unit): see
// jit_rt.c for the whole, jit_builtins.h for what the parts share, and
// unison-runtime/src/Unison/Runtime/JIT/design.md for the design.
#include "jit_builtins.h"

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

