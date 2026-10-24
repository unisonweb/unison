// Part of the JIT's C runtime, included by jit_rt.c (the files are one translation unit): see
// jit_rt.c for the whole, jit_builtins.h for what the parts share, and
// unison-runtime/src/Unison/Runtime/JIT/design.md for the design.
#include "jit_builtins.h"

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
  if (p == NULL) return NULL;
  StgInt out_len = base == 32 ? (len + 4) / 5 * 8 : base == 64 ? (len + 2) / 3 * 4 : (len * 4 + 2) / 3;
  unsigned char *out = malloc(out_len > 0 ? out_len : 1), *o = out;
  if (out == NULL) return free(heap), NULL;
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
  if (p == NULL) return NULL;
  unsigned char *out = malloc(len > 0 ? len : 1), *o = out;
  if (out == NULL) return free(heap), NULL;
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

