// Part of the JIT's C runtime, included by jit_rt.c (the files are one translation unit): see
// jit_rt.c for the whole, jit_builtins.h for what the parts share, and
// unison-runtime/src/Unison/Runtime/JIT/design.md for the design.
#include "jit_builtins.h"

// ---------------------------------------------------------------------------
// The rest of Text and Bytes
//
// Conversions, searches, orderings and the foreign functions, as ports of
// the interpreter's primitives (Machine/Primops.hs) and foreign functions
// (Foreign/Function.hs) over the Haskell operations in Unison.Util.Text and
// Unison.Util.Bytes. Each helper takes every case it can decide exactly and
// answers "not handled" (NULL) for the rest, which generated code leaves to
// the interpreter: a text that isn't one, an element of the wrong type, a
// number written in a form the Haskell lexer might read differently, an
// encoding that isn't canonical, invalid UTF-8 (the interpreter builds the
// Failure). The results are built as the interpreter builds them, so that a
// program can't tell which side made a value:
//
//   Some v        Data1 ref tag v        (ref from the pooled None)
//   (a, b)        Data2 ref tag a (Data2 ref tag b ())   (ref from the pooled Tuple)
//   Right v       Data1 ref tag v        (ref from the pooled Either)
//   a Val         a pointer, then a word; a boxed value's word is -1
//   a new text    chunks of at most the threshold's characters over one
//                 array, as Text.fromText cuts them

// --- results ---






// --- UTF-8 ---

static inline StgInt utf8_len(unsigned char b) { return b < 0x80 ? 1 : b < 0xE0 ? 2 : b < 0xF0 ? 3 : 4; }
static inline StgInt utf8_cp_len(StgInt c) { return c < 0x80 ? 1 : c < 0x800 ? 2 : c < 0x10000 ? 3 : 4; }

// the code point at p, and its byte length (p is valid UTF-8)
static inline StgInt utf8_decode(const unsigned char *p, StgInt *n) {
  unsigned char b = p[0];
  if (b < 0x80) return *n = 1, b;
  if (b < 0xE0) return *n = 2, ((b & 0x1F) << 6) | (p[1] & 0x3F);
  if (b < 0xF0) return *n = 3, ((b & 0x0F) << 12) | ((p[1] & 0x3F) << 6) | (p[2] & 0x3F);
  return *n = 4, ((b & 0x07) << 18) | ((p[1] & 0x3F) << 12) | ((p[2] & 0x3F) << 6) | (p[3] & 0x3F);
}

// the start of the last character of p[0..len)
static inline StgInt utf8_last_start(const unsigned char *p, StgInt len) {
  StgInt i = len - 1;
  while (i > 0 && (p[i] & 0xC0) == 0x80) i--;
  return i;
}

static inline StgInt utf8_encode(StgInt c, unsigned char *o) {
  if (c < 0x80) return o[0] = c, 1;
  if (c < 0x800) return o[0] = 0xC0 | (c >> 6), o[1] = 0x80 | (c & 0x3F), 2;
  if (c < 0x10000) return o[0] = 0xE0 | (c >> 12), o[1] = 0x80 | ((c >> 6) & 0x3F), o[2] = 0x80 | (c & 0x3F), 3;
  return o[0] = 0xF0 | (c >> 18), o[1] = 0x80 | ((c >> 12) & 0x3F), o[2] = 0x80 | ((c >> 6) & 0x3F),
         o[3] = 0x80 | (c & 0x3F), 4;
}

// The number of characters in p[0..len) if it is valid UTF-8 as
// Data.Text.decodeUtf8' sees it (no overlong forms, no surrogates, nothing
// past U+10FFFF, nothing cut short), else -1.
static StgInt utf8_count_valid(const unsigned char *p, StgInt len) {
  StgInt i = 0, count = 0;
  while (i < len) {
    unsigned char b = p[i];
    StgInt n, c;
    if (b < 0x80) n = 1, c = b;
    else if (b < 0xC2) return -1;
    else if (b < 0xE0) n = 2, c = b & 0x1F;
    else if (b < 0xF0) n = 3, c = b & 0x0F;
    else if (b < 0xF5) n = 4, c = b & 0x07;
    else return -1;
    if (i + n > len) return -1;
    for (StgInt j = 1; j < n; j++) {
      if ((p[i + j] & 0xC0) != 0x80) return -1;
      c = (c << 6) | (p[i + j] & 0x3F);
    }
    if ((n == 3 && c < 0x800) || (n == 4 && (c < 0x10000 || c > 0x10FFFF)) || (c >= 0xD800 && c <= 0xDFFF)) return -1;
    i += n, count++;
  }
  return count;
}

// --- building texts and bytes ---

// A text over the bytes [off, off + len) of arr, which are valid UTF-8, in
// chunks of at most the threshold's characters: what Text.fromText makes
// (T.chunksOf threshold, then snoc after snoc).
static StgClosure *text_over(UnisonJitCtx *ctx, StgArrBytes *arr, StgInt off, StgInt len) {
  const RopeKind *K = &TK;
  StgClosure *r = K->empty;
  const unsigned char *p = (const unsigned char *)arr->payload;
  StgInt i = off, end = off + len;
  while (i < end) {
    StgInt start = i, count = 0;
    while (i < end && count < K->threshold) i += utf8_len(p[i]), count++;
    r = rope_snoc(ctx, K, r, count, new_chunk(ctx, K, arr, count, start, i - start));
  }
  return r;
}

// a text of the UTF-8 bytes at p, copied
static void *text_copy(UnisonJitCtx *ctx, const unsigned char *p, StgInt len) {
  if (len == 0) return wrap_rope(ctx, &TK, TK.empty, NULL);
  StgArrBytes *arr = new_arr(ctx, len);
  memcpy(arr->payload, p, len);
  return wrap_rope(ctx, &TK, text_over(ctx, arr, 0, len), NULL);
}

// a bytes of one chunk holding the n bytes at p (Bytes.fromWord8s)
static void *bytes_copy(UnisonJitCtx *ctx, const unsigned char *p, StgInt n) {
  if (n == 0) return wrap_rope(ctx, &BK, BK.empty, NULL);
  StgArrBytes *arr = new_arr(ctx, n);
  memcpy(arr->payload, p, n);
  return wrap_rope(ctx, &BK, rope_one(ctx, &BK, new_chunk(ctx, &BK, arr, n, 0, n)), NULL);
}

// The bytes of a rope in one piece: a chunk's own bytes when it has one
// chunk, else a copy in malloc'd memory, which the caller frees (*heap).
static const unsigned char *rope_flat(const RopeKind *K, StgClosure *r, StgInt *len, unsigned char **heap) {
  *heap = NULL;
  if (LTAG(r) == 1) return *len = 0, (const unsigned char *)"";
  if (LTAG(r) == 2) {
    StgClosure *c = LP(r, 0);
    *len = chunk_len(K, c);
    return chunk_bytes(K, c);
  }
  StgInt n = rope_size(K, r), total = 0, off;
  for (StgInt i = 0; i < n;) {
    StgClosure *c = unison_jit_rope_chunk_at(K, r, i, &off);
    total += chunk_len(K, c), i += chunk_size(K, c);
  }
  unsigned char *buf = malloc(total > 0 ? total : 1);
  if (buf == NULL) return *len = 0, NULL; // the caller returns "not handled"
  StgInt at = 0;
  for (StgInt i = 0; i < n;) {
    StgClosure *c = unison_jit_rope_chunk_at(K, r, i, &off);
    memcpy(buf + at, chunk_bytes(K, c), chunk_len(K, c));
    at += chunk_len(K, c), i += chunk_size(K, c);
  }
  *len = total;
  return *heap = buf;
}

// --- walking a list ---

static void node_each(StgClosure *nd, int depth, ItemFn fn, void *env) {
  StgClosure **k = node_kids(nd);
  StgInt n = node_arity(nd);
  for (StgInt i = 0; i < n; i++)
    if (depth == 1) fn(k[i], env);
    else node_each(k[i], depth - 1, fn, env);
}

static inline void item_each(StgClosure *x, int depth, ItemFn fn, void *env) {
  if (depth == 0) fn(x, env);
  else node_each(x, depth, fn, env);
}

// the elements of a deque level in order: `depth` is how many levels of
// nodes its items hold (0 at the top)
void unison_jit_lv_each(StgClosure *c, int top, int depth, ItemFn fn, void *env) {
  Lv v;
  if (!lv_read(c, top, &v)) return;
  for (StgClosure *l = v.pr; IS_CONS(l); l = TAIL(l)) item_each(HEAD(l), depth, fn, env);
  unison_jit_lv_each(v.m, 0, depth + 1, fn, env);
  StgClosure *k[MAXD];
  StgInt n = s_items(v.sf, k);
  for (StgInt i = n - 1; i >= 0; i--) item_each(k[i], depth, fn, env);
}

// --- Text.uncons, Text.unsnoc ---

// Some (c, rest) (front) or Some (rest, c), None for the empty text, NULL if
// not a text. `char_tag` is a Char's type tag closure.
void *unison_jit_text_uncons(UnisonJitCtx *ctx, void *text, void *none, int64_t some_tag, void *pair, int64_t pair_tag,
                             void *unit, void *char_tag, int64_t front) {
  const RopeKind *K = &TK;
  StgClosure *r = rope_of(K, text);
  if (r == NULL) return NULL;
  if (LTAG(r) == 1) return none;
  StgInt n = rope_size(K, r), off, clen, cp;
  StgClosure *c, *rest;
  if (front) {
    c = unison_jit_rope_chunk_at(K, r, 0, &off);
    cp = utf8_decode(chunk_bytes(K, c), &clen);
    rest = rope_drop(ctx, K, 1, r);
  } else {
    c = unison_jit_rope_chunk_at(K, r, n - 1, &off);
    const unsigned char *p = chunk_bytes(K, c);
    cp = utf8_decode(p + utf8_last_start(p, chunk_len(K, c)), &clen);
    rest = rope_take(ctx, K, n - 1, r);
  }
  void *wrest = wrap_rope(ctx, K, rest, NULL);
  void *p = front ? mk_pair(ctx, pair, pair_tag, unit, char_tag, cp, wrest, -1)
                  : mk_pair(ctx, pair, pair_tag, unit, wrest, -1, char_tag, cp);
  return mk_some(ctx, none, some_tag, p, -1);
}

// --- numbers to text ---

// Int.toText / Nat.toText: `show`
void *unison_jit_int_to_text(UnisonJitCtx *ctx, int64_t n, int64_t is_signed) {
  char buf[32];
  int len = is_signed ? snprintf(buf, sizeof buf, "%lld", (long long)n) : snprintf(buf, sizeof buf, "%llu", (unsigned long long)n);
  return text_copy(ctx, (const unsigned char *)buf, len);
}

// Haskell's `show` for a Double: the shortest digits that read back as the
// number (the nearest of them when several do), written as d.ddd for
// 0.1 <= |x| < 10^7 and as d.ddde<exp> otherwise, with at least one digit
// after the point. Returns the length.
static int hs_show_double(double x, char *out) {
  if (isnan(x)) return sprintf(out, "NaN");
  if (isinf(x)) return sprintf(out, x < 0 ? "-Infinity" : "Infinity");
  int neg = signbit(x) != 0;
  double ax = fabs(x);
  if (ax == 0) return sprintf(out, neg ? "-0.0" : "0.0");
  char digits[24] = "";
  int e10 = 0, nd = 0;
  for (int p = 1; p <= 17 && nd == 0; p++) {
    char buf[40];
    snprintf(buf, sizeof buf, "%.*e", p - 1, ax);
    // mantissa digits and exponent
    unsigned long long m = 0;
    int e = 0;
    const char *q = buf;
    for (; *q && *q != 'e'; q++)
      if (*q >= '0' && *q <= '9') m = m * 10 + (*q - '0');
    if (*q == 'e') e = atoi(q + 1);
    unsigned long long pow10 = 1;
    for (int i = 1; i < p; i++) pow10 *= 10;
    // m is the nearest p-digit decimal. If it doesn't read back as x, the
    // neighbour on x's side of m is the next nearest, then the other one:
    // the first of the three that reads back is what Haskell's floatToDigits
    // picks (the nearest of the shortest). Which side x lies on is read off
    // its exact expansion: if truncating it to p digits gives m, x >= m.
    char exact[64];
    snprintf(exact, sizeof exact, "%.30e", ax);
    unsigned long long trunc = 0;
    int seen = 0, ee = 0;
    for (const char *q = exact; *q && *q != 'e'; q++)
      if (*q >= '0' && *q <= '9' && seen < p) trunc = trunc * 10 + (*q - '0'), seen++;
    ee = atoi(strchr(exact, 'e') + 1);
    int side = (ee == e && trunc == m) ? 1 : -1;
    unsigned long long cands[3] = {m, m + side, m - side};
    for (int k = 0; k < 3 && nd == 0; k++) {
      unsigned long long c = cands[k];
      int ce = e;
      if (c >= pow10 * 10) c /= 10, ce++; // 999 + 1
      if (c < pow10 && p > 1) continue;    // a shorter string: an earlier p would have found it
      char str[48], ds[24];
      int l = snprintf(ds, sizeof ds, "%llu", c);
      snprintf(str, sizeof str, "%c.%se%d", ds[0], l > 1 ? ds + 1 : "0", ce);
      if (strtod(str, NULL) != ax) continue;
      nd = l, e10 = ce;
      memcpy(digits, ds, l + 1);
    }
  }
  if (nd == 0) nd = snprintf(digits, sizeof digits, "%.17g", ax); // not reached
  while (nd > 1 && digits[nd - 1] == '0') digits[--nd] = 0;
  int e = e10 + 1; // x = 0.d1d2... * 10^e
  char *o = out;
  if (neg) *o++ = '-';
  if (ax >= 0.1 && ax < 1e7) {
    if (e <= 0) {
      o += sprintf(o, "0.");
      for (int i = 0; i < -e; i++) *o++ = '0';
      o += sprintf(o, "%s", digits);
    } else {
      for (int i = 0; i < e; i++) *o++ = i < nd ? digits[i] : '0';
      *o++ = '.';
      if (nd > e) o += sprintf(o, "%s", digits + e);
      else *o++ = '0';
    }
  } else {
    *o++ = digits[0];
    *o++ = '.';
    if (nd > 1) o += sprintf(o, "%s", digits + 1);
    else *o++ = '0';
    o += sprintf(o, "e%d", e - 1);
  }
  *o = 0;
  return (int)(o - out);
}

void *unison_jit_float_to_text(UnisonJitCtx *ctx, int64_t bits) {
  double x;
  memcpy(&x, &bits, 8);
  char buf[64];
  int len = hs_show_double(x, buf);
  return text_copy(ctx, (const unsigned char *)buf, len);
}

// --- text to numbers ---

// Text.toInt (kind 0), Text.toNat (1), Text.toFloat (2): Some n or None, as
// the interpreter's readMaybe decides. Only the plain forms are decided here:
// an optional sign and digits (a point and an exponent for floats), with no
// other character; a text with anything else, or of more than 64 characters,
// is left to the interpreter (NULL), whose lexer also takes spaces, hex,
// parentheses and more. `tag` is the result's type tag closure.
void *unison_jit_text_to_num(UnisonJitCtx *ctx, void *text, void *none, int64_t some_tag, void *tag, int64_t kind) {
  const RopeKind *K = &TK;
  StgClosure *r = rope_of(K, text);
  if (r == NULL) return NULL;
  StgInt n = rope_size(K, r);
  if (n == 0) return none;
  if (n > 64) return NULL;
  unsigned char buf[65 * 4];
  StgInt len = 0, off;
  for (StgInt i = 0; i < n;) {
    StgClosure *c = unison_jit_rope_chunk_at(K, r, i, &off);
    memcpy(buf + len, chunk_bytes(K, c), chunk_len(K, c));
    len += chunk_len(K, c), i += chunk_size(K, c);
  }
  buf[len] = 0;
  StgInt i = 0;
  int neg = 0, plus = 0;
  if (buf[i] == '-') neg = 1, i++;
  else if (buf[i] == '+') plus = 1, i++;
  StgInt digits_at = i;
  while (i < len && buf[i] >= '0' && buf[i] <= '9') i++;
  StgInt ndig = i - digits_at;
  if (kind == 2) {
    // [-]digits[.digits][(e|E)[+|-]digits]: what both C and the Haskell lexer read
    if (plus) return none; // Read Double has no unary plus
    if (ndig == 0) return NULL; // "NaN", "Infinity", or nothing: the interpreter decides
    if (i < len && buf[i] == '.') {
      StgInt j = ++i;
      while (i < len && buf[i] >= '0' && buf[i] <= '9') i++;
      if (i == j) return none; // "1." reads as nothing
    }
    if (i < len && (buf[i] == 'e' || buf[i] == 'E')) {
      StgInt j = ++i;
      if (i < len && (buf[i] == '+' || buf[i] == '-')) i++, j++;
      while (i < len && buf[i] >= '0' && buf[i] <= '9') i++;
      if (i == j) return NULL;
    }
    if (i != len) return NULL;
    double d = strtod((const char *)buf, NULL);
    int64_t bits;
    memcpy(&bits, &d, 8);
    return mk_some(ctx, none, some_tag, tag, bits);
  }
  if (i != len) return NULL;    // something other than digits: the interpreter decides
  if (ndig == 0) return none;   // a sign alone
  if (plus && kind == 1) return none; // Text.toNat has no unary plus
  unsigned long long mag = 0;
  for (StgInt j = digits_at; j < len; j++) {
    unsigned d = buf[j] - '0';
    if (mag > (ULLONG_MAX - d) / 10) return none; // past every range
    mag = mag * 10 + d;
  }
  if (kind == 1) {
    if (neg && mag != 0) return none;
    return mk_some(ctx, none, some_tag, tag, (int64_t)mag);
  }
  if (neg ? mag > (unsigned long long)1 << 63 : mag > (unsigned long long)INT64_MAX) return none;
  return mk_some(ctx, none, some_tag, tag, neg ? (int64_t)(0 - mag) : (int64_t)mag);
}

// --- Text.fromCharList, Text.toCharList ---

typedef struct {
  void *tag;     // the elements' required type tag, or NULL
  StgInt count;  // elements seen
  StgInt bytes;  // their UTF-8 bytes (chars)
  int bad;       // an element of another type, or out of range
  unsigned char *out; // where to write (second pass)
  StgInt limit;  // the largest allowed value (bytes)
} PackEnv;

static void pack_measure(StgClosure *x, void *env) {
  PackEnv *e = env;
  StgInt u = LW(x, 1);
  // a surrogate code point is not a character: Data.Text.pack replaces it,
  // so such a list is left to the interpreter, which does the same
  if (LP(x, 0) != e->tag || u < 0 || u > e->limit || (e->limit != 255 && u >= 0xD800 && u <= 0xDFFF)) e->bad = 1;
  else e->count++, e->bytes += e->limit == 255 ? 1 : utf8_cp_len(u);
}

static void pack_write(StgClosure *x, void *env) {
  PackEnv *e = env;
  StgInt u = LW(x, 1);
  if (e->limit == 255) e->out[e->count++] = (unsigned char)u;
  else e->bytes += utf8_encode(u, e->out + e->bytes);
}

// Text.fromCharList: a text of the list's characters, as Text.pack makes it;
// NULL if not a list or an element isn't a Char.
void *unison_jit_text_pack(UnisonJitCtx *ctx, void *list, void *char_tag) {
  StgClosure *dq = deque_of(list);
  if (dq == NULL) return NULL;
  PackEnv e = {char_tag, 0, 0, 0, NULL, 0x10FFFF};
  unison_jit_lv_each(dq, 1, 0, pack_measure, &e);
  if (e.bad) return NULL;
  if (e.count == 0) return wrap_rope(ctx, &TK, TK.empty, NULL);
  StgArrBytes *arr = new_arr(ctx, e.bytes);
  e.out = (unsigned char *)arr->payload, e.bytes = 0;
  unison_jit_lv_each(dq, 1, 0, pack_write, &e);
  return wrap_rope(ctx, &TK, text_over(ctx, arr, 0, e.bytes), NULL);
}

// Bytes.fromList: one chunk of the list's Nats; NULL if an element isn't a
// Nat below 256 (the interpreter raises the error)
void *unison_jit_bytes_pack(UnisonJitCtx *ctx, void *list, void *nat_tag) {
  StgClosure *dq = deque_of(list);
  if (dq == NULL) return NULL;
  PackEnv e = {nat_tag, 0, 0, 0, NULL, 255};
  unison_jit_lv_each(dq, 1, 0, pack_measure, &e);
  if (e.bad) return NULL;
  if (e.count == 0) return wrap_rope(ctx, &BK, BK.empty, NULL);
  StgArrBytes *arr = new_arr(ctx, e.count);
  e.out = (unsigned char *)arr->payload, e.count = 0;
  unison_jit_lv_each(dq, 1, 0, pack_write, &e);
  StgInt n = e.count;
  return wrap_rope(ctx, &BK, rope_one(ctx, &BK, new_chunk(ctx, &BK, arr, n, 0, n)), NULL);
}

// Text.toCharList (chars) or Bytes.toList (bytes): a list of the elements
// as unboxed values with the given type tag
static void *rope_unpack(UnisonJitCtx *ctx, const RopeKind *K, void *x, void *tag) {
  StgClosure *r = rope_of(K, x);
  if (r == NULL) return NULL;
  StgClosure *acc = LF.nil;
  StgInt n = rope_size(K, r), off;
  for (StgInt i = 0; i < n;) {
    StgClosure *c = unison_jit_rope_chunk_at(K, r, i, &off);
    const unsigned char *p = chunk_bytes(K, c), *end = p + chunk_len(K, c);
    while (p < end) {
      StgInt v, l = 1;
      if (K->utf8) v = utf8_decode(p, &l);
      else v = *p;
      acc = lv_snoc(ctx, 1, acc, mk_val(ctx, tag, v));
      p += l;
    }
    i += chunk_size(K, c);
  }
  return wrap_list(ctx, acc);
}

void *unison_jit_text_unpack(UnisonJitCtx *ctx, void *text, void *char_tag) { return rope_unpack(ctx, &TK, text, char_tag); }
void *unison_jit_bytes_unpack(UnisonJitCtx *ctx, void *bytes, void *nat_tag) { return rope_unpack(ctx, &BK, bytes, nat_tag); }

// --- indexOf ---

// Text.indexOf (the position in characters) or Bytes.indexOf: Some i or
// None. An empty text needle finds position 0; an empty bytes needle is left
// to the interpreter.
static void *rope_index_of(UnisonJitCtx *ctx, const RopeKind *K, void *needle, void *hay, void *none, StgInt some_tag,
                           void *nat_tag) {
  StgClosure *a = rope_of(K, needle), *b = rope_of(K, hay);
  if (a == NULL || b == NULL) return NULL;
  if (rope_size(K, a) == 0) return K->utf8 ? mk_some(ctx, none, some_tag, nat_tag, 0) : NULL;
  StgInt la, lb;
  unsigned char *ha, *hb;
  const unsigned char *pa = rope_flat(K, a, &la, &ha), *pb = rope_flat(K, b, &lb, &hb);
  if (pa == NULL || pb == NULL) return free(ha), free(hb), NULL;
  const unsigned char *at = la <= lb ? mem_find(pb, lb, pa, la) : NULL;
  void *res = none;
  if (at != NULL) {
    StgInt ix = 0;
    if (K->utf8) {
      for (const unsigned char *q = pb; q < at; q++) ix += (*q & 0xC0) != 0x80;
    } else ix = at - pb;
    res = mk_some(ctx, none, some_tag, nat_tag, ix);
  }
  free(ha), free(hb);
  return res;
}

void *unison_jit_text_index_of(UnisonJitCtx *ctx, void *needle, void *hay, void *none, int64_t some_tag, void *nat_tag) {
  return rope_index_of(ctx, &TK, needle, hay, none, some_tag, nat_tag);
}
void *unison_jit_bytes_index_of(UnisonJitCtx *ctx, void *needle, void *hay, void *none, int64_t some_tag, void *nat_tag) {
  return rope_index_of(ctx, &BK, needle, hay, none, some_tag, nat_tag);
}

// --- ordering ---

// compare: -1, 0 or 1 (by code points, which is the byte order of UTF-8,
// and by bytes for Bytes; a prefix comes first), or 2 if a closure isn't a
// rope of the kind
static int64_t rope_cmp(const RopeKind *K, void *x, void *y) {
  StgClosure *a = rope_of(K, x), *b = rope_of(K, y);
  if (a == NULL || b == NULL) return 2;
  if (a == b) return 0;
  StgInt n = rope_size(K, a), m = rope_size(K, b);
  const unsigned char *pa = NULL, *pb = NULL;
  StgInt na = 0, nb = 0, ia = 0, ib = 0, off;
  for (;;) {
    if (na == 0 && ia < n) {
      StgClosure *c = unison_jit_rope_chunk_at(K, a, ia, &off);
      pa = chunk_bytes(K, c), na = chunk_len(K, c), ia += chunk_size(K, c);
    }
    if (nb == 0 && ib < m) {
      StgClosure *c = unison_jit_rope_chunk_at(K, b, ib, &off);
      pb = chunk_bytes(K, c), nb = chunk_len(K, c), ib += chunk_size(K, c);
    }
    if (na == 0 || nb == 0) return na == nb ? 0 : na == 0 ? -1 : 1;
    StgInt k = na < nb ? na : nb;
    int d = memcmp(pa, pb, k);
    if (d != 0) return d < 0 ? -1 : 1;
    pa += k, pb += k, na -= k, nb -= k;
  }
}

int64_t unison_jit_text_cmp(void *x, void *y) { return rope_cmp(&TK, x, y); }

// Universal comparison on two texts or two bytes (kinds as in
// unison_jit_foreign_eq): -1, 0, 1, or 2 for anything else
int64_t unison_jit_foreign_cmp(void *x, void *y, int64_t kinds) {
  if ((kinds & 1) && rope_of(&TK, x) != NULL) return rope_cmp(&TK, x, y);
  if ((kinds & 2) && rope_of(&BK, x) != NULL) return rope_cmp(&BK, x, y);
  return 2;
}

// --- the Text foreign functions ---

// Char.toText
void *unison_jit_char_to_text(UnisonJitCtx *ctx, int64_t c) {
  unsigned char buf[4];
  return text_copy(ctx, buf, utf8_encode(c, buf));
}

// Text.repeat, as Util.Text.replicate: fewer than the threshold's characters
// in all is one chunk, else the two halves appended
static StgClosure *text_rep(UnisonJitCtx *ctx, StgInt n, StgClosure *t, StgInt size) {
  const RopeKind *K = &TK;
  if (size * n < K->threshold) {
    if (size * n == 0) return K->empty;
    StgInt len;
    unsigned char *heap;
    const unsigned char *p = rope_flat(K, t, &len, &heap);
    if (p == NULL) return NULL;
    StgArrBytes *arr = new_arr_mayfail(ctx, len * n);
    if (arr == NULL) return free(heap), NULL;
    for (StgInt i = 0; i < n; i++) memcpy((char *)arr->payload + i * len, p, len);
    free(heap);
    return rope_one(ctx, K, new_chunk(ctx, K, arr, size * n, 0, len * n));
  }
  if (n == 1) return t;
  StgClosure *lo = text_rep(ctx, n / 2, t, size);
  if (lo == NULL) return NULL;
  StgClosure *hi = text_rep(ctx, n - n / 2, t, size);
  if (hi == NULL) return NULL;
  return rope_append(ctx, K, lo, hi);
}

void *unison_jit_text_repeat(UnisonJitCtx *ctx, int64_t n, void *text) {
  StgClosure *r = rope_of(&TK, text);
  if (r == NULL) return NULL;
  StgInt size = rope_size(&TK, r);
  if (n < 0 || (size > 0 && n > ((StgInt)1 << 40) / size)) return NULL; // a size that can't be built
  if (size == 0 || n == 0) return wrap_rope(ctx, &TK, TK.empty, NULL);
  if (n == 1) return text;
  StgClosure *rep = text_rep(ctx, n, r, size);
  if (rep == NULL) return NULL;
  return wrap_rope(ctx, &TK, rep, NULL);
}

// a chunk's characters in reverse order, in a fresh array
static StgClosure *chunk_reverse(UnisonJitCtx *ctx, StgClosure *c) {
  const RopeKind *K = &TK;
  StgInt len = chunk_len(K, c);
  const unsigned char *p = chunk_bytes(K, c);
  StgArrBytes *arr = new_arr(ctx, len);
  unsigned char *o = (unsigned char *)arr->payload + len;
  for (StgInt i = 0; i < len;) {
    StgInt l = utf8_len(p[i]);
    o -= l;
    memcpy(o, p + i, l);
    i += l;
  }
  return new_chunk(ctx, K, arr, chunk_size(K, c), 0, len);
}

// Text.reverse, as the rope's: each chunk reversed, consed in front of the
// chunks before it
void *unison_jit_text_reverse(UnisonJitCtx *ctx, void *text) {
  const RopeKind *K = &TK;
  StgClosure *r = rope_of(K, text);
  if (r == NULL) return NULL;
  if (LTAG(r) == 1) return text;
  if (LTAG(r) == 2) return wrap_rope(ctx, K, rope_one(ctx, K, chunk_reverse(ctx, LP(r, 0))), NULL);
  StgClosure *acc = K->empty;
  StgInt n = rope_size(K, r), off;
  for (StgInt i = 0; i < n;) {
    StgClosure *c = unison_jit_rope_chunk_at(K, r, i, &off);
    acc = rope_cons(ctx, K, chunk_size(K, c), chunk_reverse(ctx, c), acc);
    i += chunk_size(K, c);
  }
  return wrap_rope(ctx, K, acc, NULL);
}

// Text.toUppercase / toLowercase for a text of ASCII only (Data.Text's case
// mapping of anything else is Unicode's, and may change the length): the
// chunks mapped one by one and snoc'd, as the rope's map does. NULL if any
// character is beyond ASCII.
void *unison_jit_text_case(UnisonJitCtx *ctx, void *text, int64_t upper) {
  const RopeKind *K = &TK;
  StgClosure *r = rope_of(K, text);
  if (r == NULL) return NULL;
  StgInt n = rope_size(K, r), off;
  for (StgInt i = 0; i < n;) {
    StgClosure *c = unison_jit_rope_chunk_at(K, r, i, &off);
    if (chunk_len(K, c) != chunk_size(K, c)) return NULL;
    i += chunk_size(K, c);
  }
  StgClosure *acc = K->empty;
  for (StgInt i = 0; i < n;) {
    StgClosure *c = unison_jit_rope_chunk_at(K, r, i, &off);
    StgInt len = chunk_len(K, c);
    const unsigned char *p = chunk_bytes(K, c);
    StgArrBytes *arr = new_arr(ctx, len);
    unsigned char *o = (unsigned char *)arr->payload;
    // ASCII only (checked above), by arithmetic: toupper/tolower follow the
    // process locale, and a single-byte non-ASCII locale maps some letters to
    // bytes >= 0x80, which would not be UTF-8
    for (StgInt j = 0; j < len; j++) {
      unsigned char ch = p[j];
      if (upper) o[j] = (ch >= 'a' && ch <= 'z') ? ch - 32 : ch;
      else o[j] = (ch >= 'A' && ch <= 'Z') ? ch + 32 : ch;
    }
    acc = rope_snoc(ctx, K, acc, len, new_chunk(ctx, K, arr, len, 0, len));
    i += len;
  }
  return wrap_rope(ctx, K, acc, NULL);
}

// Text.toUtf8: the same arrays as chunks of bytes, snoc'd in order
void *unison_jit_text_to_utf8(UnisonJitCtx *ctx, void *text) {
  StgClosure *r = rope_of(&TK, text);
  if (r == NULL) return NULL;
  StgClosure *acc = BK.empty;
  StgInt n = rope_size(&TK, r), off;
  for (StgInt i = 0; i < n;) {
    StgClosure *c = unison_jit_rope_chunk_at(&TK, r, i, &off);
    StgInt len = chunk_len(&TK, c);
    acc = rope_snoc(ctx, &BK, acc, len, new_chunk(ctx, &BK, LP(c, 0), len, chunk_off(&TK, c), len));
    i += chunk_size(&TK, c);
  }
  return wrap_rope(ctx, &BK, acc, NULL);
}

// Text.fromUtf8: Right text over a copy of the bytes, as Text.fromText cuts
// it; NULL for invalid UTF-8 (the interpreter builds the Failure). `either`
// is the pooled Either enumeration.
void *unison_jit_text_from_utf8(UnisonJitCtx *ctx, void *bytes, void *either, int64_t right_tag) {
  StgClosure *r = rope_of(&BK, bytes);
  if (r == NULL) return NULL;
  StgInt len;
  unsigned char *heap;
  const unsigned char *p = rope_flat(&BK, r, &len, &heap);
  if (p == NULL) return NULL;
  StgInt count = utf8_count_valid(p, len);
  void *res = NULL;
  if (count >= 0) res = mk_data1(ctx, LP(either, 0), right_tag, text_copy(ctx, p, len), -1);
  free(heap);
  return res;
}

