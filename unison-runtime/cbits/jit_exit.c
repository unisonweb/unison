// The exit routine, compiled to LLVM IR (cbits/jit_exit.ll, by
// cbits/gen-helpers.sh) and added to the JIT at startup as a module of its
// own, before any generated module. It is never compiled into the library:
// its calling convention is preserve_most, which only clang spells, and
// having LLVM compile it at startup means the build needs no particular C
// compiler and every target gets the convention the generated code asks
// for ("Write-back" in design.md).
//
// Where native code hands a frame back to the interpreter (an exit, or an
// unwind after a callee exited), it calls this with the frame's live slots
// in an argument block instead of writing them itself: one call instruction
// per site plus the stores into the block, where the generated stores were
// most of a function's code. What to do with them is a constant descriptor
// in the module, built by Codegen.joinCall:
//
//   [flags, base, fbase, depth, nslots, nrecs, slot offsets..., records...]
//
// flags bit 0: an exit, so record ap, fp and sp in the context (fp + base
// is the interpreter's frame base, fp + depth its stack pointer); bit 1:
// the code is not inside an inline binding, so ap is the ap argument,
// otherwise it is the frame base. The argument block holds a word and a
// pointer per slot, in the order of the offsets. Each record is three words
// (frame table index, frame size, pending arguments), written after the
// frame records already there; a negative pending count means
// fp + fbase - ap, the count of the outermost frame. A record whose index
// is negative is a Local frame (an affine handler case's environment switch,
// in force at the exit): the environment to switch back to is one more
// pointer in the block, after the slots, which goes in the boxed stack's
// out-area (a collector root) and the record's index becomes -1 - its
// position there. The site returns the status itself (a constant there,
// which LLVM would lose if this handed it back).
//
// preserve_most: generated code calls this with that convention (every
// register but the argument and scratch ones preserved by the callee), so
// that a site keeps the caller's live values in scratch registers across
// it; see Codegen.joinCall. The block is a fixed-size alloca in the
// caller's entry block, not variadic arguments: clang's IR for a variadic
// function carries the target's va_list ABI, which this file must not.
//
// Only jit_rt.h may be included: the IR must stay platform-independent
// (see "The IR helpers" in design.md), and the context is int64_t and
// pointer fields, the same on every 64-bit target.

#include "jit_rt.h"

// jit_rt.c: reports a Local record overflow and aborts (the IR may not
// use stdio itself).
void unison_jit_exit_fail(void);

__attribute__((preserve_most)) void unison_jit_exit_frame(UnisonJitCtx *ctx, const int64_t *d, int64_t fp, int64_t ap,
                                                          const int64_t *args) {
  int64_t flags = d[0], base = d[1], fbase = d[2], depth = d[3], nslots = d[4], nrecs = d[5];
  const int64_t *slots = d + 6, *recs = slots + nslots;
  int64_t *ustk = ctx->ustk;
  void **bstk = ctx->bstk;
  for (int64_t i = 0; i < nslots; i++) {
    int64_t k = fp + slots[i];
    ustk[k] = args[2 * i];
    bstk[k] = (void *)args[2 * i + 1];
  }
  const int64_t *extra = args + 2 * nslots;
  if (flags & 1) {
    ctx->ap = (flags & 2) ? ap : fp + base;
    ctx->fp = fp + base;
    ctx->sp = fp + depth;
    if (fp + depth > ctx->max_sp) ctx->max_sp = fp + depth;
  }
  // the buffer is sized from the C stack budget (jit_rt.c), which bounds
  // the records a chain of native frames can write; the check makes the
  // bound explicit rather than trusting it (security review, 2026-10-24)
  if (__builtin_expect(ctx->n_frames + nrecs > ctx->max_frames, 0)) unison_jit_exit_fail();
  int64_t *fr = ctx->frames + 3 * ctx->n_frames;
  for (int64_t i = 0; i < nrecs; i++, recs += 3, fr += 3) {
    if (recs[0] < 0) {
      int64_t idx = ctx->n_local_ptrs++;
      if (__builtin_expect(idx >= UNISON_JIT_LOCALS, 0)) unison_jit_exit_fail();
      bstk[ctx->stack_size + 1 + idx] = (void *)*extra++;
      fr[0] = -1 - idx;
      fr[1] = 0;
    } else {
      fr[0] = recs[0];
      fr[1] = recs[1];
    }
    fr[2] = recs[2] < 0 ? fp + fbase - ap : recs[2];
  }
  ctx->n_frames += nrecs;
}
