// Shared state between the interpreter and JIT-compiled code. See unison-runtime/src/Unison/Runtime/JIT/design.md.
// The Haskell side (Unison.Runtime.JIT.Codegen, CtxOffsets) mirrors this layout and checks
// it at startup with unison_jit_ctx_layout.
#ifndef UNISON_JIT_RT_H
#define UNISON_JIT_RT_H

#include <stdint.h>

// One per OS thread that runs native code. Allocated with malloc; never moves.
typedef struct UnisonJitCtx {
  // Set by the trampoline before every entry.
  int64_t *ustk;        // unboxed stack: element 0 of the MutableByteArray
  void **bstk;          // boxed stack: element 0 of the MutableArray
  void **pool;          // constant pool: element 0 of the MutableArray
  int64_t stack_size;   // number of slots in ustk and bstk
  void **hplim;         // address of the capability's HpLim; *hplim == NULL means "stop"

  // Written by native code before it returns.
  int64_t ap;
  int64_t fp;
  int64_t sp;
  // Highest slot native code may have written, kept up to date at every
  // function entry (sp + frame size). Used to mark the boxed stack on return.
  int64_t max_sp;

  // Stress modes (UNISON_JIT_STRESS). Zero means off.
  int64_t stress_poll;        // fire the entry poll every this many entries
  int64_t stress_poll_left;   // countdown for the above
  int64_t stress_callee;      // treat every this-many-th callee as not compiled
  int64_t stress_callee_left; // countdown for the above

  // Frame records: three words each (frame table index, sp - fp, fp - ap),
  // written by a native caller when its callee exits. Emptied at every entry.
  int64_t *frames;
  int64_t n_frames;
  int64_t max_frames;

  // Lowest C stack pointer native code may make a non-tail call at.
  int64_t cstack_limit;

  // The capability running this thread, for allocate(). Set at entry.
  void *cap;
  // The allocation budget, as an address: hp plus the words the budget still
  // allows. The entry poll fires once hp has passed it. Set by the trampoline
  // from the budget and moved by the slow path when it changes block.
  void *budget_end;
  // Inline bump allocation: the free pointer of the capability's current
  // allocation block (the register table's rCurrentAlloc), copied here by
  // the trampoline at entry, and the limit native code bumps it against: the
  // nearer of the block's end and budget_end. Native code calls
  // unison_jit_alloc_words when the object doesn't fit; the trampoline
  // writes hp back to the block on return. hp and hp_lim are NULL when
  // there is no block to bump in (before the first allocation, in a test
  // context, or with UNISON_JIT_BUMP=0), so that every allocation takes the
  // call.
  void *hp;
  void *hp_lim;
  // The current handler environment (HEnv): read by ability requests to
  // find their handler, switched by an affine handler case's InLocal and
  // switched back by its Yield. The trampoline copies it in from the boxed
  // stack's out-area at entry and back there on return, so that the
  // collector sees the pointer while Haskell runs; native code never keeps
  // one past a return.
  void *henv;
  // How many environment switches native code has in force (an InLocal
  // without its Yield yet): zero at entry; an InLocal takes the slow path
  // when it has reached UNISON_JIT_LOCALS, so that an exit never needs
  // more out-area pointers than there are.
  int64_t n_locals;

  // Not read by generated code (and so not in the layout the Haskell side
  // checks): the lowest address of this thread's C stack, plus the reserve.
  int64_t stack_floor;
  // Whether hp/hp_lim are in use in this context, and the words the slow
  // path handed out (which allocate() has already accounted for).
  int64_t bump;
  int64_t slow_words;
  // Pointers unison_jit_exit_frame has stored in the boxed out-area for
  // Local frame records since entry.
  int64_t n_local_ptrs;
} UnisonJitCtx;

// unison_jit_enter hands its results back in spare words past the last slot
// of the unboxed stack (nativeOutWords in Stack.hs): ap, fp, sp, the number
// of frame records, then up to this many records inline. More than this
// many go in a malloc'd copy.
#define UNISON_JIT_INLINE_FRAMES 8
#define UNISON_JIT_OUT_WORDS (4 + 3 * UNISON_JIT_INLINE_FRAMES)

// The boxed stack's out-area (nativeOutPtrs in Stack.hs): the handler
// environment, in at entry and out at return, then one environment per
// Local frame record (at most UNISON_JIT_LOCALS switches are in force at
// once, see n_locals).
#define UNISON_JIT_LOCALS 32
#define UNISON_JIT_OUT_PTRS (1 + UNISON_JIT_LOCALS)

// Status values returned by native code. Positive values are exit indices.
#define UNISON_JIT_OK 0
#define UNISON_JIT_EXIT_ERROR (-1)

#endif
