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
  // The handler environment (HEnv) the interpreter entered with: read by
  // ability requests to find their handler. Set by the trampoline from the
  // one-element array the Haskell side passes, so that the collector sees
  // the pointer while Haskell runs; never kept by native code past a return.
  void *henv;

  // Not read by generated code (and so not in the layout the Haskell side
  // checks): the lowest address of this thread's C stack, plus the reserve.
  int64_t stack_floor;
  // Whether hp/hp_lim are in use in this context, and the words the slow
  // path handed out (which allocate() has already accounted for).
  int64_t bump;
  int64_t slow_words;
} UnisonJitCtx;

// unison_jit_enter hands its results back in spare words past the last slot
// of the unboxed stack (nativeOutWords in Stack.hs): ap, fp, sp, the number
// of frame records, then up to this many records inline. More than this
// many go in a malloc'd copy.
#define UNISON_JIT_INLINE_FRAMES 8
#define UNISON_JIT_OUT_WORDS (4 + 3 * UNISON_JIT_INLINE_FRAMES)

// Status values returned by native code. Positive values are exit indices.
#define UNISON_JIT_OK 0
#define UNISON_JIT_EXIT_ERROR (-1)

#endif
