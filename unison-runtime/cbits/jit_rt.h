// Shared state between the interpreter and JIT-compiled code. See docs/jit-design.md.
// The Haskell side (Unison.Runtime.JIT.Ctx) mirrors this layout and checks it at startup
// with unison_jit_ctx_layout.
#ifndef UNISON_JIT_RT_H
#define UNISON_JIT_RT_H

#include <stdint.h>

// One per Haskell thread that runs native code. Allocated with malloc; never moves.
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
  int64_t stress_poll;      // fire the entry poll every this many entries
  int64_t stress_poll_left; // countdown for the above
} UnisonJitCtx;

// Status values returned by native code. Positive values are exit indices.
#define UNISON_JIT_OK 0
#define UNISON_JIT_EXIT_ERROR (-1)

#endif
