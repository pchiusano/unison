// Runtime helpers for JIT-compiled code, and the pieces of the GHC runtime it needs.
// See docs/jit-design.md.

#include "Rts.h"
#include "jit_rt.h"

#include <pthread.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>

// Field offsets, so the Haskell side can check that its picture of Ctx matches this one.
// The order matches the fields of UnisonJitCtx.
int64_t unison_jit_ctx_layout(int64_t *out, int64_t n) {
  int64_t offs[] = {
      offsetof(UnisonJitCtx, ustk),        offsetof(UnisonJitCtx, bstk),
      offsetof(UnisonJitCtx, pool),        offsetof(UnisonJitCtx, stack_size),
      offsetof(UnisonJitCtx, hplim),       offsetof(UnisonJitCtx, ap),
      offsetof(UnisonJitCtx, fp),          offsetof(UnisonJitCtx, sp),
      offsetof(UnisonJitCtx, max_sp),      offsetof(UnisonJitCtx, stress_poll),
      offsetof(UnisonJitCtx, stress_poll_left), offsetof(UnisonJitCtx, stress_callee),
      offsetof(UnisonJitCtx, stress_callee_left), offsetof(UnisonJitCtx, frames),
      offsetof(UnisonJitCtx, n_frames),    offsetof(UnisonJitCtx, max_frames),
      offsetof(UnisonJitCtx, cstack_limit),
  };
  int64_t count = sizeof offs / sizeof offs[0];
  for (int64_t i = 0; i < n && i < count; i++) out[i] = offs[i];
  return sizeof(UnisonJitCtx);
}

// Marks a boxed array as changed in slots lo..hi, as writeArray# would have.
static void unison_jit_mark_bstk(void **elems, int64_t lo, int64_t hi) {
  StgMutArrPtrs *arr = (StgMutArrPtrs *)((StgWord *)elems - sizeofW(StgMutArrPtrs));
  SET_INFO((StgClosure *)arr, &stg_MUT_ARR_PTRS_DIRTY_info);
  for (int64_t card = lo >> MUT_ARR_PTRS_CARD_BITS; card <= hi >> MUT_ARR_PTRS_CARD_BITS; card++)
    *mutArrPtrsCard(arr, card) = 1;
}

// The address of HpLim for the capability running this thread. Valid for as long as the
// thread stays on this capability, which it does throughout an unsafe foreign call; the
// trampoline refreshes it on every entry. A Capability starts with the function table and
// then the register table, both of which are public types.
void **unison_jit_hplim_address(void) {
  Capability *cap = rts_unsafeGetMyCapability();
  StgRegTable *reg = (StgRegTable *)((char *)cap + sizeof(StgFunTable));
  return (void **)&reg->rHpLim;
}

// ---------------------------------------------------------------------------
// Entering native code

// Settings, copied into each context when it is created.
static int64_t stress_poll = 0;
static int64_t stress_callee = 0;
static int64_t trace = 0;
// How much C stack native code may use for non-tail calls, at most. The
// stress mode cstack=N makes this small.
static int64_t cstack_budget = 1 << 20;
// C stack kept free for the runtime's own C code (the GC in particular),
// below which native code never goes whatever the budget says.
#define CSTACK_RESERVE (256 * 1024)
// Every native non-tail call in progress uses at least 16 bytes of C stack,
// and a frame writes one record per inline Let it is inside of plus its
// own, so the budget bounds the number of frame records. Frames are far
// bigger than this in practice; the pages of the buffer that are never
// touched cost nothing.
#define MIN_NATIVE_FRAME 4

void unison_jit_configure(int64_t poll_every, int64_t callee_every, int64_t cstack, int64_t trace_on) {
  stress_poll = poll_every;
  stress_callee = callee_every;
  if (cstack > 0) cstack_budget = cstack;
  trace = trace_on;
}

// One context per OS thread. A Haskell thread stays on one OS thread for the
// whole of an unsafe foreign call, and everything that touches the context
// happens inside unison_jit_enter, so this is safe. Contexts are never freed;
// there are only as many as the runtime has worker threads.
static _Thread_local UnisonJitCtx *thread_ctx = NULL;
// The lowest address of this thread's C stack, plus the reserve.
static _Thread_local int64_t thread_stack_floor = 0;

static UnisonJitCtx *get_ctx(void) {
  if (thread_ctx == NULL) {
    UnisonJitCtx *ctx = calloc(1, sizeof(UnisonJitCtx));
    ctx->stress_poll = stress_poll;
    ctx->stress_poll_left = stress_poll;
    ctx->stress_callee = stress_callee;
    ctx->stress_callee_left = stress_callee;
    ctx->max_frames = cstack_budget / MIN_NATIVE_FRAME + 16;
    ctx->frames = calloc(ctx->max_frames, 3 * sizeof(int64_t));
    pthread_t self = pthread_self();
    int64_t top = (int64_t)pthread_get_stackaddr_np(self);
    int64_t size = (int64_t)pthread_get_stacksize_np(self);
    thread_stack_floor = top - size + CSTACK_RESERVE;
    if (trace)
      fprintf(stderr, "[jit] new context: thread stack %lld bytes, budget %lld, %lld frame records\n",
              (long long)size, (long long)cstack_budget, (long long)ctx->max_frames);
    thread_ctx = ctx;
  }
  return thread_ctx;
}

typedef int64_t (*UnisonNativeFn)(UnisonJitCtx *ctx, int64_t ap, int64_t fp, int64_t sp);

// Runs a native function. The arrays arrive as pointers to their first element,
// which is how GHC passes MutableByteArray# and MutableArray# to foreign calls.
// On return, out[0..2] hold the new ap, fp and sp, out[3] the number of
// frame records and out[4] a malloc'd copy of them (0 if there are none),
// which the caller frees. Returns the status.
int64_t unison_jit_enter(UnisonNativeFn fn, int64_t *ustk, void **bstk, void **pool,
                         int64_t stack_size, int64_t ap, int64_t fp, int64_t sp,
                         int64_t *out) {
  UnisonJitCtx *ctx = get_ctx();
  ctx->ustk = ustk;
  ctx->bstk = bstk;
  ctx->pool = pool;
  ctx->stack_size = stack_size;
  ctx->hplim = unison_jit_hplim_address();
  ctx->max_sp = sp;
  ctx->n_frames = 0;
  // The budget counts down from here, but never below the thread's floor.
  int64_t here = (int64_t)&ctx;
  int64_t limit = here - cstack_budget;
  ctx->cstack_limit = limit > thread_stack_floor ? limit : thread_stack_floor;
  if (trace)
    fprintf(stderr, "[jit] enter %p ap/fp/sp %lld/%lld/%lld hplim %p stress %lld/%lld (global %lld)\n", (void *)fn,
            (long long)ap, (long long)fp, (long long)sp, *ctx->hplim, (long long)ctx->stress_poll,
            (long long)ctx->stress_poll_left, (long long)stress_poll);
  int64_t status = fn(ctx, ap, fp, sp);
  if (trace) fprintf(stderr, "[jit] status %lld\n", (long long)status);
  out[0] = ctx->ap;
  out[1] = ctx->fp;
  out[2] = ctx->sp;
  out[3] = ctx->n_frames;
  out[4] = 0;
  if (ctx->n_frames > ctx->max_frames) {
    fprintf(stderr, "[jit] frame record buffer overflowed (%lld records)\n", (long long)ctx->n_frames);
    abort();
  }
  if (ctx->n_frames > 0) {
    size_t bytes = ctx->n_frames * 3 * sizeof(int64_t);
    int64_t *copy = malloc(bytes);
    memcpy(copy, ctx->frames, bytes);
    out[4] = (int64_t)copy;
    if (trace) fprintf(stderr, "[jit] %lld frame records\n", (long long)ctx->n_frames);
  }
  // Native code stores into bstk without a write barrier. Tell the GC which
  // parts changed, once, now that it is about to be allowed to run again.
  int64_t hi = ctx->max_sp < stack_size - 1 ? ctx->max_sp : stack_size - 1;
  if (hi >= fp + 1) unison_jit_mark_bstk(bstk, fp + 1 < 0 ? 0 : fp + 1, hi);
  return status;
}

// ---------------------------------------------------------------------------
// Closure layout probe (decision D7)

// elems[0] is a sample closure; elems[1..n-1] are the values in its pointer
// fields. Fills out[] with: info pointer, pointer tag, closure type, pointer
// field count, non-pointer field count, constructor tag, then for each
// payload word its raw value, then for each pointer field the index in elems
// of the object it points to (or -1). Returns the payload word count.
int64_t unison_jit_probe(StgClosure **elems, int64_t n, int64_t *out) {
  StgClosure *tagged = elems[0];
  StgClosure *c = UNTAG_CLOSURE(tagged);
  const StgInfoTable *itbl = get_itbl(c);
  int64_t ptrs = itbl->layout.payload.ptrs;
  int64_t nptrs = itbl->layout.payload.nptrs;
  out[0] = (int64_t)c->header.info;
  out[1] = GET_CLOSURE_TAG(tagged);
  out[2] = itbl->type;
  out[3] = ptrs;
  out[4] = nptrs;
  out[5] = itbl->srt;
  int64_t total = ptrs + nptrs;
  for (int64_t i = 0; i < total; i++) out[6 + i] = (int64_t)c->payload[i];
  for (int64_t i = 0; i < ptrs; i++) {
    StgClosure *field = UNTAG_CLOSURE(c->payload[i]);
    out[6 + total + i] = -1;
    for (int64_t k = 1; k < n; k++)
      if (UNTAG_CLOSURE(elems[k]) == field) out[6 + total + i] = k;
  }
  return total;
}

// For tracing: the current value of HpLim, as native code would see it.
int64_t unison_jit_hplim_value(void) { return (int64_t)*unison_jit_hplim_address(); }
