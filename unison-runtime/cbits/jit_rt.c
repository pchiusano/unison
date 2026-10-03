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
      offsetof(UnisonJitCtx, cstack_limit), offsetof(UnisonJitCtx, cap),
      offsetof(UnisonJitCtx, alloc_left),
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

// The address of HpLim for a capability. Valid for as long as the thread stays on the
// capability, which it does throughout an unsafe foreign call; the trampoline refreshes it
// on every entry. A Capability starts with the function table and then the register table,
// both of which are public types.
static inline void **hplim_of(Capability *cap) {
  StgRegTable *reg = (StgRegTable *)((char *)cap + sizeof(StgFunTable));
  return (void **)&reg->rHpLim;
}

void **unison_jit_hplim_address(void) { return hplim_of(rts_unsafeGetMyCapability()); }

// ---------------------------------------------------------------------------
// Entering native code

// Settings, copied into each context when it is created.
static int64_t stress_poll = 0;
static int64_t stress_callee = 0;
static int64_t trace = 0;
// How much C stack native code may use for non-tail calls, at most. The
// stress mode cstack=N makes this small.
static int64_t cstack_budget = 1 << 20;
// Words native code may allocate between two polls. The stress mode
// alloc=N makes this small.
static int64_t alloc_budget = 256 * 1024;
// C stack kept free for the runtime's own C code (the GC in particular),
// below which native code never goes whatever the budget says.
#define CSTACK_RESERVE (256 * 1024)
// Every native non-tail call in progress uses at least 16 bytes of C stack,
// and a frame writes one record per inline Let it is inside of plus its
// own, so the budget bounds the number of frame records. Frames are far
// bigger than this in practice; the pages of the buffer that are never
// touched cost nothing.
#define MIN_NATIVE_FRAME 4

// Generated code calls the helpers below by name, and the JIT finds them in
// the process's symbol table. The linker drops a function nothing refers to
// (GHC links with -dead_strip), so each is referred to here, from a function
// that is certainly kept.
void *unison_jit_alloc_words(UnisonJitCtx *ctx, int64_t n);
void unison_jit_write_mutvar(UnisonJitCtx *ctx, StgMutVar *mv, StgClosure *v);
int64_t unison_jit_list_size(void *list);
void *unison_jit_list_view(UnisonJitCtx *ctx, void *list, void *empty, int64_t elem_tag, int64_t left);
void *unison_jit_list_push(UnisonJitCtx *ctx, void *list, int64_t u, void *b, int64_t front);
void *unison_jit_list_index(UnisonJitCtx *ctx, void *list, int64_t i, void *none, int64_t some_tag);
void *unison_jit_list_lit(UnisonJitCtx *ctx, void *acc, int64_t u, void *b);
void *unison_jit_list_wrap(UnisonJitCtx *ctx, void *acc);
void *unison_jit_list_cut(UnisonJitCtx *ctx, void *list, int64_t n, int64_t take);
void *unison_jit_list_split(UnisonJitCtx *ctx, void *list, int64_t n, void *empty, int64_t elem_tag, int64_t left);
void *unison_jit_list_append(UnisonJitCtx *ctx, void *x, void *y);
int64_t unison_jit_text_size(void *text);
void *unison_jit_text_append(UnisonJitCtx *ctx, void *x, void *y);
void *unison_jit_text_cut(UnisonJitCtx *ctx, void *text, int64_t n, int64_t take);
int64_t unison_jit_text_eq(void *x, void *y);
void *unison_jit_name(UnisonJitCtx *ctx, void *fun, int64_t n, int64_t u0, void *b0, int64_t u1, void *b1, int64_t u2,
                      void *b2, int64_t u3, void *b3);
void *unison_jit_helpers[] = {
    (void *)&unison_jit_alloc_words, (void *)&unison_jit_write_mutvar, (void *)&unison_jit_list_size,
    (void *)&unison_jit_list_view,   (void *)&unison_jit_list_push,    (void *)&unison_jit_list_index,
    (void *)&unison_jit_text_size,   (void *)&unison_jit_text_append,  (void *)&unison_jit_text_cut,
    (void *)&unison_jit_text_eq,     (void *)&unison_jit_name,         (void *)&unison_jit_list_lit,
    (void *)&unison_jit_list_wrap,   (void *)&unison_jit_list_cut,     (void *)&unison_jit_list_split,
    (void *)&unison_jit_list_append,
};

void unison_jit_configure(int64_t poll_every, int64_t callee_every, int64_t cstack, int64_t alloc,
                          int64_t trace_on) {
  for (size_t i = 0; i < sizeof unison_jit_helpers / sizeof unison_jit_helpers[0]; i++)
    if (unison_jit_helpers[i] == NULL) abort();
  stress_poll = poll_every;
  stress_callee = callee_every;
  if (cstack > 0) cstack_budget = cstack;
  if (alloc > 0) alloc_budget = alloc;
  trace = trace_on;
}

// ---------------------------------------------------------------------------
// Allocation

// Generated code calls this once per straight-line run to get room for the
// objects the run builds. allocate() never returns NULL (it aborts on heap
// overflow like Haskell code would); the budget in Ctx bounds how much can
// be allocated before the interpreter gets a chance to run a GC.
void *unison_jit_alloc_words(UnisonJitCtx *ctx, int64_t n) {
  return allocate((Capability *)ctx->cap, n);
}

// Writes a MutVar# (an IORef) the way compiled Haskell does: the store,
// then the runtime's barrier, which moves a clean variable onto the
// mutable list. Cold code in C (decision D9).
void unison_jit_write_mutvar(UnisonJitCtx *ctx, StgMutVar *mv, StgClosure *v) {
  StgClosure *old = mv->var;
  __atomic_thread_fence(__ATOMIC_RELEASE);
  mv->var = v;
  // the register table follows the function table at the start of a Capability
  // (see unison_jit_hplim_address)
  StgRegTable *reg = (StgRegTable *)((char *)ctx->cap + sizeof(StgFunTable));
  dirty_MUT_VAR(reg, mv, old);
}

// Addresses and sizes from the runtime that generated code needs: the
// info pointers for the two kinds of array in a Seg, and their layouts.
int64_t unison_jit_rts_facts(int64_t *out, int64_t n) {
  int64_t facts[] = {
      (int64_t)&stg_ARR_WORDS_info,                // 0: ByteArray# info
      (int64_t)&stg_MUT_ARR_PTRS_FROZEN_CLEAN_info, // 1: frozen Array# info
      sizeofW(StgArrBytes),                         // 2: ByteArray# header words (incl. the byte count)
      offsetof(StgArrBytes, bytes) / sizeof(W_),    // 3: word index of the byte count
      sizeofW(StgMutArrPtrs),                       // 4: Array# header words (incl. ptrs and size)
      offsetof(StgMutArrPtrs, ptrs) / sizeof(W_),   // 5: word index of the element count
      offsetof(StgMutArrPtrs, size) / sizeof(W_),   // 6: word index of the payload size (elements + card table)
      MUT_ARR_PTRS_CARD_BITS,                       // 7: log2 of elements per card
      (int64_t)&unison_jit_alloc_words,             // 8: the allocator
      offsetof(StgMutVar, var) / sizeof(W_),        // 9: word index of a MutVar#'s content
      (int64_t)&unison_jit_write_mutvar,            // 10: the MutVar# write barrier
      (int64_t)&stg_MUT_ARR_PTRS_DIRTY_info,        // 11: mutable Array# info after a write
  };
  int64_t count = sizeof facts / sizeof facts[0];
  for (int64_t i = 0; i < n && i < count; i++) out[i] = facts[i];
  return count;
}

// ---------------------------------------------------------------------------
// Lists
//
// A Unison list is a Unison.Util.Deque of Val (a strict finger tree), held
// as Foreign (WrapSeq deque). Every field is strict, so everything reachable
// from a list is an evaluated, tagged constructor and can be read and built
// here without evaluating anything. These helpers are ports of the Haskell
// operations in lib/unison-util-rope's Deque.hs (same results, structure
// included) and take every case: a helper answers "not handled" (NULL, or a
// negative size) only for a closure that isn't a list. They allocate with
// allocate(), like generated code, and never call into Haskell.
//
// The layouts are GHC's for the constructors in Deque.hs (pointer fields
// first, in declaration order, then the unpacked words):
//
//   SList: SNil (tag 1), SCons x rest (tag 2)
//   Deque: Nil (tag 1), Deep t pr m sf (tag 2): pointers pr, m, sf; word t
//   Mid:   MNil (tag 1), MDeep t ps pr m sf (tag 2): pointers pr, m, sf;
//          words t, ps
//   Node:  N8 a .. h (tag 1): eight pointers
//          NA size kids (tag 2): the pointer to a SmallArray#, then the size
//
// t packs the number of leaves (bits 8 and up), the suffix's item count
// (bits 4 to 7) and the prefix's (bits 0 to 3). A prefix runs front to back
// and a suffix back to front. A Deque is the top level, whose items are the
// elements; a Mid is a level below, whose items are nodes, and ps is the
// number of leaves under its prefix. The code here handles both with one set
// of functions that take `top`.
//
// The layouts are checked against sample lists at startup, see
// unison_jit_list_init and unison_jit_list_check, and then every helper is
// run against the Haskell operation; if anything is off the JIT stays off.

typedef struct {
  StgWord foreign_info, wrapseq_info, wrapseq_tag;
  StgWord deep_info, mdeep_info, scons_info, n8_info, na_info;
  StgWord val_info, data1_info, data2_info;
  StgClosure *nil, *mnil, *snil; // the constructors without fields (tagged)
} ListFacts;
static ListFacts LF;

#define LTAG(p) GET_CLOSURE_TAG((StgClosure *)(p))
#define LUN(p) UNTAG_CLOSURE((StgClosure *)(p))
#define LP(c, i) (LUN(c)->payload[i])
#define LW(c, i) ((StgInt)LUN(c)->payload[i])

// the most items in a digit (Deque.maxD)
#define MAXD 10
#define TSIZE(t) ((t) >> 8)
#define TPC(t) ((t)&15)
#define TSC(t) (((t) >> 4) & 15)
#define MK(n, p, s) (((StgInt)(n) << 8) | ((StgInt)(s) << 4) | (StgInt)(p))
#define HEAD(l) LP(l, 0)
#define TAIL(l) LP(l, 1)
#define IS_CONS(l) (LTAG(l) == 2)

// one level, read out of a Deep or an MDeep
typedef struct {
  StgClosure *pr, *m, *sf;
  StgInt t, ps;
} Lv;

static inline int lv_read(StgClosure *c, int top, Lv *v) {
  if (LTAG(c) != 2) return 0;
  v->pr = LP(c, 0), v->m = LP(c, 1), v->sf = LP(c, 2);
  v->t = LW(c, 3);
  v->ps = top ? 0 : LW(c, 4);
  return 1;
}

static inline StgInt mid_size(StgClosure *m) { return LTAG(m) == 2 ? TSIZE(LW(m, 3)) : 0; }

static inline StgClosure **node_kids(StgClosure *nd) {
  return LTAG(nd) == 1 ? LUN(nd)->payload : ((StgSmallMutArrPtrs *)LP(nd, 0))->payload;
}
static inline StgInt node_arity(StgClosure *nd) {
  return LTAG(nd) == 1 ? 8 : (StgInt)((StgSmallMutArrPtrs *)LP(nd, 0))->ptrs;
}
static inline StgInt node_size(StgClosure *nd) { return LTAG(nd) == 1 ? 8 : LW(nd, 1); }

// The deque inside a list value (tagged), or NULL if the closure isn't a list.
static inline StgClosure *deque_of(void *list) {
  if (LTAG(list) != 7 || (StgWord)LUN(list)->header.info != LF.foreign_info) return NULL;
  StgClosure *w = LP(list, 0);
  if (LTAG(w) != LF.wrapseq_tag || (StgWord)LUN(w)->header.info != LF.wrapseq_info) return NULL;
  return LP(w, 0);
}

// Everything allocated is written in full before the helper returns: the
// heap must have no gaps.
static inline StgWord *list_alloc(UnisonJitCtx *ctx, int64_t words) {
  ctx->alloc_left -= words;
  return (StgWord *)allocate((Capability *)ctx->cap, words);
}

// --- building ---

// Foreign (WrapSeq dq), in four words at p.
static inline void *wrap_at(StgWord *p, StgClosure *dq) {
  p[0] = LF.wrapseq_info;
  p[1] = (StgWord)dq;
  p[2] = LF.foreign_info;
  p[3] = (StgWord)p | LF.wrapseq_tag;
  return (void *)((StgWord)(p + 2) | 7);
}

static inline void *wrap_list(UnisonJitCtx *ctx, StgClosure *dq) { return wrap_at(list_alloc(ctx, 4), dq); }

#define DEEP_WORDS 5
static inline StgClosure *deep_at(StgWord *p, StgInt t, StgClosure *pr, StgClosure *m, StgClosure *sf) {
  p[0] = LF.deep_info;
  p[1] = (StgWord)pr;
  p[2] = (StgWord)m;
  p[3] = (StgWord)sf;
  p[4] = (StgWord)t;
  return (StgClosure *)((StgWord)p | 2);
}

static inline StgClosure *scons_at(StgWord *p, StgClosure *x, StgClosure *rest) {
  p[0] = LF.scons_info;
  p[1] = (StgWord)x;
  p[2] = (StgWord)rest;
  return (StgClosure *)((StgWord)p | 2);
}

static inline StgClosure *scons(UnisonJitCtx *ctx, StgClosure *x, StgClosure *rest) {
  return scons_at(list_alloc(ctx, 3), x, rest);
}

// a Deep (ps isn't stored) or an MDeep
static inline StgClosure *mk_level(UnisonJitCtx *ctx, int top, StgInt t, StgInt ps, StgClosure *pr, StgClosure *m,
                                   StgClosure *sf) {
  if (top) return deep_at(list_alloc(ctx, DEEP_WORDS), t, pr, m, sf);
  StgWord *p = list_alloc(ctx, 6);
  p[0] = LF.mdeep_info;
  p[1] = (StgWord)pr;
  p[2] = (StgWord)m;
  p[3] = (StgWord)sf;
  p[4] = (StgWord)t;
  p[5] = (StgWord)ps;
  return (StgClosure *)((StgWord)p | 2);
}

static inline StgClosure *mk_n8(UnisonJitCtx *ctx, StgClosure **k) {
  StgWord *p = list_alloc(ctx, 9);
  p[0] = LF.n8_info;
  for (int i = 0; i < 8; i++) p[1 + i] = (StgWord)k[i];
  return (StgClosure *)((StgWord)p | 1);
}

// NA size (the n items at k, in an array)
static inline StgClosure *mk_na(UnisonJitCtx *ctx, StgInt size, StgClosure **k, StgInt n) {
  StgWord *p = list_alloc(ctx, 2 + n + 3);
  p[0] = (StgWord)&stg_SMALL_MUT_ARR_PTRS_FROZEN_CLEAN_info;
  p[1] = (StgWord)n;
  for (StgInt i = 0; i < n; i++) p[2 + i] = (StgWord)k[i];
  StgWord *q = p + 2 + n;
  q[0] = LF.na_info;
  q[1] = (StgWord)p;
  q[2] = (StgWord)size;
  return (StgClosure *)((StgWord)q | 2);
}

// the first k cells of a list, copied (all of them if it is shorter)
static StgClosure *s_take(UnisonJitCtx *ctx, StgInt k, StgClosure *l) {
  StgClosure *res = LF.snil, **tail = &res;
  for (; k > 0 && IS_CONS(l); k--, l = TAIL(l)) {
    StgWord *p = list_alloc(ctx, 3);
    *tail = scons_at(p, HEAD(l), LF.snil);
    tail = (StgClosure **)&p[2];
  }
  return res;
}

static inline StgClosure *s_drop(StgInt k, StgClosure *l) {
  for (; k > 0 && IS_CONS(l); k--) l = TAIL(l);
  return l;
}

// the first list reversed, then acc
static StgClosure *s_rev_onto(UnisonJitCtx *ctx, StgClosure *l, StgClosure *acc) {
  for (; IS_CONS(l); l = TAIL(l)) acc = scons(ctx, HEAD(l), acc);
  return acc;
}

// a copy of the first list that ends in the second
static StgClosure *s_append(UnisonJitCtx *ctx, StgClosure *a, StgClosure *b) {
  StgClosure *res = b, **tail = &res;
  for (; IS_CONS(a); a = TAIL(a)) {
    StgWord *p = list_alloc(ctx, 3);
    *tail = scons_at(p, HEAD(a), b);
    tail = (StgClosure **)&p[2];
  }
  return res;
}

// the leaves under a list of nodes
static inline StgInt s_sum(StgClosure *l) {
  StgInt s = 0;
  for (; IS_CONS(l); l = TAIL(l)) s += node_size(HEAD(l));
  return s;
}

// a list's items into an array, in list order; their number
static inline StgInt s_items(StgClosure *l, StgClosure **out) {
  StgInt n = 0;
  for (; IS_CONS(l); l = TAIL(l)) out[n++] = HEAD(l);
  return n;
}

// k[0], ..., k[n-1], then rest
static StgClosure *s_from(UnisonJitCtx *ctx, StgClosure **k, StgInt n, StgClosure *rest) {
  for (StgInt i = n - 1; i >= 0; i--) rest = scons(ctx, k[i], rest);
  return rest;
}

// k[n-1], ..., k[0], then rest
static StgClosure *s_from_rev(UnisonJitCtx *ctx, StgClosure **k, StgInt n, StgClosure *rest) {
  for (StgInt i = 0; i < n; i++) rest = scons(ctx, k[i], rest);
  return rest;
}

// all but the first `from` children of a node, front to back (nodeDropFwd)
static inline StgClosure *kids_fwd(UnisonJitCtx *ctx, StgClosure *nd, StgInt from) {
  return s_from(ctx, node_kids(nd) + from, node_arity(nd) - from, LF.snil);
}

// the first k children of a node, back to front (nodeTakeRev)
static inline StgClosure *kids_rev(UnisonJitCtx *ctx, StgClosure *nd, StgInt k) {
  return s_from_rev(ctx, node_kids(nd), k, LF.snil);
}

// --- adding at the ends (cons, consFull, consM; snoc, snocFull, snocM) ---

static StgClosure *lv_cons(UnisonJitCtx *ctx, int top, StgClosure *x, StgClosure *c) {
  Lv v;
  StgInt s = top ? 1 : node_size(x);
  if (!lv_read(c, top, &v)) return mk_level(ctx, top, MK(s, 1, 0), s, scons(ctx, x, LF.snil), LF.mnil, LF.snil);
  if (TPC(v.t) < MAXD) return mk_level(ctx, top, v.t + (s << 8) + 1, v.ps + s, scons(ctx, x, v.pr), v.m, v.sf);
  if (TSC(v.t) == 0) {
    // no middle yet: the far half of the prefix becomes the suffix
    StgClosure *kept = s_take(ctx, 5, v.pr);
    StgInt ps = top ? 0 : s + s_sum(kept);
    return mk_level(ctx, top, MK(TSIZE(v.t) + s, 6, 5), ps, scons(ctx, x, kept), LF.mnil,
                    s_rev_onto(ctx, s_drop(5, v.pr), LF.snil));
  }
  // the prefix keeps its two outermost items and sheds the other eight
  StgClosure *k[MAXD];
  s_items(v.pr, k);
  StgInt keep = top ? 2 : node_size(k[0]) + node_size(k[1]);
  StgClosure *nd = top ? mk_n8(ctx, k + 2) : mk_na(ctx, v.ps - keep, k + 2, 8);
  StgClosure *m = lv_cons(ctx, 0, nd, v.m);
  return mk_level(ctx, top, v.t + (s << 8) - 7, s + keep, scons(ctx, x, s_from(ctx, k, 2, LF.snil)), m, v.sf);
}

static StgClosure *lv_snoc(UnisonJitCtx *ctx, int top, StgClosure *c, StgClosure *x) {
  Lv v;
  StgInt s = top ? 1 : node_size(x);
  if (!lv_read(c, top, &v)) return mk_level(ctx, top, MK(s, 0, 1), 0, LF.snil, LF.mnil, scons(ctx, x, LF.snil));
  if (TSC(v.t) < MAXD) return mk_level(ctx, top, v.t + (s << 8) + 0x10, v.ps, v.pr, v.m, scons(ctx, x, v.sf));
  if (TPC(v.t) == 0) {
    StgClosure *pr = s_rev_onto(ctx, s_drop(5, v.sf), LF.snil);
    return mk_level(ctx, top, MK(TSIZE(v.t) + s, 5, 6), top ? 0 : s_sum(pr), pr, LF.mnil,
                    scons(ctx, x, s_take(ctx, 5, v.sf)));
  }
  // the suffix runs back to front: s1, s2, then the eight to shed, last first
  StgClosure *k[MAXD], *shed[8];
  s_items(v.sf, k);
  for (int i = 0; i < 8; i++) shed[i] = k[9 - i];
  StgInt sz = top ? 8 : TSIZE(v.t) - v.ps - mid_size(v.m) - node_size(k[0]) - node_size(k[1]);
  StgClosure *nd = top ? mk_n8(ctx, shed) : mk_na(ctx, sz, shed, 8);
  StgClosure *m = lv_snoc(ctx, 0, v.m, nd);
  return mk_level(ctx, top, v.t + (s << 8) - 0x70, v.ps, v.pr, m, scons(ctx, x, s_from(ctx, k, 2, LF.snil)));
}

// --- removing at the ends (uncons, unconsLast, unconsSlow, unconsM; and unsnoc) ---

// The first item of a level that isn't empty, and the level without it.
static StgClosure *lv_uncons(UnisonJitCtx *ctx, int top, StgClosure *c, StgClosure **out) {
  Lv v;
  lv_read(c, top, &v);
  StgClosure *empty = top ? LF.nil : LF.mnil;
  if (IS_CONS(v.pr)) {
    StgClosure *x = HEAD(v.pr), *rest = TAIL(v.pr);
    StgInt s = top ? 1 : node_size(x);
    *out = x;
    if (!IS_CONS(rest) && LTAG(v.m) == 2) {
      // the prefix's only item goes: the first node of the middle becomes the prefix
      StgClosure *nn;
      StgClosure *m = lv_uncons(ctx, 0, v.m, &nn);
      return mk_level(ctx, top, v.t - (s << 8) - 1 + node_arity(nn), node_size(nn), kids_fwd(ctx, nn, 0), m, v.sf);
    }
    StgInt t = v.t - (s << 8) - 1;
    return t < 0x100 ? empty : mk_level(ctx, top, t, v.ps - s, rest, v.m, v.sf);
  }
  // the prefix is empty, so there is no middle: half of the suffix moves over
  StgInt sc = TSC(v.t), q = (sc - 1) / 2, p = sc - 1 - q;
  StgClosure *l = s_rev_onto(ctx, s_drop(q, v.sf), LF.snil);
  StgClosure *x = HEAD(l), *pr = TAIL(l);
  *out = x;
  if (sc == 1) return empty;
  return mk_level(ctx, top, MK(TSIZE(v.t) - (top ? 1 : node_size(x)), p, q), top ? 0 : s_sum(pr), pr, LF.mnil,
                  s_take(ctx, q, v.sf));
}

static StgClosure *lv_unsnoc(UnisonJitCtx *ctx, int top, StgClosure *c, StgClosure **out) {
  Lv v;
  lv_read(c, top, &v);
  StgClosure *empty = top ? LF.nil : LF.mnil;
  if (IS_CONS(v.sf)) {
    StgClosure *x = HEAD(v.sf), *rest = TAIL(v.sf);
    StgInt s = top ? 1 : node_size(x);
    *out = x;
    if (!IS_CONS(rest) && LTAG(v.m) == 2) {
      StgClosure *nn;
      StgClosure *m = lv_unsnoc(ctx, 0, v.m, &nn);
      StgInt k = node_arity(nn);
      return mk_level(ctx, top, v.t - (s << 8) - 0x10 + (k << 4), v.ps, v.pr, m, kids_rev(ctx, nn, k));
    }
    StgInt t = v.t - (s << 8) - 0x10;
    return t < 0x100 ? empty : mk_level(ctx, top, t, v.ps, v.pr, v.m, rest);
  }
  StgInt pc = TPC(v.t), p = (pc - 1) / 2, q = pc - 1 - p;
  StgClosure *l = s_rev_onto(ctx, s_drop(p, v.pr), LF.snil);
  StgClosure *x = HEAD(l), *sf = TAIL(l);
  *out = x;
  if (pc == 1) return empty;
  StgClosure *pr = s_take(ctx, p, v.pr);
  return mk_level(ctx, top, MK(TSIZE(v.t) - (top ? 1 : node_size(x)), p, q), top ? 0 : s_sum(pr), pr, LF.mnil, sf);
}

// A level whose suffix would be empty: the suffix gets the last node of the
// middle, if there is one (deepR, mdeepR). And the same for a prefix.
static StgClosure *lv_deep_r(UnisonJitCtx *ctx, int top, StgInt n, StgInt pc, StgInt ps, StgClosure *pr,
                             StgClosure *m) {
  if (LTAG(m) != 2) return mk_level(ctx, top, MK(n, pc, 0), ps, pr, LF.mnil, LF.snil);
  StgClosure *nd;
  StgClosure *m2 = lv_unsnoc(ctx, 0, m, &nd);
  StgInt k = node_arity(nd);
  return mk_level(ctx, top, MK(n, pc, k), ps, pr, m2, kids_rev(ctx, nd, k));
}

static StgClosure *lv_deep_l(UnisonJitCtx *ctx, int top, StgInt n, StgInt sc, StgClosure *m, StgClosure *sf) {
  if (LTAG(m) != 2) return mk_level(ctx, top, MK(n, 0, sc), 0, LF.snil, LF.mnil, sf);
  StgClosure *nd;
  StgClosure *m2 = lv_uncons(ctx, 0, m, &nd);
  return mk_level(ctx, top, MK(n, node_arity(nd), sc), node_size(nd), kids_fwd(ctx, nd, 0), m2, sf);
}

// --- lookup ---

static inline StgClosure *s_nth(StgClosure *l, StgInt k) {
  while (k-- > 0) l = TAIL(l);
  return HEAD(l);
}

// The node of a middle holding its leaf number i, and the leaf's offset in
// the node (lookM). A full child of this level's nodes has 2^sh leaves; sh is
// negative for a text's middle, where a node's size says nothing of its
// children's.
static StgClosure *mid_look(StgClosure *c, int sh, StgInt i, StgInt *off) {
  Lv v;
  lv_read(c, 0, &v);
  if (i < v.ps) {
    for (StgClosure *l = v.pr;; l = TAIL(l)) {
      StgClosure *nd = HEAD(l);
      StgInt s = node_size(nd);
      if (i < s) {
        *off = i;
        return nd;
      }
      i -= s;
    }
  }
  StgInt i2 = i - v.ps;
  if (i2 < mid_size(v.m)) {
    StgInt o;
    StgClosure *nn = mid_look(v.m, sh < 0 ? sh : sh + 3, i2, &o);
    StgClosure **kids = node_kids(nn);
    if (sh >= 0 && node_size(nn) == (StgInt)8 << (sh + 3) && node_arity(nn) == 8) { // all eight children full
      *off = o & (((StgInt)1 << (sh + 3)) - 1);
      return kids[o >> (sh + 3)];
    }
    for (StgInt q = 0;; q++) {
      StgInt s = node_size(kids[q]);
      if (o < s) {
        *off = o;
        return kids[q];
      }
      o -= s;
    }
  }
  StgInt r = TSIZE(v.t) - 1 - i; // leaves after the one looked for
  for (StgClosure *l = v.sf;; l = TAIL(l)) {
    StgClosure *nd = HEAD(l);
    StgInt s = node_size(nd);
    if (r < s) {
      *off = s - 1 - r;
      return nd;
    }
    r -= s;
  }
}

// --- take and drop ---

// The first j leaves of a middle, 0 < j <= size (takeM): returns everything
// before the node holding the last of them; that node, and how many of its
// leaves are among them.
static StgClosure *mid_take(UnisonJitCtx *ctx, StgClosure *c, StgInt j, StgClosure **node, StgInt *k) {
  Lv v;
  lv_read(c, 0, &v);
  StgInt n = TSIZE(v.t), pc = TPC(v.t), sc = TSC(v.t), ms = mid_size(v.m);
  if (j <= v.ps) {
    StgInt q = 0, sb = 0; // q whole nodes of the prefix, with sb leaves, come before the cut
    for (StgClosure *l = v.pr;; l = TAIL(l), q++) {
      StgClosure *nd = HEAD(l);
      StgInt s = node_size(nd);
      if (j <= sb + s) {
        *node = nd;
        *k = j - sb;
        return q == 0 ? LF.mnil : mk_level(ctx, 0, MK(sb, q, 0), sb, s_take(ctx, q, v.pr), LF.mnil, LF.snil);
      }
      sb += s;
    }
  }
  if (j <= v.ps + ms) {
    StgClosure *nn;
    StgInt kk;
    StgClosure *m2 = mid_take(ctx, v.m, j - v.ps, &nn, &kk);
    // the children of the node the level below cut in: those before the one
    // holding the cut make the new suffix
    StgClosure **kids = node_kids(nn);
    StgInt q = 0, sb = 0;
    for (;; q++) {
      StgInt s = node_size(kids[q]);
      if (kk - 1 < sb + s) break;
      sb += s;
    }
    StgInt total = v.ps + mid_size(m2) + sb;
    *node = kids[q];
    *k = kk - sb;
    if (total == 0) return LF.mnil;
    if (q == 0) return lv_deep_r(ctx, 0, total, pc, v.ps, v.pr, m2);
    return mk_level(ctx, 0, MK(total, pc, q), v.ps, v.pr, m2, kids_rev(ctx, nn, q));
  }
  StgInt r = n - j, cnt = sc; // r leaves are to go from the back; cnt nodes of the suffix are left
  for (StgClosure *l = v.sf;; l = TAIL(l), cnt--) {
    StgClosure *nd = HEAD(l);
    StgInt s = node_size(nd);
    if (r >= s) {
      r -= s;
      continue;
    }
    StgInt keep = s - r, total = j - keep;
    *node = nd;
    *k = keep;
    if (total == 0) return LF.mnil;
    if (cnt == 1) return lv_deep_r(ctx, 0, total, pc, v.ps, v.pr, v.m);
    return mk_level(ctx, 0, MK(total, pc, cnt - 1), v.ps, v.pr, v.m, TAIL(l));
  }
}

// All but the first j leaves of a middle, 0 <= j < size (dropM): returns
// everything after the node holding leaf j; that node, and how many of its
// leaves to drop.
static StgClosure *mid_drop(UnisonJitCtx *ctx, StgClosure *c, StgInt j, StgClosure **node, StgInt *k) {
  Lv v;
  lv_read(c, 0, &v);
  StgInt n = TSIZE(v.t), pc = TPC(v.t), sc = TSC(v.t), ms = mid_size(v.m);
  if (j < v.ps) {
    // kk leaves still to drop; the prefix has cnt nodes and psz leaves left, the level tot
    StgInt kk = j, cnt = pc, psz = v.ps, tot = n;
    for (StgClosure *l = v.pr;; l = TAIL(l)) {
      StgClosure *nd = HEAD(l);
      StgInt s = node_size(nd);
      if (kk >= s) {
        kk -= s, cnt--, psz -= s, tot -= s;
        continue;
      }
      *node = nd;
      *k = kk;
      if (tot == s) return LF.mnil;
      if (cnt == 1) return lv_deep_l(ctx, 0, tot - s, sc, v.m, v.sf);
      return mk_level(ctx, 0, MK(tot - s, cnt - 1, sc), psz - s, TAIL(l), v.m, v.sf);
    }
  }
  if (j < v.ps + ms) {
    StgClosure *nn;
    StgInt kk;
    StgClosure *m2 = mid_drop(ctx, v.m, j - v.ps, &nn, &kk);
    // the children of the node the level below cut in: those after the one
    // holding the cut make the new prefix
    StgClosure **kids = node_kids(nn);
    StgInt q = 0, sb = 0;
    for (;; q++) {
      StgInt s = node_size(kids[q]);
      if (kk < sb + s) break;
      sb += s;
    }
    StgClosure *nd = kids[q];
    StgInt cn = node_arity(nn) - q - 1;
    StgInt sa = node_size(nn) - sb - node_size(nd);
    StgInt total = sa + mid_size(m2) + (n - v.ps - ms);
    *node = nd;
    *k = kk - sb;
    if (total == 0) return LF.mnil;
    if (cn == 0) return lv_deep_l(ctx, 0, total, sc, m2, v.sf);
    return mk_level(ctx, 0, MK(total, cn, sc), sa, kids_fwd(ctx, nn, q + 1), m2, v.sf);
  }
  // r leaves come after leaf j; cn nodes of the suffix, with sa leaves, are wholly after it
  StgInt r = n - 1 - j, cn = 0, sa = 0;
  for (StgClosure *l = v.sf;; l = TAIL(l)) {
    StgClosure *nd = HEAD(l);
    StgInt s = node_size(nd);
    if (r >= s) {
      r -= s, cn++, sa += s;
      continue;
    }
    *node = nd;
    *k = s - 1 - r;
    if (cn == 0) return LF.mnil;
    return mk_level(ctx, 0, MK(sa, 0, cn), 0, LF.snil, LF.mnil, s_take(ctx, cn, v.sf));
  }
}

// take i d; d itself when nothing is cut
static StgClosure *dq_take(UnisonJitCtx *ctx, StgInt i, StgClosure *d) {
  Lv v;
  if (!lv_read(d, 1, &v) || i <= 0) return LF.nil;
  StgInt n = TSIZE(v.t), pc = TPC(v.t), sc = TSC(v.t);
  if (i >= n) return d;
  if (i <= pc) return mk_level(ctx, 1, MK(i, i, 0), 0, s_take(ctx, i, v.pr), LF.mnil, LF.snil);
  if (i >= n - sc) {
    StgInt k = i - (n - sc);
    if (k == 0) return lv_deep_r(ctx, 1, i, pc, 0, v.pr, v.m);
    return mk_level(ctx, 1, MK(i, pc, k), 0, v.pr, v.m, s_drop(sc - k, v.sf));
  }
  StgClosure *nd;
  StgInt k;
  StgClosure *m = mid_take(ctx, v.m, i - pc, &nd, &k);
  return mk_level(ctx, 1, MK(i, pc, k), 0, v.pr, m, kids_rev(ctx, nd, k));
}

static StgClosure *dq_drop(UnisonJitCtx *ctx, StgInt i, StgClosure *d) {
  Lv v;
  if (!lv_read(d, 1, &v)) return LF.nil;
  if (i <= 0) return d;
  StgInt n = TSIZE(v.t), pc = TPC(v.t), sc = TSC(v.t);
  if (i >= n) return LF.nil;
  if (i >= n - sc) {
    StgInt r = n - i;
    return mk_level(ctx, 1, MK(r, 0, r), 0, LF.snil, LF.mnil, s_take(ctx, r, v.sf));
  }
  if (i <= pc) {
    if (i == pc) return lv_deep_l(ctx, 1, n - i, sc, v.m, v.sf);
    return mk_level(ctx, 1, MK(n - i, pc - i, sc), 0, s_drop(i, v.pr), v.m, v.sf);
  }
  StgClosure *nd;
  StgInt k;
  StgClosure *m = mid_drop(ctx, v.m, i - pc, &nd, &k);
  return mk_level(ctx, 1, MK(n - i, node_arity(nd) - k, sc), 0, kids_fwd(ctx, nd, k), m, v.sf);
}

// --- append ---

// c leaves as nodes (packLeaves), c >= 2: eight to a node while that leaves
// none or at least two for the next. Returns the number of nodes.
static StgInt pack_leaves(UnisonJitCtx *ctx, StgInt c, StgClosure **items, StgClosure **out) {
  StgInt n = 0;
  while (c > 0) {
    StgInt k = c <= 8 ? c : c == 9 ? 5 : 8;
    out[n++] = k == 8 ? mk_n8(ctx, items) : mk_na(ctx, k, items, k);
    items += k, c -= k;
  }
  return n;
}

// c nodes with sz leaves under them as nodes one level down (packNodes),
// c >= 2: one node of up to eight, or two or three of about the same number
// of children.
static StgInt pack_nodes(UnisonJitCtx *ctx, StgInt c, StgInt sz, StgClosure **items, StgClosure **out) {
  if (c <= 8) {
    out[0] = mk_na(ctx, sz, items, c);
    return 1;
  }
  StgInt q = (c + 7) >> 3, k1 = (c + q - 1) / q, sx = 0;
  for (StgInt i = 0; i < k1; i++) sx += node_size(items[i]);
  out[0] = mk_na(ctx, sx, items, k1);
  if (q == 2) {
    out[1] = mk_na(ctx, sz - sx, items + k1, c - k1);
    return 2;
  }
  StgInt k2 = (c - k1 + 1) / 2, sy = 0;
  for (StgInt i = 0; i < k2; i++) sy += node_size(items[k1 + i]);
  out[1] = mk_na(ctx, sy, items + k1, k2);
  out[2] = mk_na(ctx, sz - sx - sy, items + k1 + k2, c - k1 - k2);
  return 3;
}

// Two levels with nn items between them (none at the top), which hold sns
// leaves (append, appM). The digits that end up inside either join the outer
// digit of a side that has no middle, when they fit there, or are packed
// into nodes and handed down between the two middles.
static StgClosure *lv_append(UnisonJitCtx *ctx, int top, StgClosure *a, StgInt sns, StgClosure **ns, StgInt nn,
                             StgClosure *b) {
  Lv x, y;
  if (!lv_read(a, top, &x)) {
    for (StgInt i = nn - 1; i >= 0; i--) b = lv_cons(ctx, top, ns[i], b);
    return b;
  }
  if (!lv_read(b, top, &y)) {
    for (StgInt i = 0; i < nn; i++) a = lv_snoc(ctx, top, a, ns[i]);
    return a;
  }
  StgInt pc1 = TPC(x.t), sc1 = TSC(x.t), pc2 = TPC(y.t), sc2 = TSC(y.t);
  StgInt n = TSIZE(x.t) + sns + TSIZE(y.t), c = sc1 + nn + pc2;
  if (LTAG(x.m) != 2 && pc1 + c <= MAXD) {
    StgClosure *inner = s_rev_onto(ctx, x.sf, s_from(ctx, ns, nn, y.pr));
    return mk_level(ctx, top, MK(n, pc1 + c, sc2), TSIZE(x.t) + sns + y.ps, s_append(ctx, x.pr, inner), y.m, y.sf);
  }
  if (LTAG(y.m) != 2 && c + sc2 <= MAXD) {
    StgClosure *inner = s_rev_onto(ctx, y.pr, s_from_rev(ctx, ns, nn, x.sf));
    return mk_level(ctx, top, MK(n, pc1, c + sc2), x.ps, x.pr, x.m, s_append(ctx, y.sf, inner));
  }
  StgClosure *items[3 * MAXD];
  if (c >= 2 && pc1 != 0 && sc2 != 0) {
    // the left suffix (back to front), the items between, the right prefix
    StgInt at = sc1;
    for (StgClosure *l = x.sf; IS_CONS(l); l = TAIL(l)) items[--at] = HEAD(l);
    at = sc1;
    for (StgInt i = 0; i < nn; i++) items[at++] = ns[i];
    s_items(y.pr, items + at);
    StgClosure *out[3];
    StgInt sz = top ? c : (TSIZE(x.t) - x.ps - mid_size(x.m)) + sns + y.ps;
    StgInt no = top ? pack_leaves(ctx, c, items, out) : pack_nodes(ctx, c, sz, items, out);
    StgClosure *m = lv_append(ctx, 0, x.m, sz, out, no, y.m);
    return mk_level(ctx, top, MK(n, pc1, sc2), x.ps, x.pr, m, y.sf);
  }
  // what is left: a side with no middle whose outer digit is empty, or a
  // single item inside
  if (LTAG(x.m) != 2) {
    for (StgInt i = nn - 1; i >= 0; i--) b = lv_cons(ctx, top, ns[i], b);
    for (StgClosure *l = x.sf; IS_CONS(l); l = TAIL(l)) b = lv_cons(ctx, top, HEAD(l), b);
    for (StgInt i = s_items(x.pr, items) - 1; i >= 0; i--) b = lv_cons(ctx, top, items[i], b);
    return b;
  }
  for (StgInt i = 0; i < nn; i++) a = lv_snoc(ctx, top, a, ns[i]);
  for (StgClosure *l = y.pr; IS_CONS(l); l = TAIL(l)) a = lv_snoc(ctx, top, a, HEAD(l));
  for (StgInt i = s_items(y.sf, items) - 1; i >= 0; i--) a = lv_snoc(ctx, top, a, items[i]);
  return a;
}

// --- the helpers generated code calls ---

// List.size, or -1 if the closure isn't a list.
int64_t unison_jit_list_size(void *list) {
  StgClosure *dq = deque_of(list);
  if (dq == NULL) return -1;
  return LTAG(dq) == 2 ? TSIZE(LW(dq, 3)) : 0;
}

// A SeqView closure: Data2 ref tag a b, whose pointers are ref, a's, b's and
// whose words are tag, a's, b's. Here for element x (a Val: a pointer, then a
// word) and the rest of the list (a Foreign), in seven words at p.
static inline void *view_at(StgWord *p, void *empty, int64_t elem_tag, int64_t left, StgClosure *x, void *rest) {
  p[0] = LF.data2_info;
  p[1] = (StgWord)LP(empty, 0);
  p[4] = elem_tag;
  if (left) {
    p[2] = (StgWord)LP(x, 0);
    p[3] = (StgWord)rest;
    p[5] = (StgWord)LP(x, 1);
    p[6] = (StgWord)-1;
  } else {
    p[2] = (StgWord)rest;
    p[3] = (StgWord)LP(x, 0);
    p[5] = (StgWord)-1;
    p[6] = (StgWord)LP(x, 1);
  }
  return (void *)((StgWord)p | 4);
}

// The view of a list from the left (x, rest) or the right (rest, x): the
// SeqView closure the interpreter's VWLS and VWRS build. `empty` is the
// SeqViewEmpty closure (its first field is the type's Reference).
void *unison_jit_list_view(UnisonJitCtx *ctx, void *list, void *empty, int64_t elem_tag, int64_t left) {
  StgClosure *dq = deque_of(list);
  if (dq == NULL) return NULL;
  if (LTAG(dq) != 2) return empty;
  StgClosure *near = LP(dq, left ? 0 : 2);
  if (IS_CONS(near) && IS_CONS(TAIL(near))) {
    // the end's digit keeps an item: one allocation for everything
    StgWord *p = list_alloc(ctx, DEEP_WORDS + 4 + 7);
    StgInt t = LW(dq, 3) - (left ? 0x101 : 0x110);
    StgClosure *rest = TAIL(near);
    StgClosure *nd = left ? deep_at(p, t, rest, LP(dq, 1), LP(dq, 2)) : deep_at(p, t, LP(dq, 0), LP(dq, 1), rest);
    return view_at(p + DEEP_WORDS + 4, empty, elem_tag, left, HEAD(near), wrap_at(p + DEEP_WORDS, nd));
  }
  StgClosure *x;
  StgClosure *nd = left ? lv_uncons(ctx, 1, dq, &x) : lv_unsnoc(ctx, 1, dq, &x);
  StgWord *p = list_alloc(ctx, 4 + 7);
  return view_at(p + 4, empty, elem_tag, left, x, wrap_at(p, nd));
}

// cons (front) or snoc of the value (u, b).
void *unison_jit_list_push(UnisonJitCtx *ctx, void *list, int64_t u, void *b, int64_t front) {
  StgClosure *dq = deque_of(list);
  if (dq == NULL) return NULL;
  if (LTAG(dq) == 2) {
    StgInt t = LW(dq, 3);
    if ((front ? TPC(t) : TSC(t)) < MAXD) {
      // room in the end's digit: one allocation for everything
      StgWord *p = list_alloc(ctx, 3 + 3 + DEEP_WORDS + 4);
      p[0] = LF.val_info; // the element: Val u b (pointer first)
      p[1] = (StgWord)b;
      p[2] = (StgWord)u;
      StgClosure *cell = scons_at(p + 3, (StgClosure *)((StgWord)p | 1), LP(dq, front ? 0 : 2));
      StgClosure *nd = front ? deep_at(p + 6, t + 0x101, cell, LP(dq, 1), LP(dq, 2))
                             : deep_at(p + 6, t + 0x110, LP(dq, 0), LP(dq, 1), cell);
      return wrap_at(p + 6 + DEEP_WORDS, nd);
    }
  }
  StgWord *p = list_alloc(ctx, 3);
  p[0] = LF.val_info;
  p[1] = (StgWord)b;
  p[2] = (StgWord)u;
  StgClosure *x = (StgClosure *)((StgWord)p | 1);
  return wrap_list(ctx, front ? lv_cons(ctx, 1, x, dq) : lv_snoc(ctx, 1, dq, x));
}

// A list literal is built with these two: the elements are added in order
// to a deque that isn't wrapped yet (NULL for the empty one), and the result
// is wrapped. Nothing else may run in between.
void *unison_jit_list_lit(UnisonJitCtx *ctx, void *acc, int64_t u, void *b) {
  StgWord *p = list_alloc(ctx, 3);
  p[0] = LF.val_info;
  p[1] = (StgWord)b;
  p[2] = (StgWord)u;
  return lv_snoc(ctx, 1, acc == NULL ? LF.nil : (StgClosure *)acc, (StgClosure *)((StgWord)p | 1));
}

void *unison_jit_list_wrap(UnisonJitCtx *ctx, void *acc) {
  return wrap_list(ctx, acc == NULL ? LF.nil : (StgClosure *)acc);
}

// List.at: Some x or None, as the interpreter's IDXS builds them. `none` is
// the None closure (its first field is the type's Reference).
void *unison_jit_list_index(UnisonJitCtx *ctx, void *list, int64_t i, void *none, int64_t some_tag) {
  StgClosure *dq = deque_of(list);
  if (dq == NULL) return NULL;
  if (LTAG(dq) != 2) return none;
  StgInt t = LW(dq, 3), n = TSIZE(t), pc = TPC(t);
  if (i < 0 || i >= n) return none;
  StgInt j = n - 1 - i;
  StgClosure *x;
  if (i < pc)
    x = s_nth(LP(dq, 0), i);
  else if (j < TSC(t))
    x = s_nth(LP(dq, 2), j);
  else {
    StgInt off;
    StgClosure *nd = mid_look(LP(dq, 1), 0, i - pc, &off);
    x = node_kids(nd)[off];
  }
  // Data1 ref tag v: pointers ref, v's; words tag, v's
  StgWord *p = list_alloc(ctx, 5);
  p[0] = LF.data1_info;
  p[1] = (StgWord)LP(none, 0);
  p[2] = (StgWord)LP(x, 0);
  p[3] = some_tag;
  p[4] = (StgWord)LP(x, 1);
  return (void *)((StgWord)p | 3);
}

// List.take (take != 0) or List.drop of n, as TAKS and DRPS do them: a
// negative n is a Nat too large to be a size.
void *unison_jit_list_cut(UnisonJitCtx *ctx, void *list, int64_t n, int64_t take) {
  StgClosure *dq = deque_of(list);
  if (dq == NULL) return NULL;
  StgClosure *r = take ? (n < 0 ? dq : dq_take(ctx, n, dq)) : (n < 0 ? LF.nil : dq_drop(ctx, n, dq));
  return r == dq ? list : wrap_list(ctx, r);
}

// SPLL (left != 0) and SPLR: the list split after its first n elements, or
// before its last n, as a SeqView of the two parts; the empty view if the
// list is shorter than n.
void *unison_jit_list_split(UnisonJitCtx *ctx, void *list, int64_t n, void *empty, int64_t elem_tag, int64_t left) {
  StgClosure *dq = deque_of(list);
  if (dq == NULL) return NULL;
  StgInt sz = LTAG(dq) == 2 ? TSIZE(LW(dq, 3)) : 0;
  if (sz < n) return empty;
  StgInt i = left ? n : sz - n;
  StgClosure *a = dq_take(ctx, i, dq), *b = dq_drop(ctx, i, dq);
  void *fa = a == dq ? list : wrap_list(ctx, a), *fb = b == dq ? list : wrap_list(ctx, b);
  StgWord *p = list_alloc(ctx, 7);
  p[0] = LF.data2_info;
  p[1] = (StgWord)LP(empty, 0);
  p[2] = (StgWord)fa;
  p[3] = (StgWord)fb;
  p[4] = elem_tag;
  p[5] = (StgWord)-1;
  p[6] = (StgWord)-1;
  return (void *)((StgWord)p | 4);
}

// List.++
void *unison_jit_list_append(UnisonJitCtx *ctx, void *x, void *y) {
  StgClosure *a = deque_of(x), *b = deque_of(y);
  if (a == NULL || b == NULL) return NULL;
  if (LTAG(a) != 2) return y;
  if (LTAG(b) != 2) return x;
  return wrap_list(ctx, lv_append(ctx, 1, a, 0, NULL, 0, b));
}

// --- checking the layouts ---

// A sample handed over from Haskell can be a reference to a top-level value
// (the optimizer makes constants of the samples it can), which is an
// untagged pointer to an indirection. This follows it to the constructor
// and tags the pointer the way a field holding it would be. Values on the
// Unison stack don't need this: the helpers decline a pointer without its
// tag, and the interpreter handles it.
static void *settle(void *p) {
  StgClosure *c = (StgClosure *)p;
  for (;;) {
    if (GET_CLOSURE_TAG(c) != 0) return c;
    const StgInfoTable *it = get_itbl(c);
    if (it->type == IND || it->type == IND_STATIC) {
      c = ((StgInd *)c)->indirectee;
      continue;
    }
    if (it->type >= CONSTR && it->type <= CONSTR_NOCAF) {
      StgWord tag = it->srt + 1; // the constructor's number
      return (StgClosure *)((StgWord)c | (tag > 7 ? 7 : tag));
    }
    return c;
  }
}

static int con_is(StgClosure *c, StgWord tag, StgWord ptrs, StgWord nptrs) {
  if (LTAG(c) != tag) return 0;
  const StgInfoTable *it = get_itbl(LUN(c));
  return it->type >= CONSTR && it->type <= CONSTR_NOCAF && it->layout.payload.ptrs == ptrs &&
         it->layout.payload.nptrs == nptrs;
}

// a first constructor that has no fields (a static closure)
static int con_nullary(StgClosure *c) {
  if (LTAG(c) != 1) return 0;
  const StgInfoTable *it = get_itbl(LUN(c));
  return it->type >= CONSTR && it->type <= CONSTR_NOCAF && it->layout.payload.ptrs == 0;
}

// Whether c is the same constructor without fields as ref. Not a comparison
// of addresses: a constructor without fields has a static closure of its own,
// but so does every top-level binding that is just that constructor (empty =
// Nil), and which of them a piece of code refers to is up to the compiler.
static inline int same_con0(StgClosure *c, StgClosure *ref) {
  return LTAG(c) == LTAG(ref) && LUN(c)->header.info == LUN(ref)->header.info;
}

// the length of a digit's list, or -1 if it isn't one
static StgInt check_slist(StgClosure *l) {
  StgInt len = 0;
  for (; LTAG(l) == 2; l = LP(l, 1), len++)
    if ((StgWord)LUN(l)->header.info != LF.scons_info || len > MAXD) return -1;
  return same_con0(l, LF.snil) ? len : -1;
}

static int is_small_array(StgClosure *a) {
  const StgInfoTable *info = a->header.info;
  return info == &stg_SMALL_MUT_ARR_PTRS_FROZEN_CLEAN_info || info == &stg_SMALL_MUT_ARR_PTRS_FROZEN_DIRTY_info;
}

// the leaves under a node `depth` levels down (1: its children are elements), or -1
static StgInt check_node(StgClosure *nd, int depth, int64_t *kinds) {
  if (LTAG(nd) == 1) {
    if (depth != 1 || (StgWord)LUN(nd)->header.info != LF.n8_info) return -1;
    *kinds |= 2;
    return 8;
  }
  if (LTAG(nd) != 2 || (StgWord)LUN(nd)->header.info != LF.na_info || !is_small_array(LP(nd, 0))) return -1;
  *kinds |= 4;
  StgInt arity = node_arity(nd), total = 0;
  if (arity < 2 || arity > 8) return -1;
  for (StgInt c = 0; c < arity; c++) {
    StgInt z = depth == 1 ? 1 : check_node(node_kids(nd)[c], depth - 1, kinds);
    if (z < 0) return -1;
    total += z;
  }
  return total == LW(nd, 1) ? total : -1;
}

// a digit of a level `depth` down with the given count; its leaves, or -1
static StgInt check_digit(StgClosure *l, int depth, StgInt count, int64_t *kinds) {
  if (check_slist(l) != count) return -1;
  StgInt total = 0;
  for (; LTAG(l) == 2; l = LP(l, 1)) {
    StgInt z = depth == 0 ? (LTAG(HEAD(l)) == 1 ? 1 : -1) : check_node(HEAD(l), depth, kinds);
    if (z < 0) return -1;
    total += z;
  }
  return total;
}

// Walks a whole list checking that every constructor has the shape, the
// counts and the invariants this file assumes. Returns -1 if not, else a
// set of bits saying which constructors it met: 1 for a Deep, 2 and 4 for
// the two kinds of node, 8 for an MDeep.
int64_t unison_jit_list_check(void **elems) {
  StgClosure *c = deque_of(settle(elems[0]));
  if (c == NULL) return -1;
  if (LTAG(c) == 1) return same_con0(c, LF.nil) ? 0 : -1;
  int64_t kinds = 1;
  StgInt sizes[64], befores[64];
  int depth = 0;
  // down: each level's digits; then up: each level's size against its parts
  StgInt inner = 0;
  for (;; depth++) {
    if (depth >= 64) return -1;
    if (depth == 0 ? (StgWord)LUN(c)->header.info != LF.deep_info || !con_is(c, 2, 3, 1)
                   : (StgWord)LUN(c)->header.info != LF.mdeep_info || !con_is(c, 2, 3, 2))
      return -1;
    if (depth > 0) kinds |= 8;
    StgInt t = LW(c, 3);
    StgInt a = check_digit(LP(c, 0), depth, TPC(t), &kinds), b = check_digit(LP(c, 2), depth, TSC(t), &kinds);
    if (a < 0 || b < 0 || TSIZE(t) <= 0) return -1;
    if (depth > 0 && LW(c, 4) != a) return -1;
    sizes[depth] = TSIZE(t), befores[depth] = a + b;
    StgClosure *m = LP(c, 1);
    if (LTAG(m) == 1) {
      if (!same_con0(m, LF.mnil)) return -1;
      break;
    }
    // a level with a middle has an item in both digits
    if (TPC(t) == 0 || TSC(t) == 0) return -1;
    c = m;
  }
  for (; depth >= 0; depth--) {
    if (sizes[depth] != befores[depth] + inner) return -1;
    inner = sizes[depth];
  }
  return kinds;
}

// Learns the constructors from samples. elems[0] is a list of four values
// made as  x <| (empty |> a |> b |> c),  so it has a prefix holding x, a
// suffix of three and no middle; elems[1] is x (a Val); elems[2] is the empty
// list; elems[3] is a list of a thousand values added one at a time at the
// back, which has both kinds of node. info[] holds the info pointers of
// Foreign, Val, Data1 and Data2. Returns 1 if everything is as this file
// assumes, else a number saying which check failed.
int64_t unison_jit_list_init(void **raw, int64_t *info) {
  void *elems[4] = {settle(raw[0]), settle(raw[1]), settle(raw[2]), settle(raw[3])};
  memset(&LF, 0, sizeof LF);
  LF.foreign_info = info[0];
  LF.val_info = info[1];
  LF.data1_info = info[2];
  LF.data2_info = info[3];
  StgClosure *f = elems[0];
  if (LTAG(f) != 7 || (StgWord)LUN(f)->header.info != LF.foreign_info) return 2;
  StgClosure *w = LP(f, 0);
  const StgInfoTable *wit = get_itbl(LUN(w));
  if (!(wit->type >= CONSTR && wit->type <= CONSTR_NOCAF) || wit->layout.payload.ptrs != 1 ||
      wit->layout.payload.nptrs != 0)
    return 3;
  LF.wrapseq_info = (StgWord)LUN(w)->header.info;
  LF.wrapseq_tag = LTAG(w);
  StgClosure *dq = LP(w, 0);
  if (!con_is(dq, 2, 3, 1)) return 4;
  LF.deep_info = (StgWord)LUN(dq)->header.info;
  if (LW(dq, 3) != MK(4, 1, 3)) return 5;
  StgClosure *pr = LP(dq, 0), *m = LP(dq, 1), *sf = LP(dq, 2);
  if (!con_is(pr, 2, 2, 0)) return 6;
  if (LUN(LP(pr, 0)) != LUN(elems[1])) return 10;
  if (!con_nullary(LP(pr, 1))) return 11;
  LF.scons_info = (StgWord)LUN(pr)->header.info;
  LF.snil = LP(pr, 1);
  if (check_slist(sf) != 3 || !con_nullary(m)) return 7;
  LF.mnil = m;
  StgClosure *e = deque_of(elems[2]);
  if (e == NULL || !con_nullary(e) || LUN(e) == LUN(m)) return 8;
  LF.nil = e;
  // a Val is a pointer and a word, tag 1
  if (!con_is(elems[1], 1, 1, 1) || (StgWord)LUN(elems[1])->header.info != LF.val_info) return 9;
  // the big sample: a middle whose digits hold nodes of eight leaves, and
  // under it a middle whose digits hold nodes with arrays
  StgClosure *big = deque_of(elems[3]);
  if (big == NULL || !con_is(big, 2, 3, 1) || TSIZE(LW(big, 3)) != 1000) return 12;
  StgClosure *m1 = LP(big, 1);
  if (!con_is(m1, 2, 3, 2)) return 13;
  LF.mdeep_info = (StgWord)LUN(m1)->header.info;
  if (!con_is(LP(m1, 0), 2, 2, 0)) return 14;
  StgClosure *n8 = HEAD(LP(m1, 0));
  if (!con_is(n8, 1, 8, 0)) return 15;
  LF.n8_info = (StgWord)LUN(n8)->header.info;
  StgClosure *m2 = LP(m1, 1);
  if (!con_is(m2, 2, 3, 2) || (StgWord)LUN(m2)->header.info != LF.mdeep_info || !con_is(LP(m2, 0), 2, 2, 0))
    return 16;
  StgClosure *na = HEAD(LP(m2, 0));
  if (!con_is(na, 2, 1, 1) || !is_small_array(LP(na, 0))) return 17;
  LF.na_info = (StgWord)LUN(na)->header.info;
  return 1;
}

// For the startup self-test: runs one helper on elems[0] with the other
// arguments in elems[1] (a closure), arg and arg2, and puts the result in
// elems[3]. Returns 0 if the helper didn't handle the case.
//   op 0: view left, 1: view right (elems[1] the empty view, arg the tag)
//   2: cons, 3: snoc (elems[1] the boxed half of the value, arg the unboxed half)
//   4: index arg (elems[1] None, arg2 the tag)
//   5: take arg, 6: drop arg
//   7: split left at arg, 8: split right (elems[1] the empty view, arg2 the tag)
//   9: append elems[0] and elems[1]
//   10: the literal of arg copies of the value (arg2, elems[1])
// (The context is a temporary one: the thread's own is made on its first real
// entry, after the settings are in place.)
int64_t unison_jit_list_test(void **elems, int64_t op, int64_t arg, int64_t arg2) {
  UnisonJitCtx tmp = {0}, *ctx = &tmp;
  ctx->cap = rts_unsafeGetMyCapability();
  void *res = NULL;
  void *e0 = settle(elems[0]), *e1 = settle(elems[1]);
  switch (op) {
    case 0:
    case 1:
      res = unison_jit_list_view(ctx, e0, e1, arg, op == 0);
      break;
    case 2:
    case 3:
      res = unison_jit_list_push(ctx, e0, arg, e1, op == 2);
      break;
    case 4:
      res = unison_jit_list_index(ctx, e0, arg, e1, arg2);
      break;
    case 5:
    case 6:
      res = unison_jit_list_cut(ctx, e0, arg, op == 5);
      break;
    case 7:
    case 8:
      res = unison_jit_list_split(ctx, e0, arg, e1, arg2, op == 7);
      break;
    case 9:
      res = unison_jit_list_append(ctx, e0, e1);
      break;
    case 10: {
      void *acc = NULL;
      for (int64_t i = 0; i < arg; i++) acc = unison_jit_list_lit(ctx, acc, arg2 + i, e1);
      res = unison_jit_list_wrap(ctx, acc);
      break;
    }
  }
  if (res == NULL) return 0;
  elems[3] = res;
  unison_jit_mark_bstk(elems, 3, 3);
  return 1;
}

// ---------------------------------------------------------------------------
// Text
//
// A Unison Text is a Unison.Util.Rope of chunks, held as Foreign (WrapText
// rope). The rope is the list's finger tree with chunks for elements and
// sizes counted in characters: its middle is the Deque's own Mid, so the
// functions above that take top = 0 work on it as they are, and only the top
// level, whose items are chunks, is written here. These helpers are ports of
// the Haskell operations in lib/unison-util-rope's Rope.hs and the Chunk
// instances in Unison.Util.Text, and take every case.
//
//   Rope: Empty (tag 1), One chunk (tag 2),
//         Deep t ps pr m sf (tag 3): pointers pr, m, sf; words t, ps
//   Chunk count (Text array offset length), unpacked: the pointer to the
//         byte array, then the character count, byte offset and byte length
//
// t and ps are as in an MDeep, with characters for leaves. The rope's
// invariants: no chunk is empty; a rope of one chunk is One; both digits of a
// Deep have a chunk; two chunks next to each other have more than
// ROPE_THRESHOLD characters between them.
//
// Checked at startup like the list layouts (unison_jit_text_init).

typedef struct {
  StgWord foreign_info, wraptext_info, wraptext_tag;
  StgWord one_info, deep_info, chunk_info;
  StgClosure *empty; // Rope's Empty (tagged)
  StgInt threshold;
} TextFacts;
static TextFacts TF;

enum { CH_ARR, CH_COUNT, CH_OFF, CH_LEN };
// the most characters two chunks may have between them to be made one
// (Rope.threshold, handed to unison_jit_text_init)
#define ROPE_THRESHOLD (TF.threshold)

static inline StgClosure *rope_of(void *text) {
  if (LTAG(text) != 7 || (StgWord)LUN(text)->header.info != TF.foreign_info) return NULL;
  StgClosure *w = LP(text, 0);
  if (LTAG(w) != TF.wraptext_tag || (StgWord)LUN(w)->header.info != TF.wraptext_info) return NULL;
  return LP(w, 0);
}

static inline StgInt chunk_size(StgClosure *c) { return LW(c, CH_COUNT); }

// the fields of a Deep (which lv_read doesn't take: its tag isn't a list level's)
static inline void rope_read(StgClosure *c, Lv *v) {
  v->pr = LP(c, 0), v->m = LP(c, 1), v->sf = LP(c, 2);
  v->t = LW(c, 3), v->ps = LW(c, 4);
}

static inline StgInt rope_size(StgClosure *r) {
  switch (LTAG(r)) {
    case 2: return chunk_size(LP(r, 0));
    case 3: return TSIZE(LW(r, 3));
    default: return 0;
  }
}

static inline const unsigned char *chunk_bytes(StgClosure *c) {
  return (const unsigned char *)((StgArrBytes *)LP(c, CH_ARR))->payload + LW(c, CH_OFF);
}

// the characters under a list of chunks
static inline StgInt s_sum_chunks(StgClosure *l) {
  StgInt s = 0;
  for (; IS_CONS(l); l = TAIL(l)) s += chunk_size(HEAD(l));
  return s;
}

// --- building ---

static inline StgClosure *new_chunk(UnisonJitCtx *ctx, void *arr, StgInt count, StgInt off, StgInt len) {
  StgWord *p = list_alloc(ctx, 5);
  p[0] = TF.chunk_info;
  p[1 + CH_ARR] = (StgWord)arr;
  p[1 + CH_COUNT] = count;
  p[1 + CH_OFF] = off;
  p[1 + CH_LEN] = len;
  return (StgClosure *)((StgWord)p | 1);
}

static inline StgClosure *rope_one(UnisonJitCtx *ctx, StgClosure *chunk) {
  StgWord *p = list_alloc(ctx, 2);
  p[0] = TF.one_info;
  p[1] = (StgWord)chunk;
  return (StgClosure *)((StgWord)p | 2);
}

static inline StgClosure *rope_deep(UnisonJitCtx *ctx, StgInt t, StgInt ps, StgClosure *pr, StgClosure *m,
                                    StgClosure *sf) {
  StgWord *p = list_alloc(ctx, 6);
  p[0] = TF.deep_info;
  p[1] = (StgWord)pr;
  p[2] = (StgWord)m;
  p[3] = (StgWord)sf;
  p[4] = (StgWord)t;
  p[5] = (StgWord)ps;
  return (StgClosure *)((StgWord)p | 3);
}

// the two chunks' characters as one chunk, in a fresh byte array
static StgClosure *chunk_join(UnisonJitCtx *ctx, StgClosure *a, StgClosure *b) {
  StgInt la = LW(a, CH_LEN), lb = LW(b, CH_LEN);
  StgWord bytes = la + lb;
  StgArrBytes *arr = (StgArrBytes *)list_alloc(ctx, sizeofW(StgArrBytes) + ROUNDUP_BYTES_TO_WDS(bytes));
  SET_INFO((StgClosure *)arr, &stg_ARR_WORDS_info);
  arr->bytes = bytes;
  memcpy(arr->payload, chunk_bytes(a), la);
  memcpy((char *)arr->payload + la, chunk_bytes(b), lb);
  return new_chunk(ctx, arr, chunk_size(a) + chunk_size(b), 0, bytes);
}

// the number of bytes the first k characters of UTF-8 text take
static inline StgInt utf8_prefix(const unsigned char *p, StgInt k) {
  const unsigned char *q = p;
  while (k-- > 0) q += *q < 0x80 ? 1 : *q < 0xE0 ? 2 : *q < 0xF0 ? 3 : 4;
  return q - p;
}

// the first k characters of a chunk, 0 < k < its size; and the rest
static inline StgClosure *chunk_take(UnisonJitCtx *ctx, StgClosure *c, StgInt k) {
  return new_chunk(ctx, LP(c, CH_ARR), k, LW(c, CH_OFF), utf8_prefix(chunk_bytes(c), k));
}

static inline StgClosure *chunk_drop(UnisonJitCtx *ctx, StgClosure *c, StgInt k) {
  StgInt nb = utf8_prefix(chunk_bytes(c), k);
  return new_chunk(ctx, LP(c, CH_ARR), chunk_size(c) - k, LW(c, CH_OFF) + nb, LW(c, CH_LEN) - nb);
}

// cnt chunks in order, at most a digit's worth, holding n characters (fromFwd)
static StgClosure *rope_from_fwd(UnisonJitCtx *ctx, StgInt n, StgInt cnt, StgClosure *l) {
  if (cnt == 0) return TF.empty;
  if (cnt == 1) return rope_one(ctx, HEAD(l));
  StgInt p = (cnt + 1) / 2;
  StgClosure *pr = s_take(ctx, p, l);
  return rope_deep(ctx, MK(n, p, cnt - p), s_sum_chunks(pr), pr, LF.mnil, s_rev_onto(ctx, s_drop(p, l), LF.snil));
}

// ... back to front (fromBwd)
static StgClosure *rope_from_bwd(UnisonJitCtx *ctx, StgInt n, StgInt cnt, StgClosure *l) {
  if (cnt == 0) return TF.empty;
  if (cnt == 1) return rope_one(ctx, HEAD(l));
  StgInt q = cnt / 2;
  StgClosure *pr = s_rev_onto(ctx, s_drop(q, l), LF.snil);
  return rope_deep(ctx, MK(n, cnt - q, q), s_sum_chunks(pr), pr, LF.mnil, s_take(ctx, q, l));
}

// A rope of n characters from the parts of a Deep, either of whose digits may
// be empty (build): an empty digit takes a node from the middle, or half of
// the other digit when there is no middle.
static StgClosure *rope_build(UnisonJitCtx *ctx, StgInt n, StgInt pc, StgInt ps, StgClosure *pr, StgClosure *m,
                              StgInt sc, StgClosure *sf) {
  if (pc == 0) {
    if (LTAG(m) != 2) return rope_from_bwd(ctx, n, sc, sf);
    StgClosure *nd;
    m = lv_uncons(ctx, 0, m, &nd);
    pc = node_arity(nd), ps = node_size(nd), pr = kids_fwd(ctx, nd, 0);
  }
  if (sc == 0) {
    if (LTAG(m) != 2) return rope_from_fwd(ctx, n, pc, pr);
    StgClosure *nd;
    m = lv_unsnoc(ctx, 0, m, &nd);
    sc = node_arity(nd), sf = kids_rev(ctx, nd, sc);
  }
  return rope_deep(ctx, MK(n, pc, sc), ps, pr, m, sf);
}

// --- adding a chunk at an end (cons', snoc') ---

// a chunk c of s characters in front of a rope
static StgClosure *rope_cons(UnisonJitCtx *ctx, StgInt s, StgClosure *c, StgClosure *r) {
  switch (LTAG(r)) {
    case 1: return rope_one(ctx, c);
    case 2: {
      StgClosure *a = LP(r, 0);
      StgInt sa = chunk_size(a);
      if (s + sa <= ROPE_THRESHOLD) return rope_one(ctx, chunk_join(ctx, c, a));
      return rope_deep(ctx, MK(s + sa, 1, 1), s, scons(ctx, c, LF.snil), LF.mnil, scons(ctx, a, LF.snil));
    }
  }
  Lv v;
  rope_read(r, &v);
  StgClosure *f = HEAD(v.pr);
  if (s + chunk_size(f) <= ROPE_THRESHOLD)
    return rope_deep(ctx, v.t + (s << 8), v.ps + s, scons(ctx, chunk_join(ctx, c, f), TAIL(v.pr)), v.m, v.sf);
  if (TPC(v.t) < MAXD) return rope_deep(ctx, v.t + (s << 8) + 1, v.ps + s, scons(ctx, c, v.pr), v.m, v.sf);
  // a full prefix keeps its two outermost chunks and sheds the other eight
  StgClosure *k[MAXD];
  s_items(v.pr, k);
  StgInt keep = chunk_size(k[0]) + chunk_size(k[1]);
  StgClosure *m = lv_cons(ctx, 0, mk_na(ctx, v.ps - keep, k + 2, 8), v.m);
  return rope_deep(ctx, v.t + (s << 8) - 7, s + keep, scons(ctx, c, s_from(ctx, k, 2, LF.snil)), m, v.sf);
}

// ... or behind it
static StgClosure *rope_snoc(UnisonJitCtx *ctx, StgClosure *r, StgInt s, StgClosure *c) {
  switch (LTAG(r)) {
    case 1: return rope_one(ctx, c);
    case 2: {
      StgClosure *a = LP(r, 0);
      StgInt sa = chunk_size(a);
      if (sa + s <= ROPE_THRESHOLD) return rope_one(ctx, chunk_join(ctx, a, c));
      return rope_deep(ctx, MK(sa + s, 1, 1), sa, scons(ctx, a, LF.snil), LF.mnil, scons(ctx, c, LF.snil));
    }
  }
  Lv v;
  rope_read(r, &v);
  StgClosure *l = HEAD(v.sf);
  if (chunk_size(l) + s <= ROPE_THRESHOLD)
    return rope_deep(ctx, v.t + (s << 8), v.ps, v.pr, v.m, scons(ctx, chunk_join(ctx, l, c), TAIL(v.sf)));
  if (TSC(v.t) < MAXD) return rope_deep(ctx, v.t + (s << 8) + 0x10, v.ps, v.pr, v.m, scons(ctx, c, v.sf));
  // the suffix runs back to front: s1, s2, then the eight to shed, last first
  StgClosure *k[MAXD], *shed[8];
  s_items(v.sf, k);
  for (int i = 0; i < 8; i++) shed[i] = k[9 - i];
  StgInt sz = TSIZE(v.t) - v.ps - mid_size(v.m) - chunk_size(k[0]) - chunk_size(k[1]);
  StgClosure *m = lv_snoc(ctx, 0, v.m, mk_na(ctx, sz, shed, 8));
  return rope_deep(ctx, v.t + (s << 8) - 0x70, v.ps, v.pr, m, scons(ctx, c, s_from(ctx, k, 2, LF.snil)));
}

// --- take and drop (takeR, dropR) ---
//
// The rope is cut between two chunks, by the list's code when the cut is in
// the middle, and then the part of the chunk the cut falls in is added back
// with snoc or cons, which joins it to its neighbour if the two are small.

static StgClosure *rope_take(UnisonJitCtx *ctx, StgInt i, StgClosure *r) {
  switch (LTAG(r)) {
    case 1: return r;
    case 2: {
      StgClosure *c = LP(r, 0);
      if (i <= 0) return TF.empty;
      if (i >= chunk_size(c)) return r;
      return rope_one(ctx, chunk_take(ctx, c, i));
    }
  }
  Lv v;
  rope_read(r, &v);
  StgInt n = TSIZE(v.t), pc = TPC(v.t), sc = TSC(v.t), ms = mid_size(v.m);
  if (i <= 0) return TF.empty;
  if (i >= n) return r;
  if (i <= v.ps) {
    StgInt q = 0, sb = 0; // q whole chunks of the prefix, with sb characters, come before the cut
    for (StgClosure *l = v.pr;; l = TAIL(l), q++) {
      StgClosure *c = HEAD(l);
      StgInt s = chunk_size(c);
      if (sb + s < i) {
        sb += s;
        continue;
      }
      if (sb + s == i) return rope_from_fwd(ctx, i, q + 1, s_take(ctx, q + 1, v.pr));
      StgClosure *whole = rope_from_fwd(ctx, sb, q, s_take(ctx, q, v.pr));
      return rope_snoc(ctx, whole, i - sb, chunk_take(ctx, c, i - sb));
    }
  }
  if (i <= v.ps + ms) {
    StgClosure *nd;
    StgInt k;
    StgClosure *m2 = mid_take(ctx, v.m, i - v.ps, &nd, &k);
    // the first k characters of the node are kept
    StgClosure **kids = node_kids(nd);
    StgInt q = 0, sb = 0;
    for (;; q++) {
      StgClosure *c = kids[q];
      StgInt s = chunk_size(c);
      if (sb + s < k) {
        sb += s;
        continue;
      }
      if (sb + s == k) return rope_build(ctx, i, pc, v.ps, v.pr, m2, q + 1, kids_rev(ctx, nd, q + 1));
      StgClosure *whole = rope_build(ctx, i - (k - sb), pc, v.ps, v.pr, m2, q, kids_rev(ctx, nd, q));
      return rope_snoc(ctx, whole, k - sb, chunk_take(ctx, c, k - sb));
    }
  }
  StgInt d = n - i, cnt = sc; // d characters are to go from the back; cnt chunks of the suffix are left
  for (StgClosure *l = v.sf;; l = TAIL(l), cnt--) {
    StgClosure *c = HEAD(l);
    StgInt s = chunk_size(c);
    if (d >= s) {
      d -= s;
      continue;
    }
    if (d == 0) return rope_build(ctx, i, pc, v.ps, v.pr, v.m, cnt, l);
    StgClosure *whole = rope_build(ctx, i - (s - d), pc, v.ps, v.pr, v.m, cnt - 1, TAIL(l));
    return rope_snoc(ctx, whole, s - d, chunk_take(ctx, c, s - d));
  }
}

static StgClosure *rope_drop(UnisonJitCtx *ctx, StgInt i, StgClosure *r) {
  switch (LTAG(r)) {
    case 1: return r;
    case 2: {
      StgClosure *c = LP(r, 0);
      if (i <= 0) return r;
      if (i >= chunk_size(c)) return TF.empty;
      return rope_one(ctx, chunk_drop(ctx, c, i));
    }
  }
  Lv v;
  rope_read(r, &v);
  StgInt n = TSIZE(v.t), pc = TPC(v.t), sc = TSC(v.t), ms = mid_size(v.m);
  if (i <= 0) return r;
  if (i >= n) return TF.empty;
  StgInt left = n - i;
  if (i >= v.ps + ms) {
    StgInt q = 0, sa = 0; // q whole chunks of the suffix, with sa characters, come after the cut
    for (StgClosure *l = v.sf;; l = TAIL(l), q++) {
      StgClosure *c = HEAD(l);
      StgInt s = chunk_size(c);
      if (sa + s < left) {
        sa += s;
        continue;
      }
      if (sa + s == left) return rope_from_bwd(ctx, left, q + 1, s_take(ctx, q + 1, v.sf));
      StgClosure *whole = rope_from_bwd(ctx, sa, q, s_take(ctx, q, v.sf));
      return rope_cons(ctx, left - sa, chunk_drop(ctx, c, s - (left - sa)), whole);
    }
  }
  if (i >= v.ps) {
    StgClosure *nd;
    StgInt k;
    StgClosure *m2 = mid_drop(ctx, v.m, i - v.ps, &nd, &k);
    // the first k characters of the node go
    StgClosure **kids = node_kids(nd);
    StgInt arity = node_arity(nd), q = 0, sb = 0;
    for (;; q++) {
      StgClosure *c = kids[q];
      StgInt s = chunk_size(c);
      if (k >= sb + s) {
        sb += s;
        continue;
      }
      if (k == sb) return rope_build(ctx, left, arity - q, node_size(nd) - sb, kids_fwd(ctx, nd, q), m2, sc, v.sf);
      StgClosure *whole = rope_build(ctx, left - (sb + s - k), arity - q - 1, node_size(nd) - sb - s,
                                     kids_fwd(ctx, nd, q + 1), m2, sc, v.sf);
      return rope_cons(ctx, sb + s - k, chunk_drop(ctx, c, k - sb), whole);
    }
  }
  // k characters still to drop; the prefix has cnt chunks and psz characters left
  StgInt k = i, cnt = pc, psz = v.ps;
  for (StgClosure *l = v.pr;; l = TAIL(l)) {
    StgClosure *c = HEAD(l);
    StgInt s = chunk_size(c);
    if (k >= s) {
      k -= s, cnt--, psz -= s;
      continue;
    }
    if (k == 0) return rope_build(ctx, left, cnt, psz, l, v.m, sc, v.sf);
    StgClosure *whole = rope_build(ctx, left - (s - k), cnt - 1, psz - s, TAIL(l), v.m, sc, v.sf);
    return rope_cons(ctx, s - k, chunk_drop(ctx, c, k), whole);
  }
}

// --- append ---

// If the two chunks that meet are small they are joined, as the last chunk of
// the left side. Then the digits that end up inside join the outer digit of a
// side that has no middle, if they fit there (the shorter side's, if they fit
// in either: that copies fewer cells), or are packed into nodes and handed to
// the list's append of two middles.
static StgClosure *rope_append(UnisonJitCtx *ctx, StgClosure *a, StgClosure *b) {
  for (;;) {
    if (LTAG(a) == 1) return b;
    if (LTAG(b) == 1) return a;
    if (LTAG(a) == 2) return rope_cons(ctx, rope_size(a), LP(a, 0), b);
    if (LTAG(b) == 2) return rope_snoc(ctx, a, rope_size(b), LP(b, 0));
    Lv x, y;
    rope_read(a, &x);
    rope_read(b, &y);
    StgInt pc1 = TPC(x.t), sc1 = TSC(x.t), pc2 = TPC(y.t), sc2 = TSC(y.t);
    StgInt n = TSIZE(x.t) + TSIZE(y.t);
    // isf: the left side's suffix; ipr: the ipc chunks of the right side's
    // prefix that are still its own (d characters of it went to the left)
    StgClosure *isf = x.sf, *ipr = y.pr;
    StgInt ipc = pc2, d = 0;
    StgClosure *l = HEAD(x.sf), *f = HEAD(y.pr);
    if (chunk_size(l) + chunk_size(f) <= ROPE_THRESHOLD) {
      d = chunk_size(f);
      isf = scons(ctx, chunk_join(ctx, l, f), TAIL(x.sf));
      ipr = TAIL(y.pr), ipc = pc2 - 1;
    }
    StgInt c = sc1 + ipc;
    int flat1 = LTAG(x.m) != 2, flat2 = LTAG(y.m) != 2;
    if (flat1 && pc1 + c <= MAXD && !(flat2 && c + sc2 <= MAXD && sc2 + ipc < pc1 + sc1))
      return rope_deep(ctx, MK(n, pc1 + c, sc2), TSIZE(x.t) + y.ps, s_append(ctx, x.pr, s_rev_onto(ctx, isf, ipr)),
                       y.m, y.sf);
    if (flat2 && c + sc2 <= MAXD)
      return rope_deep(ctx, MK(n, pc1, c + sc2), x.ps, x.pr, x.m, s_append(ctx, y.sf, s_rev_onto(ctx, ipr, isf)));
    if (c < 2) {
      // a single chunk between two sides that can't take it: the right side
      // gets a prefix again, from its middle or its suffix
      a = rope_deep(ctx, x.t + (d << 8), x.ps, x.pr, x.m, isf);
      b = rope_build(ctx, TSIZE(y.t) - d, 0, 0, LF.snil, y.m, sc2, y.sf);
      continue;
    }
    // the left suffix (back to front) and the right prefix, as nodes of eight
    // while that leaves none or at least two for the next (packChunks)
    StgClosure *items[2 * MAXD], *out[3];
    StgInt at = sc1, no = 0;
    for (StgClosure *p = isf; IS_CONS(p); p = TAIL(p)) items[--at] = HEAD(p);
    s_items(ipr, items + sc1);
    for (StgInt done = 0; done < c;) {
      StgInt left = c - done, k = left <= 8 ? left : left == 9 ? 5 : 8, sz = 0;
      for (StgInt j = 0; j < k; j++) sz += chunk_size(items[done + j]);
      out[no++] = mk_na(ctx, sz, items + done, k);
      done += k;
    }
    StgInt sns = (TSIZE(x.t) - x.ps - mid_size(x.m)) + y.ps;
    StgClosure *m = lv_append(ctx, 0, x.m, sns, out, no, y.m);
    return rope_deep(ctx, MK(n, pc1, sc2), x.ps, x.pr, m, y.sf);
  }
}

// --- finding the chunk that holds a character (chunkAt) ---

// The chunk holding character i, 0 <= i < size, and i's offset in it.
static StgClosure *rope_chunk_at(StgClosure *r, StgInt i, StgInt *off) {
  if (LTAG(r) == 2) {
    *off = i;
    return LP(r, 0);
  }
  Lv v;
  rope_read(r, &v);
  if (i < v.ps) {
    for (StgClosure *l = v.pr;; l = TAIL(l)) {
      StgInt s = chunk_size(HEAD(l));
      if (i < s) {
        *off = i;
        return HEAD(l);
      }
      i -= s;
    }
  }
  StgInt i2 = i - v.ps;
  if (i2 < mid_size(v.m)) {
    StgInt o;
    StgClosure *nd = mid_look(v.m, -1, i2, &o);
    for (StgClosure **kids = node_kids(nd);; kids++) {
      StgInt s = chunk_size(*kids);
      if (o < s) {
        *off = o;
        return *kids;
      }
      o -= s;
    }
  }
  StgInt back = TSIZE(v.t) - 1 - i; // characters after the one looked for
  for (StgClosure *l = v.sf;; l = TAIL(l)) {
    StgInt s = chunk_size(HEAD(l));
    if (back < s) {
      *off = s - 1 - back;
      return HEAD(l);
    }
    back -= s;
  }
}

// Foreign (WrapText rope); `same` is returned if it already holds that rope
static void *wrap_text(UnisonJitCtx *ctx, StgClosure *rope, void *same) {
  if (same != NULL && rope_of(same) == rope) return same;
  StgWord *p = list_alloc(ctx, 4);
  p[0] = TF.wraptext_info;
  p[1] = (StgWord)rope;
  p[2] = TF.foreign_info;
  p[3] = (StgWord)p | TF.wraptext_tag;
  return (void *)((StgWord)(p + 2) | 7);
}

// --- the helpers generated code calls ---

// Text.size, or -1 if the closure isn't a text.
int64_t unison_jit_text_size(void *text) {
  StgClosure *r = rope_of(text);
  return r == NULL ? -1 : rope_size(r);
}

// Text.++, or NULL if a closure isn't a text.
void *unison_jit_text_append(UnisonJitCtx *ctx, void *x, void *y) {
  StgClosure *a = rope_of(x), *b = rope_of(y);
  if (a == NULL || b == NULL) return NULL;
  if (LTAG(a) == 1) return y;
  if (LTAG(b) == 1) return x;
  return wrap_text(ctx, rope_append(ctx, a, b), NULL);
}

// Text.take (take != 0) or Text.drop of n characters. n comes from a Nat: a
// negative one is a count too large to be a size, and takes everything or
// drops everything, as in the interpreter.
void *unison_jit_text_cut(UnisonJitCtx *ctx, void *text, int64_t n, int64_t take) {
  StgClosure *r = rope_of(text);
  if (r == NULL) return NULL;
  if (take) return n < 0 ? text : wrap_text(ctx, rope_take(ctx, n, r), text);
  return wrap_text(ctx, n < 0 ? TF.empty : rope_drop(ctx, n, r), text);
}

// Text equality: 1 or 0, or -1 if a closure isn't a text. Texts are equal
// when they have the same characters, however they are cut into chunks; UTF-8
// makes that the same bytes. Each side is walked a chunk at a time, finding
// the next chunk by its position.
int64_t unison_jit_text_eq(void *x, void *y) {
  StgClosure *a = rope_of(x), *b = rope_of(y);
  if (a == NULL || b == NULL) return -1;
  if (a == b) return 1;
  StgInt n = rope_size(a);
  if (n != rope_size(b)) return 0;
  if (LTAG(a) == 2 && LTAG(b) == 2) {
    StgClosure *ca = LP(a, 0), *cb = LP(b, 0);
    return LW(ca, CH_LEN) == LW(cb, CH_LEN) && memcmp(chunk_bytes(ca), chunk_bytes(cb), LW(ca, CH_LEN)) == 0;
  }
  const unsigned char *pa = NULL, *pb = NULL;
  StgInt na = 0, nb = 0, ia = 0, ib = 0, off; // bytes left in each side's chunk; characters before the next one
  for (;;) {
    if (na == 0 && ia < n) {
      StgClosure *c = rope_chunk_at(a, ia, &off);
      pa = chunk_bytes(c), na = LW(c, CH_LEN), ia += chunk_size(c);
    }
    if (nb == 0 && ib < n) {
      StgClosure *c = rope_chunk_at(b, ib, &off);
      pb = chunk_bytes(c), nb = LW(c, CH_LEN), ib += chunk_size(c);
    }
    if (na == 0 || nb == 0) return na == nb;
    StgInt k = na < nb ? na : nb;
    if (memcmp(pa, pb, k) != 0) return 0;
    pa += k, pb += k, na -= k, nb -= k;
  }
}

// --- the startup checks ---

// a chunk's character count, or -1 if it isn't laid out as assumed
static StgInt check_chunk(StgClosure *c) {
  if (LTAG(c) != 1 || (StgWord)LUN(c)->header.info != TF.chunk_info) return -1;
  StgArrBytes *arr = (StgArrBytes *)LP(c, CH_ARR);
  StgInt off = LW(c, CH_OFF), len = LW(c, CH_LEN), count = LW(c, CH_COUNT);
  if (arr->header.info != &stg_ARR_WORDS_info || off < 0 || len <= 0 || (StgWord)(off + len) > arr->bytes) return -1;
  return utf8_prefix(chunk_bytes(c), count) == len ? count : -1;
}

// the characters under a node `depth` levels down (1: its children are chunks), or -1
static StgInt check_text_node(StgClosure *nd, int depth) {
  if (LTAG(nd) != 2 || (StgWord)LUN(nd)->header.info != LF.na_info || !is_small_array(LP(nd, 0))) return -1;
  StgInt arity = node_arity(nd), total = 0;
  if (arity < 2 || arity > 8) return -1;
  for (StgInt c = 0; c < arity; c++) {
    StgClosure *kid = node_kids(nd)[c];
    StgInt z = depth == 1 ? check_chunk(kid) : check_text_node(kid, depth - 1);
    if (z < 0) return -1;
    total += z;
  }
  return total == LW(nd, 1) ? total : -1;
}

// a digit of a level `depth` down with the given count; its characters, or -1
static StgInt check_text_digit(StgClosure *l, int depth, StgInt count) {
  if (check_slist(l) != count) return -1;
  StgInt total = 0;
  for (; LTAG(l) == 2; l = LP(l, 1)) {
    StgInt z = depth == 0 ? check_chunk(HEAD(l)) : check_text_node(HEAD(l), depth);
    if (z < 0) return -1;
    total += z;
  }
  return total;
}

// Checks a text's structure: every constructor's shape, the counts and sizes
// at every level, each chunk's character count against its bytes, and the
// rope's invariants. 1 if all is as assumed.
int64_t unison_jit_text_check(void **elems) {
  StgClosure *r = rope_of(settle(elems[0]));
  if (r == NULL) return 0;
  if (LTAG(r) == 1) return same_con0(r, TF.empty);
  if (LTAG(r) == 2) return (StgWord)LUN(r)->header.info == TF.one_info && check_chunk(LP(r, 0)) > 0;
  StgClosure *c = r;
  StgInt sizes[64], befores[64], inner = 0;
  int depth = 0;
  for (;; depth++) {
    if (depth >= 64) return 0;
    if (depth == 0 ? (StgWord)LUN(c)->header.info != TF.deep_info || !con_is(c, 3, 3, 2)
                   : (StgWord)LUN(c)->header.info != LF.mdeep_info || !con_is(c, 2, 3, 2))
      return 0;
    StgInt t = LW(c, 3);
    StgInt a = check_text_digit(LP(c, 0), depth, TPC(t)), b = check_text_digit(LP(c, 2), depth, TSC(t));
    if (a < 0 || b < 0 || TSIZE(t) <= 0 || LW(c, 4) != a) return 0;
    sizes[depth] = TSIZE(t), befores[depth] = a + b;
    StgClosure *m = LP(c, 1);
    // the top level always has a chunk in both digits; a level below, when it has a middle
    if ((depth == 0 || LTAG(m) != 1) && (TPC(t) == 0 || TSC(t) == 0)) return 0;
    if (LTAG(m) == 1) {
      if (!same_con0(m, LF.mnil)) return 0;
      break;
    }
    c = m;
  }
  for (; depth >= 0; depth--) {
    if (sizes[depth] != befores[depth] + inner) return 0;
    inner = sizes[depth];
  }
  // no two chunks next to each other are small enough to be one
  StgInt n = rope_size(r), prev = ROPE_THRESHOLD + 1, off;
  for (StgInt i = 0; i < n;) {
    StgClosure *ch = rope_chunk_at(r, i, &off);
    StgInt s = chunk_size(ch);
    if (off != 0 || prev + s <= ROPE_THRESHOLD) return 0;
    prev = s, i += s;
  }
  return 1;
}

// Learns the constructors from samples: elems[0] is the text "abc", elems[1]
// two pieces of one character more than the threshold appended, which is a
// Deep with one chunk in each digit, elems[2] the empty text. info[0] is
// Foreign's info pointer and info[1] Rope.threshold. The
// constructors of the digits and of the levels below are the list's
// (unison_jit_list_init has run). Returns 1, or the number of the check that
// failed.
int64_t unison_jit_text_init(void **raw, int64_t *info) {
  void *elems[3] = {settle(raw[0]), settle(raw[1]), settle(raw[2])};
  memset(&TF, 0, sizeof TF);
  TF.foreign_info = info[0];
  TF.threshold = info[1];
  if (TF.threshold < 1) return 14;
  StgInt piece = TF.threshold + 1;
  StgClosure *f = elems[0];
  if (LTAG(f) != 7 || (StgWord)LUN(f)->header.info != TF.foreign_info) return 2;
  StgClosure *w = LP(f, 0);
  const StgInfoTable *wit = get_itbl(LUN(w));
  if (!(wit->type >= CONSTR && wit->type <= CONSTR_NOCAF) || wit->layout.payload.ptrs != 1 ||
      wit->layout.payload.nptrs != 0)
    return 3;
  TF.wraptext_info = (StgWord)LUN(w)->header.info;
  TF.wraptext_tag = LTAG(w);
  StgClosure *one = LP(w, 0);
  if (!con_is(one, 2, 1, 0)) return 4;
  TF.one_info = (StgWord)LUN(one)->header.info;
  StgClosure *c = LP(one, 0);
  if (!con_is(c, 1, 1, 3)) return 5;
  TF.chunk_info = (StgWord)LUN(c)->header.info;
  StgArrBytes *arr = (StgArrBytes *)LP(c, CH_ARR);
  if (arr->header.info != &stg_ARR_WORDS_info) return 6;
  if (LW(c, CH_COUNT) != 3 || LW(c, CH_LEN) != 3 || LW(c, CH_OFF) < 0 || (StgWord)(LW(c, CH_OFF) + 3) > arr->bytes)
    return 7;
  if (memcmp(chunk_bytes(c), "abc", 3) != 0) return 8;
  StgClosure *deep = rope_of(elems[1]);
  if (deep == NULL || !con_is(deep, 3, 3, 2)) return 9;
  TF.deep_info = (StgWord)LUN(deep)->header.info;
  if (LW(deep, 3) != MK(2 * piece, 1, 1) || LW(deep, 4) != piece) return 10;
  StgClosure *pr = LP(deep, 0), *sf = LP(deep, 2);
  if (!same_con0(LP(deep, 1), LF.mnil) || check_slist(pr) != 1 || check_slist(sf) != 1) return 11;
  if (!con_is(HEAD(pr), 1, 1, 3) || LW(HEAD(pr), CH_COUNT) != piece || LW(HEAD(sf), CH_COUNT) != piece)
    return 13;
  StgClosure *e = rope_of(elems[2]);
  if (e == NULL || !con_nullary(e)) return 12;
  TF.empty = e;
  return 1;
}

// For the startup self-test. op 0: elems[0] ++ elems[1]; 1: take arg; 2: drop
// arg; the result goes in elems[3] and 1 is returned (0 if not handled).
// op 3: the size; op 4: elems[0] == elems[1]; these return the answer.
int64_t unison_jit_text_test(void **elems, int64_t op, int64_t arg) {
  UnisonJitCtx tmp = {0}, *ctx = &tmp;
  ctx->cap = rts_unsafeGetMyCapability();
  void *res = NULL;
  void *e0 = settle(elems[0]), *e1 = settle(elems[1]);
  switch (op) {
    case 0: res = unison_jit_text_append(ctx, e0, e1); break;
    case 1: res = unison_jit_text_cut(ctx, e0, arg, 1); break;
    case 2: res = unison_jit_text_cut(ctx, e0, arg, 0); break;
    case 3: return unison_jit_text_size(e0);
    case 4: return unison_jit_text_eq(e0, e1);
  }
  if (res == NULL) return 0;
  elems[3] = res;
  unison_jit_mark_bstk(elems, 3, 3);
  return 1;
}

// ---------------------------------------------------------------------------
// Partial applications
//
// The Name instruction: a function value with more arguments captured. The
// closure is PAp cix comb (useg, bseg) with the first two unpacked; its
// pointers are the reference, the entry, and the two boxes around the
// segment's arrays, and its words the rest of cix and comb. The new closure
// is a copy with longer arrays: the new arguments first, the last one at
// index 0 (as the interpreter's augSeg lays them out), then the old ones.

static StgWord PF_pap_info, PF_bytes_box_info, PF_array_box_info;
enum { PAP_USEG = 2, PAP_BSEG = 3, PAP_WORDS = 9 };
#define NAME_MAX_ARGS 4

void unison_jit_closure_init(int64_t *info) {
  PF_pap_info = info[0];
  PF_bytes_box_info = info[1];
  PF_array_box_info = info[2];
}

// NULL if `fun` isn't a PAp.
void *unison_jit_name(UnisonJitCtx *ctx, void *fun, int64_t n, int64_t u0, void *b0, int64_t u1, void *b1,
                      int64_t u2, void *b2, int64_t u3, void *b3) {
  if (LTAG(fun) != 1 || (StgWord)LUN(fun)->header.info != PF_pap_info) return NULL;
  StgArrBytes *us = (StgArrBytes *)LP(LP(fun, PAP_USEG), 0);
  StgMutArrPtrs *bs = (StgMutArrPtrs *)LP(LP(fun, PAP_BSEG), 0);
  StgWord old = bs->ptrs, total = old + n;
  StgWord cards = ROUNDUP_BYTES_TO_WDS(mutArrPtrsCards(total));
  StgWord bytes_words = sizeofW(StgArrBytes) + total, ptrs_words = sizeofW(StgMutArrPtrs) + total + cards;
  StgWord *p = list_alloc(ctx, 1 + PAP_WORDS + 4 + bytes_words + ptrs_words);
  StgWord *ubox = p + 1 + PAP_WORDS, *bbox = ubox + 2;
  StgArrBytes *nus = (StgArrBytes *)(bbox + 2);
  StgMutArrPtrs *nbs = (StgMutArrPtrs *)((StgWord *)nus + bytes_words);
  // the closure: a copy, but for the two boxes
  memcpy(p, LUN(fun), (1 + PAP_WORDS) * sizeof(StgWord));
  p[1 + PAP_USEG] = (StgWord)ubox | 1;
  p[1 + PAP_BSEG] = (StgWord)bbox | 1;
  ubox[0] = PF_bytes_box_info;
  ubox[1] = (StgWord)nus;
  bbox[0] = PF_array_box_info;
  bbox[1] = (StgWord)nbs;
  SET_INFO((StgClosure *)nus, &stg_ARR_WORDS_info);
  nus->bytes = total * sizeof(StgWord);
  SET_INFO((StgClosure *)nbs, &stg_MUT_ARR_PTRS_FROZEN_CLEAN_info);
  nbs->ptrs = total;
  nbs->size = total + cards;
  memset(&nbs->payload[total], 0, cards * sizeof(StgWord));
  int64_t u[NAME_MAX_ARGS] = {u0, u1, u2, u3};
  void *b[NAME_MAX_ARGS] = {b0, b1, b2, b3};
  for (int64_t k = 0; k < n; k++) {
    nus->payload[n - 1 - k] = u[k];
    nbs->payload[n - 1 - k] = (StgClosure *)b[k];
  }
  memcpy(&nus->payload[n], us->payload, old * sizeof(StgWord));
  memcpy(&nbs->payload[n], bs->payload, old * sizeof(StgWord));
  return (void *)((StgWord)p | 1);
}

// For the startup self-test: Name on elems[0] with `n` arguments, the k-th
// being the word 100 + k and the closure elems[1]; the result goes in elems[3].
int64_t unison_jit_name_test(void **elems, int64_t n) {
  UnisonJitCtx tmp = {0}, *ctx = &tmp;
  ctx->cap = rts_unsafeGetMyCapability();
  void *res = unison_jit_name(ctx, elems[0], n, 100, elems[1], 101, elems[1], 102, elems[1], 103, elems[1]);
  if (res == NULL) return 0;
  elems[3] = res;
  unison_jit_mark_bstk(elems, 3, 3);
  return 1;
}

// One context per OS thread. A Haskell thread stays on one OS thread for the
// whole of an unsafe foreign call, and everything that touches the context
// happens inside unison_jit_enter, so this is safe. Contexts are never freed;
// there are only as many as the runtime has worker threads.
static _Thread_local UnisonJitCtx *thread_ctx = NULL;

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
    ctx->stack_floor = top - size + CSTACK_RESERVE;
    if (trace)
      fprintf(stderr, "[jit] new context: thread stack %lld bytes, budget %lld, %lld frame records\n",
              (long long)size, (long long)cstack_budget, (long long)ctx->max_frames);
    thread_ctx = ctx;
  }
  return thread_ctx;
}

typedef int64_t (*UnisonNativeFn)(UnisonJitCtx *ctx, int64_t ap, int64_t fp, int64_t sp);

// Runs a native function. The arrays arrive as pointers to their first element,
// which is how GHC passes MutableByteArray# and MutableArray# to foreign calls
// (an unsafe call, so they can't move). The results go in the spare words past
// the last slot of the unboxed stack: out[0..2] hold the new ap, fp and sp, and
// out[3] the number of frame records. Up to UNISON_JIT_INLINE_FRAMES records
// follow from out[4] on, three words each; if there are more, out[4] is instead
// a malloc'd copy of them all, which the caller frees. Returns the status.
//
// This is the round trip's fixed cost, so it does as little as it can: one
// thread-local lookup for the context, one for the capability, no allocation.
int64_t unison_jit_enter(UnisonNativeFn fn, int64_t *ustk, void **bstk, void **pool,
                         int64_t stack_size, int64_t ap, int64_t fp, int64_t sp) {
  int64_t *out = ustk + stack_size;
  {
    StgArrBytes *arr = (StgArrBytes *)((StgWord *)ustk - sizeofW(StgArrBytes));
    if (__builtin_expect(arr->bytes < (StgWord)((stack_size + UNISON_JIT_OUT_WORDS) * 8), 0)) {
      fprintf(stderr, "[jit] the unboxed stack has no room for results (%lld bytes, %lld slots)\n",
              (long long)arr->bytes, (long long)stack_size);
      abort();
    }
  }
  UnisonJitCtx *ctx = get_ctx();
  Capability *cap = rts_unsafeGetMyCapability();
  ctx->ustk = ustk;
  ctx->bstk = bstk;
  ctx->pool = pool;
  ctx->stack_size = stack_size;
  ctx->hplim = hplim_of(cap);
  ctx->max_sp = sp;
  ctx->n_frames = 0;
  ctx->cap = cap;
  ctx->alloc_left = alloc_budget;
  // The budget counts down from here, but never below the thread's floor.
  int64_t here = (int64_t)&ctx;
  int64_t limit = here - cstack_budget;
  ctx->cstack_limit = limit > ctx->stack_floor ? limit : ctx->stack_floor;
  if (__builtin_expect(trace, 0))
    fprintf(stderr, "[jit] enter %p ap/fp/sp %lld/%lld/%lld hplim %p stress %lld/%lld (global %lld)\n", (void *)fn,
            (long long)ap, (long long)fp, (long long)sp, *ctx->hplim, (long long)ctx->stress_poll,
            (long long)ctx->stress_poll_left, (long long)stress_poll);
  int64_t status = fn(ctx, ap, fp, sp);
  // Under the stress modes: a function that leaves with the same exit a
  // million times in a row, without having moved the stack, is being asked to
  // run again and again and getting nowhere (a poll that always fires, say).
  // Say what the context looks like, once. (Without the stack condition this
  // would fire for any function a benchmark loop calls a million times.)
  if (__builtin_expect(stress_poll > 0 || stress_callee > 0 || trace, 0)) {
    static _Thread_local UnisonNativeFn last_fn;
    static _Thread_local int64_t last_status, repeats;
    if (fn == last_fn && status == last_status && status > 0 && ctx->sp == sp) {
      if (++repeats == 1000000)
        fprintf(stderr,
                "[jit] %p left with exit %lld a million times in a row: hplim %p, alloc_left %lld, stress poll %lld/%lld, "
                "C stack here %p, limit %p, floor %p, sp %lld, frames %lld\n",
                (void *)fn, (long long)status, *ctx->hplim, (long long)ctx->alloc_left, (long long)ctx->stress_poll_left,
                (long long)ctx->stress_poll, (void *)&ctx, (void *)ctx->cstack_limit, (void *)ctx->stack_floor,
                (long long)sp, (long long)ctx->n_frames);
    } else {
      last_fn = fn;
      last_status = status;
      repeats = 0;
    }
  }
  int64_t n = ctx->n_frames;
  out[0] = ctx->ap;
  out[1] = ctx->fp;
  out[2] = ctx->sp;
  out[3] = n;
  if (__builtin_expect(n > 0, 0)) {
    if (n > ctx->max_frames) {
      fprintf(stderr, "[jit] frame record buffer overflowed (%lld records)\n", (long long)n);
      abort();
    }
    size_t bytes = n * 3 * sizeof(int64_t);
    if (n <= UNISON_JIT_INLINE_FRAMES) {
      memcpy(out + 4, ctx->frames, bytes);
    } else {
      int64_t *copy = malloc(bytes);
      memcpy(copy, ctx->frames, bytes);
      out[4] = (int64_t)copy;
    }
    if (trace) fprintf(stderr, "[jit] %lld frame records\n", (long long)n);
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
