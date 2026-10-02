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
    (void *)&unison_jit_text_eq,     (void *)&unison_jit_name,
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
// A Unison list is a Unison.Util.Deque of Val, held as Foreign (WrapSeq deque).
// Every field of the deque is strict, so everything reachable from one is an
// evaluated, tagged constructor and can be read here without evaluating
// anything. These helpers do the common cases of the list primitives and
// return "not handled" (NULL, or a negative size) for the rest, which
// generated code leaves to the interpreter. They allocate with allocate(),
// like generated code, and never call into Haskell.
//
// The layouts are GHC's for the constructors in Deque.hs (pointer fields
// first, in declaration order, then the unpacked words). They are checked
// against sample deques at startup, see unison_jit_list_init and
// unison_jit_list_check; if anything is off the JIT stays off.

typedef struct {
  StgWord foreign_info, wrapseq_info, wrapseq_tag;
  StgWord deque_info, scons_info;
  StgWord val_info, data1_info, data2_info;
  StgClosure *nil, *sempty, *yn, *sn; // the constructors without fields (tagged)
} ListFacts;
static ListFacts LF;

#define LTAG(p) GET_CLOSURE_TAG((StgClosure *)(p))
#define LUN(p) UNTAG_CLOSURE((StgClosure *)(p))
#define LP(c, i) (LUN(c)->payload[i])
#define LW(c, i) ((StgInt)LUN(c)->payload[i])

// Deque sz n l m r tl subs: pointers l, r, tl, subs; words sz, n, m. Tag 2 (Nil is 1).
enum { DQ_L, DQ_R, DQ_TL, DQ_SUBS, DQ_SZ, DQ_N, DQ_M, DQ_WORDS };
// SCons x rest: tag 2 (SEmpty is 1).
// YC (Lvl (Dig pn pz pl) (Dig sn sz sl)) rest: tag 2 (YN is 1).
enum { YC_PL, YC_SL, YC_REST, YC_PN, YC_PZ, YC_SN, YC_SZ };
// SC (Sub (Lvl ...) ys) more: tag 2 (SN is 1).
enum { SC_PL, SC_SL, SC_YS, SC_MORE, SC_PN, SC_PZ, SC_SN, SC_SZ };
// Nodes: N8 (tag 1), N3 (tag 2), N2 (tag 3): the children, then the leaf count.

static inline StgInt node_arity(StgClosure *nd) { return LTAG(nd) == 1 ? 8 : LTAG(nd) == 2 ? 3 : 2; }
static inline StgInt node_size(StgClosure *nd) { return LW(nd, node_arity(nd)); }

// The deque inside a list value (tagged), or NULL if the closure isn't a list.
static inline StgClosure *deque_of(void *list) {
  if (LTAG(list) != 7 || (StgWord)LUN(list)->header.info != LF.foreign_info) return NULL;
  StgClosure *w = LP(list, 0);
  if (LTAG(w) != LF.wrapseq_tag || (StgWord)LUN(w)->header.info != LF.wrapseq_info) return NULL;
  return LP(w, 0);
}

static inline StgWord *list_alloc(UnisonJitCtx *ctx, int64_t words) {
  ctx->alloc_left -= words;
  return (StgWord *)allocate((Capability *)ctx->cap, words);
}

// Foreign (WrapSeq dq), in four words at p.
static inline void *wrap_list(StgWord *p, StgClosure *dq) {
  p[0] = LF.wrapseq_info;
  p[1] = (StgWord)dq;
  p[2] = LF.foreign_info;
  p[3] = (StgWord)p | LF.wrapseq_tag;
  return (void *)((StgWord)(p + 2) | 7);
}

static inline StgClosure *new_deque(StgWord *p, StgClosure *l, StgClosure *r, StgClosure *tl, StgClosure *subs,
                                    StgInt sz, StgInt n, StgInt m) {
  p[0] = LF.deque_info;
  p[1 + DQ_L] = (StgWord)l;
  p[1 + DQ_R] = (StgWord)r;
  p[1 + DQ_TL] = (StgWord)tl;
  p[1 + DQ_SUBS] = (StgWord)subs;
  p[1 + DQ_SZ] = sz;
  p[1 + DQ_N] = n;
  p[1 + DQ_M] = m;
  return (StgClosure *)((StgWord)p | 2);
}

// List.size, or -1 if the closure isn't a list.
int64_t unison_jit_list_size(void *list) {
  StgClosure *dq = deque_of(list);
  if (dq == NULL) return -1;
  return LTAG(dq) == 2 ? LW(dq, DQ_SZ) : 0;
}

// The SeqView closure for element x and the rest of the list (a Foreign):
// Data2 ref tag a b, whose pointers are ref, a's, b's and whose words are tag,
// a's, b's. The element is a Val: a pointer, then a word.
static inline void *new_view(StgWord *p, void *empty, int64_t elem_tag, int64_t left, StgClosure *x, void *rest) {
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

// A view from the end of a single-level deque whose digit there is empty:
// all sz elements are in the other digit's list `far`, the wanted one last.
// As the Haskell code does, half of what remains stays where it is and the
// rest moves to the empty digit (all of it, if three or fewer remain).
static void *view_rebalance(UnisonJitCtx *ctx, void *empty, int64_t elem_tag, int64_t left, StgClosure *far,
                            StgInt sz) {
  StgInt rm = sz - 1, k = rm <= 3 ? 0 : rm / 2;
  // exactly what is written below: the heap must have no gaps
  StgWord *p = list_alloc(ctx, 3 * rm + (rm == 0 ? 0 : 1 + DQ_WORDS) + 4 + 7);
  // the first k cells of far, copied; then the others, reversed
  StgClosure *kept = LF.sempty, **tail = &kept, *c = far;
  for (StgInt i = 0; i < k; i++, c = LP(c, 1), p += 3) {
    p[0] = LF.scons_info;
    p[1] = (StgWord)LP(c, 0);
    *tail = (StgClosure *)((StgWord)p | 2);
    tail = (StgClosure **)&p[2];
  }
  *tail = LF.sempty;
  StgClosure *moved = LF.sempty, *x = NULL;
  for (StgInt i = k; i < sz; i++, c = LP(c, 1)) {
    if (i == sz - 1) {
      x = LP(c, 0);
      break;
    }
    p[0] = LF.scons_info;
    p[1] = (StgWord)LP(c, 0);
    p[2] = (StgWord)moved;
    moved = (StgClosure *)((StgWord)p | 2);
    p += 3;
  }
  StgClosure *nd = LF.nil;
  if (rm != 0) {
    nd = left ? new_deque(p, moved, kept, LF.yn, LF.sn, rm, rm - k, k)
              : new_deque(p, kept, moved, LF.yn, LF.sn, rm, k, rm - k);
    p += 1 + DQ_WORDS;
  }
  void *f = wrap_list(p, nd);
  return new_view(p + 4, empty, elem_tag, left, x, f);
}

// The view of a list from the left (x, rest) or the right (rest, x): the
// SeqView closure the interpreter's VWLS and VWRS build. `empty` is the
// SeqViewEmpty closure (its first field is the type's Reference). Handled
// when the end's digit has an element to give that needs no repair: three or
// more, or any at all when the deque has a single level.
void *unison_jit_list_view(UnisonJitCtx *ctx, void *list, void *empty, int64_t elem_tag, int64_t left) {
  StgClosure *dq = deque_of(list);
  if (dq == NULL) return NULL;
  if (LTAG(dq) != 2) return empty;
  StgInt sz = LW(dq, DQ_SZ), n = LW(dq, DQ_N), m = LW(dq, DQ_M);
  StgClosure *l = LP(dq, DQ_L), *r = LP(dq, DQ_R), *tl = LP(dq, DQ_TL), *subs = LP(dq, DQ_SUBS);
  StgClosure *near = left ? l : r;
  StgInt count = left ? n : m;
  int single = LTAG(tl) == 1 && LTAG(subs) == 1;
  if (single && count == 0) return view_rebalance(ctx, empty, elem_tag, left, left ? r : l, sz);
  if (LTAG(near) != 2 || !(count >= 3 || single)) return NULL;
  StgClosure *x = LP(near, 0), *rest = LP(near, 1);
  StgWord *p = list_alloc(ctx, 7 + 4 + (sz == 1 ? 0 : 1 + DQ_WORDS));
  StgClosure *nd;
  if (sz == 1) {
    nd = LF.nil;
  } else {
    nd = left ? new_deque(p, rest, r, tl, subs, sz - 1, n - 1, m) : new_deque(p, l, rest, tl, subs, sz - 1, n, m - 1);
    p += 1 + DQ_WORDS;
  }
  void *f = wrap_list(p, nd);
  p += 4;
  return new_view(p, empty, elem_tag, left, x, f);
}

// cons (front) or snoc of the value (u, b). Handled when the end's digit
// stays the color it is: one to eight elements there, or up to nine (and
// none) when the deque has a single level.
void *unison_jit_list_push(UnisonJitCtx *ctx, void *list, int64_t u, void *b, int64_t front) {
  StgClosure *dq = deque_of(list);
  if (dq == NULL) return NULL;
  StgClosure *l, *r, *tl, *subs;
  StgInt sz, n, m;
  if (LTAG(dq) == 2) {
    sz = LW(dq, DQ_SZ), n = LW(dq, DQ_N), m = LW(dq, DQ_M);
    l = LP(dq, DQ_L), r = LP(dq, DQ_R), tl = LP(dq, DQ_TL), subs = LP(dq, DQ_SUBS);
    StgInt count = front ? n : m;
    int single = LTAG(tl) == 1 && LTAG(subs) == 1;
    if (!(single ? count <= 9 : (count >= 1 && count <= 8))) return NULL;
  } else {
    sz = n = m = 0;
    l = r = LF.sempty, tl = LF.yn, subs = LF.sn;
  }
  StgWord *p = list_alloc(ctx, 3 + 3 + 1 + DQ_WORDS + 4);
  // the element: Val u b (pointer first)
  p[0] = LF.val_info;
  p[1] = (StgWord)b;
  p[2] = (StgWord)u;
  p[3] = LF.scons_info;
  p[4] = (StgWord)p | 1;
  p[5] = (StgWord)(front ? l : r);
  StgClosure *cell = (StgClosure *)((StgWord)(p + 3) | 2);
  StgClosure *nd = front ? new_deque(p + 6, cell, r, tl, subs, sz + 1, n + 1, m)
                         : new_deque(p + 6, l, cell, tl, subs, sz + 1, n, m + 1);
  return wrap_list(p + 6 + 1 + DQ_WORDS, nd);
}

static inline StgClosure *list_nth(StgClosure *l, StgInt k) {
  while (k-- > 0) l = LP(l, 1);
  return LP(l, 0);
}

// The node of this level holding the leaf with i leaves before it (and j
// after it) in these levels, and the leaf's offset in the node. sh is log2 of
// the leaves under a full element of the level.
static StgClosure *list_find(StgClosure *ys, StgClosure *subs, int sh, StgInt i, StgInt j, StgInt *off) {
  StgClosure *pl, *sl;
  StgInt pz, sz;
  if (LTAG(ys) == 2) {
    pl = LP(ys, YC_PL), sl = LP(ys, YC_SL), pz = LW(ys, YC_PZ), sz = LW(ys, YC_SZ);
    ys = LP(ys, YC_REST);
  } else {
    pl = LP(subs, SC_PL), sl = LP(subs, SC_SL), pz = LW(subs, SC_PZ), sz = LW(subs, SC_SZ);
    ys = LP(subs, SC_YS);
    subs = LP(subs, SC_MORE);
  }
  if (i < pz) {
    for (;; pl = LP(pl, 1)) {
      StgClosure *e = LP(pl, 0);
      StgInt z = node_size(e);
      if (i < z) {
        *off = i;
        return e;
      }
      i -= z;
    }
  }
  if (j < sz) { // the suffix's list runs from the back
    for (;; sl = LP(sl, 1)) {
      StgClosure *e = LP(sl, 0);
      StgInt z = node_size(e);
      if (j < z) {
        *off = z - 1 - j;
        return e;
      }
      j -= z;
    }
  }
  StgInt k;
  StgClosure *nd = list_find(ys, subs, sh + 3, i - pz, j - sz, &k);
  // the child of nd holding leaf k
  if (LTAG(nd) == 1 && node_size(nd) == (StgInt)8 << sh) { // all children full
    *off = k & (((StgInt)1 << sh) - 1);
    return LP(nd, k >> sh);
  }
  for (StgInt c = 0;; c++) {
    StgClosure *child = LP(nd, c);
    StgInt z = node_size(child);
    if (k < z) {
      *off = k;
      return child;
    }
    k -= z;
  }
}

// List.at: Some x or None, as the interpreter's IDXS builds them. `none` is
// the None closure (its first field is the type's Reference).
void *unison_jit_list_index(UnisonJitCtx *ctx, void *list, int64_t i, void *none, int64_t some_tag) {
  StgClosure *dq = deque_of(list);
  if (dq == NULL) return NULL;
  if (LTAG(dq) != 2) return none;
  StgInt sz = LW(dq, DQ_SZ), n = LW(dq, DQ_N), m = LW(dq, DQ_M);
  if (i < 0 || i >= sz) return none;
  StgInt j = sz - 1 - i;
  StgClosure *x;
  if (i < n)
    x = list_nth(LP(dq, DQ_L), i);
  else if (j < m)
    x = list_nth(LP(dq, DQ_R), j);
  else {
    StgInt off;
    StgClosure *nd = list_find(LP(dq, DQ_TL), LP(dq, DQ_SUBS), 3, i - n, j - m, &off);
    x = LP(nd, off);
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

// --- checking the layouts ---

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

// the length of a digit's list, or -1 if it isn't one
static StgInt check_slist(StgClosure *l) {
  StgInt len = 0;
  for (; LTAG(l) == 2; l = LP(l, 1), len++)
    if ((StgWord)LUN(l)->header.info != LF.scons_info || len > 64) return -1;
  return l == LF.sempty ? len : -1;
}

// the leaves under a node `depth` levels down (1: its children are elements), or -1
static StgInt check_node(StgClosure *nd, int depth, int64_t *kinds) {
  StgWord tag = LTAG(nd);
  StgInt arity = tag == 1 ? 8 : tag == 2 ? 3 : tag == 3 ? 2 : 0;
  if (arity == 0 || !con_is(nd, tag, arity, 1)) return -1;
  *kinds |= 1 << tag;
  StgInt total = 0;
  for (StgInt c = 0; c < arity; c++) {
    StgInt z = depth == 1 ? 1 : check_node(LP(nd, c), depth - 1, kinds);
    if (z < 0) return -1;
    total += z;
  }
  return total == LW(nd, arity) ? total : -1;
}

// a digit of a level `depth` down with the given count and leaf count; its leaves, or -1
static StgInt check_digit(StgClosure *l, int depth, StgInt count, StgInt leaves, int64_t *kinds) {
  if (check_slist(l) != count) return -1;
  StgInt total = 0;
  for (; LTAG(l) == 2; l = LP(l, 1)) {
    StgInt z = check_node(LP(l, 0), depth, kinds);
    if (z < 0) return -1;
    total += z;
  }
  return total == leaves ? total : -1;
}

// Walks a whole list checking that every constructor has the shape and the
// counts this file assumes. Returns -1 if not, else a set of bits saying
// which constructors it met: 2, 4, 8 for the three kinds of node, 16 for a
// yellow level, 32 for a substack.
int64_t unison_jit_list_check(void **elems) {
  StgClosure *dq = deque_of(elems[0]);
  if (dq == NULL) return -1;
  if (LTAG(dq) == 1) return dq == LF.nil ? 0 : -1;
  if (!con_is(dq, 2, 4, 3) || (StgWord)LUN(dq)->header.info != LF.deque_info) return -1;
  int64_t kinds = 0;
  StgInt n = LW(dq, DQ_N), m = LW(dq, DQ_M);
  if (check_slist(LP(dq, DQ_L)) != n || check_slist(LP(dq, DQ_R)) != m) return -1;
  StgInt total = n + m;
  StgClosure *ys = LP(dq, DQ_TL), *subs = LP(dq, DQ_SUBS);
  for (int depth = 1;; depth++) {
    StgClosure *pl, *sl;
    StgInt pn, pz, sn, sz;
    if (LTAG(ys) == 2) {
      if (!con_is(ys, 2, 3, 4)) return -1;
      pl = LP(ys, YC_PL), sl = LP(ys, YC_SL);
      pn = LW(ys, YC_PN), pz = LW(ys, YC_PZ), sn = LW(ys, YC_SN), sz = LW(ys, YC_SZ);
      ys = LP(ys, YC_REST);
      kinds |= 16;
    } else if (ys == LF.yn) {
      if (subs == LF.sn) break;
      if (!con_is(subs, 2, 4, 4)) return -1;
      pl = LP(subs, SC_PL), sl = LP(subs, SC_SL);
      pn = LW(subs, SC_PN), pz = LW(subs, SC_PZ), sn = LW(subs, SC_SN), sz = LW(subs, SC_SZ);
      ys = LP(subs, SC_YS);
      subs = LP(subs, SC_MORE);
      kinds |= 32;
    } else
      return -1;
    StgInt a = check_digit(pl, depth, pn, pz, &kinds), b = check_digit(sl, depth, sn, sz, &kinds);
    if (a < 0 || b < 0) return -1;
    total += a + b;
  }
  return total == LW(dq, DQ_SZ) ? kinds : -1;
}

// Learns the constructors from samples. elems[0] is a list of four values
// made as  x <| (empty |> a |> b |> c),  so its fields are sz 4, n 1, m 3,
// a prefix holding x and a suffix of three; elems[1] is x (a Val); elems[2]
// is the empty list. info[] holds the info pointers of Foreign, Val, Data1
// and Data2. Returns 1 if everything is as this file assumes, else a number
// saying which check failed.
int64_t unison_jit_list_init(void **elems, int64_t *info) {
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
  if (!con_is(dq, 2, 4, 3)) return 4;
  LF.deque_info = (StgWord)LUN(dq)->header.info;
  if (LW(dq, DQ_SZ) != 4 || LW(dq, DQ_N) != 1 || LW(dq, DQ_M) != 3) return 5;
  StgClosure *l = LP(dq, DQ_L), *r = LP(dq, DQ_R), *tl = LP(dq, DQ_TL), *subs = LP(dq, DQ_SUBS);
  if (!con_is(l, 2, 2, 0)) return 6;
  if (LUN(LP(l, 0)) != LUN(elems[1])) return 10;
  if (!con_nullary(LP(l, 1))) return 11;
  LF.scons_info = (StgWord)LUN(l)->header.info;
  LF.sempty = LP(l, 1);
  if (check_slist(r) != 3 || !con_nullary(tl) || !con_nullary(subs) || LUN(tl) == LUN(subs)) return 7;
  LF.yn = tl;
  LF.sn = subs;
  StgClosure *e = deque_of(elems[2]);
  if (e == NULL || !con_nullary(e)) return 8;
  LF.nil = e;
  // a Val is a pointer and a word, tag 1
  if (!con_is(elems[1], 1, 1, 1) || (StgWord)LUN(elems[1])->header.info != LF.val_info) return 9;
  return 1;
}

// For the startup self-test: runs one helper on elems[0] with the other
// arguments in elems[1] (a closure) and arg, and puts the result in elems[3].
// op 0: view left, 1: view right (elems[1] the empty view, arg the tag);
// 2: cons, 3: snoc (elems[1] the boxed half of the value, elems[2]... unused, arg the unboxed half);
// 4: index arg (elems[1] None, tag in arg2). Returns 0 if the helper didn't handle it.
// (The context is a temporary one: the thread's own is made on its first real
// entry, after the settings are in place.)
int64_t unison_jit_list_test(void **elems, int64_t op, int64_t arg, int64_t arg2) {
  UnisonJitCtx tmp = {0}, *ctx = &tmp;
  ctx->cap = rts_unsafeGetMyCapability();
  void *res = NULL;
  switch (op) {
    case 0:
    case 1:
      res = unison_jit_list_view(ctx, elems[0], elems[1], arg, op == 0);
      break;
    case 2:
    case 3:
      res = unison_jit_list_push(ctx, elems[0], arg, elems[1], op == 2);
      break;
    case 4:
      res = unison_jit_list_index(ctx, elems[0], arg, elems[1], arg2);
      break;
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
// rope). Like the list's deque it is strict throughout, so these helpers read
// and build it directly. They are ports of the Haskell operations (the
// Semigroup, Take and Drop instances in lib/unison-util-rope's Rope.hs and
// the Chunk instances in Unison.Util.Text): same results, structure included.
//
//   Rope: Empty (tag 1), One chunk (tag 2), Two size left right (tag 3;
//         the pointers left and right, then the size)
//   Chunk count (Text array offset length), unpacked: the pointer to the
//         byte array, then the character count, byte offset and byte length
//
// Checked at startup like the list layouts (unison_jit_text_init).

typedef struct {
  StgWord foreign_info, wraptext_info, wraptext_tag;
  StgWord one_info, two_info, chunk_info;
  StgClosure *empty; // Rope's Empty (tagged)
} TextFacts;
static TextFacts TF;

enum { CH_ARR, CH_COUNT, CH_OFF, CH_LEN };
enum { TW_L, TW_R, TW_SZ };
// the most characters two chunks may have between them to be merged into one (Rope.threshold)
#define ROPE_THRESHOLD 32

static inline StgClosure *rope_of(void *text) {
  if (LTAG(text) != 7 || (StgWord)LUN(text)->header.info != TF.foreign_info) return NULL;
  StgClosure *w = LP(text, 0);
  if (LTAG(w) != TF.wraptext_tag || (StgWord)LUN(w)->header.info != TF.wraptext_info) return NULL;
  return LP(w, 0);
}

static inline StgInt rope_size(StgClosure *r) {
  switch (LTAG(r)) {
    case 2: return LW(LP(r, 0), CH_COUNT);
    case 3: return LW(r, TW_SZ);
    default: return 0;
  }
}

static inline const unsigned char *chunk_bytes(StgClosure *c) {
  return (const unsigned char *)((StgArrBytes *)LP(c, CH_ARR))->payload + LW(c, CH_OFF);
}

static inline StgClosure *new_two(UnisonJitCtx *ctx, StgClosure *l, StgClosure *r, StgInt sz) {
  StgWord *p = list_alloc(ctx, 4);
  p[0] = TF.two_info;
  p[1 + TW_L] = (StgWord)l;
  p[1 + TW_R] = (StgWord)r;
  p[1 + TW_SZ] = sz;
  return (StgClosure *)((StgWord)p | 3);
}

// One (Chunk count (Text arr off len)), or Empty for no characters
static StgClosure *new_one(UnisonJitCtx *ctx, void *arr, StgInt count, StgInt off, StgInt len) {
  if (count == 0) return TF.empty;
  StgWord *p = list_alloc(ctx, 5 + 2);
  p[0] = TF.chunk_info;
  p[1 + CH_ARR] = (StgWord)arr;
  p[1 + CH_COUNT] = count;
  p[1 + CH_OFF] = off;
  p[1 + CH_LEN] = len;
  p[5] = TF.one_info;
  p[6] = (StgWord)p | 1;
  return (StgClosure *)((StgWord)(p + 5) | 2);
}

// One holding the two chunks' characters in a fresh byte array
static StgClosure *merge_chunks(UnisonJitCtx *ctx, StgClosure *a, StgClosure *b) {
  StgInt la = LW(a, CH_LEN), lb = LW(b, CH_LEN);
  StgWord bytes = la + lb;
  StgArrBytes *arr = (StgArrBytes *)list_alloc(ctx, sizeofW(StgArrBytes) + ROUNDUP_BYTES_TO_WDS(bytes));
  SET_INFO((StgClosure *)arr, &stg_ARR_WORDS_info);
  arr->bytes = bytes;
  memcpy(arr->payload, chunk_bytes(a), la);
  memcpy((char *)arr->payload + la, chunk_bytes(b), lb);
  return new_one(ctx, arr, LW(a, CH_COUNT) + LW(b, CH_COUNT), 0, bytes);
}

// size-balanced append, leaving the left tree as is (szl is its size)
static StgClosure *rope_appendL(UnisonJitCtx *ctx, StgInt szl, StgClosure *l, StgClosure *r) {
  if (szl == 0) return r;
  switch (LTAG(r)) {
    case 1: return l;
    case 2: return new_two(ctx, l, r, szl + rope_size(r));
    default: {
      StgInt szr = LW(r, TW_SZ);
      if (szl >= szr) return new_two(ctx, l, r, szl + szr);
      StgClosure *l2 = rope_appendL(ctx, szl, l, LP(r, TW_L));
      return new_two(ctx, l2, LP(r, TW_R), szl + szr);
    }
  }
}

// ... leaving the right tree as is
static StgClosure *rope_appendR(UnisonJitCtx *ctx, StgClosure *l, StgInt szr, StgClosure *r) {
  if (szr == 0) return l;
  switch (LTAG(l)) {
    case 1: return r;
    case 2: return new_two(ctx, l, r, rope_size(l) + szr);
    default: {
      StgInt szl = LW(l, TW_SZ);
      if (szr >= szl) return new_two(ctx, l, r, szl + szr);
      StgClosure *r2 = rope_appendR(ctx, LP(l, TW_R), szr, r);
      return new_two(ctx, LP(l, TW_L), r2, szl + szr);
    }
  }
}

// a one-chunk rope `one` (of sz0 characters) in front of a rope
static StgClosure *rope_cons(UnisonJitCtx *ctx, StgInt sz0, StgClosure *one, StgClosure *as) {
  switch (LTAG(as)) {
    case 1: return one;
    case 2: {
      StgInt n = sz0 + rope_size(as);
      return n <= ROPE_THRESHOLD ? merge_chunks(ctx, LP(one, 0), LP(as, 0)) : new_two(ctx, one, as, n);
    }
    default: {
      StgInt sz = LW(as, TW_SZ);
      if (sz0 >= sz) return new_two(ctx, one, as, sz0 + sz);
      StgClosure *r = LP(as, TW_R);
      return rope_appendR(ctx, rope_cons(ctx, sz0, one, LP(as, TW_L)), rope_size(r), r);
    }
  }
}

// ... or behind it
static StgClosure *rope_snoc(UnisonJitCtx *ctx, StgClosure *as, StgInt szn, StgClosure *one) {
  switch (LTAG(as)) {
    case 1: return one;
    case 2: {
      StgInt n = rope_size(as) + szn;
      return n <= ROPE_THRESHOLD ? merge_chunks(ctx, LP(as, 0), LP(one, 0)) : new_two(ctx, as, one, n);
    }
    default: {
      StgInt sz = LW(as, TW_SZ);
      if (szn >= sz) return new_two(ctx, as, one, sz + szn);
      StgClosure *l = LP(as, TW_L);
      return rope_appendL(ctx, rope_size(l), l, rope_snoc(ctx, LP(as, TW_R), szn, one));
    }
  }
}

static StgClosure *rope_append(UnisonJitCtx *ctx, StgClosure *a, StgClosure *b) {
  if (LTAG(a) == 1) return b;
  if (LTAG(b) == 1) return a;
  if (LTAG(a) == 2) return rope_cons(ctx, rope_size(a), a, b);
  if (LTAG(b) == 2) return rope_snoc(ctx, a, rope_size(b), b);
  StgInt sz1 = LW(a, TW_SZ), sz2 = LW(b, TW_SZ);
  if (sz1 * 2 >= sz2 && sz2 * 2 >= sz1) return new_two(ctx, a, b, sz1 + sz2);
  if (sz1 > sz2) {
    StgClosure *l1 = LP(a, TW_L);
    return rope_appendL(ctx, rope_size(l1), l1, rope_append(ctx, LP(a, TW_R), b));
  }
  StgClosure *r2 = LP(b, TW_R);
  return rope_appendR(ctx, rope_append(ctx, a, LP(b, TW_L)), rope_size(r2), r2);
}

// the number of bytes the first k characters of UTF-8 text take
static inline StgInt utf8_prefix(const unsigned char *p, StgInt k) {
  const unsigned char *q = p;
  while (k-- > 0) q += *q < 0x80 ? 1 : *q < 0xE0 ? 2 : *q < 0xF0 ? 3 : 4;
  return q - p;
}

static StgClosure *rope_drop(UnisonJitCtx *ctx, StgInt n, StgClosure *as) {
  if (n <= 0) return as;
  switch (LTAG(as)) {
    case 1: return as;
    case 2: {
      StgClosure *c = LP(as, 0);
      StgInt count = LW(c, CH_COUNT);
      if (n >= count) return TF.empty;
      StgInt nb = utf8_prefix(chunk_bytes(c), n);
      return new_one(ctx, LP(c, CH_ARR), count - n, LW(c, CH_OFF) + nb, LW(c, CH_LEN) - nb);
    }
    default: {
      StgClosure *l = LP(as, TW_L), *r = LP(as, TW_R);
      StgInt szl = rope_size(l);
      if (n >= szl) return rope_drop(ctx, n - szl, r);
      StgClosure *l2 = rope_drop(ctx, n, l); // the tree isn't rebalanced
      return new_two(ctx, l2, r, rope_size(l2) + rope_size(r));
    }
  }
}

static StgClosure *rope_take(UnisonJitCtx *ctx, StgInt n, StgClosure *as) {
  switch (LTAG(as)) {
    case 1: return as;
    case 2: {
      StgClosure *c = LP(as, 0);
      if (n <= 0) return TF.empty;
      if (n >= LW(c, CH_COUNT)) return as;
      return new_one(ctx, LP(c, CH_ARR), n, LW(c, CH_OFF), utf8_prefix(chunk_bytes(c), n));
    }
    default: {
      StgClosure *l = LP(as, TW_L);
      StgInt szl = rope_size(l);
      if (n < szl) return rope_take(ctx, n, l);
      if (n >= LW(as, TW_SZ)) return as;
      StgClosure *r2 = rope_take(ctx, n - szl, LP(as, TW_R));
      return new_two(ctx, l, r2, szl + rope_size(r2));
    }
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

// Walks the chunks of a rope in order, with an explicit stack.
typedef struct {
  StgClosure *stack[96];
  int top;
} RopeIter;

static int rope_iter_start(RopeIter *it, StgClosure *r) {
  it->top = 0;
  it->stack[it->top++] = r;
  return 1;
}

// the next chunk, NULL at the end; *deep is set if the stack overflowed
static StgClosure *rope_iter_next(RopeIter *it, int *deep) {
  while (it->top > 0) {
    StgClosure *r = it->stack[--it->top];
    switch (LTAG(r)) {
      case 1: break;
      case 2: return LP(r, 0);
      default:
        if (it->top + 2 > 96) {
          *deep = 1;
          return NULL;
        }
        it->stack[it->top++] = LP(r, TW_R);
        it->stack[it->top++] = LP(r, TW_L);
    }
  }
  return NULL;
}

// Text equality: 1 or 0, or -1 if a closure isn't a text (or a rope is too
// deep for the walk here). Texts are equal when they have the same
// characters, however they are cut into chunks; UTF-8 makes that the same bytes.
int64_t unison_jit_text_eq(void *x, void *y) {
  StgClosure *a = rope_of(x), *b = rope_of(y);
  if (a == NULL || b == NULL) return -1;
  if (a == b) return 1;
  if (rope_size(a) != rope_size(b)) return 0;
  RopeIter ia, ib;
  int deep = 0;
  rope_iter_start(&ia, a);
  rope_iter_start(&ib, b);
  const unsigned char *pa = NULL, *pb = NULL;
  StgInt na = 0, nb = 0;
  for (;;) {
    if (na == 0) {
      StgClosure *c = rope_iter_next(&ia, &deep);
      if (c != NULL) pa = chunk_bytes(c), na = LW(c, CH_LEN);
    }
    if (nb == 0) {
      StgClosure *c = rope_iter_next(&ib, &deep);
      if (c != NULL) pb = chunk_bytes(c), nb = LW(c, CH_LEN);
    }
    if (deep) return -1;
    if (na == 0 || nb == 0) return na == nb;
    StgInt k = na < nb ? na : nb;
    if (memcmp(pa, pb, k) != 0) return 0;
    pa += k, pb += k, na -= k, nb -= k;
  }
}

// Checks a text's structure: every constructor's shape, the cached sizes, and
// each chunk's character count against its bytes. 1 if all is as assumed.
static StgInt check_rope(StgClosure *r, int depth) {
  if (depth > 200) return -1;
  switch (LTAG(r)) {
    case 1: return r == TF.empty ? 0 : -1;
    case 2: {
      if ((StgWord)LUN(r)->header.info != TF.one_info) return -1;
      StgClosure *c = LP(r, 0);
      if (LTAG(c) != 1 || (StgWord)LUN(c)->header.info != TF.chunk_info) return -1;
      StgArrBytes *arr = (StgArrBytes *)LP(c, CH_ARR);
      StgInt off = LW(c, CH_OFF), len = LW(c, CH_LEN), count = LW(c, CH_COUNT);
      if (arr->header.info != &stg_ARR_WORDS_info || off < 0 || len <= 0 || (StgWord)(off + len) > arr->bytes)
        return -1;
      if (utf8_prefix(chunk_bytes(c), count) != len) return -1;
      return count;
    }
    case 3: {
      if ((StgWord)LUN(r)->header.info != TF.two_info) return -1;
      StgInt a = check_rope(LP(r, TW_L), depth + 1), b = check_rope(LP(r, TW_R), depth + 1);
      return a < 0 || b < 0 || a + b != LW(r, TW_SZ) ? -1 : a + b;
    }
    default: return -1;
  }
}

int64_t unison_jit_text_check(void **elems) {
  StgClosure *r = rope_of(elems[0]);
  return r != NULL && check_rope(r, 0) >= 0;
}

// Learns the constructors from samples: elems[0] is the text "abc" in one
// chunk, elems[1] the same appended (unbalanced) to "de", elems[2] the empty
// text. info[0] is Foreign's info pointer. Returns 1, or the number of the
// check that failed.
int64_t unison_jit_text_init(void **elems, int64_t *info) {
  memset(&TF, 0, sizeof TF);
  TF.foreign_info = info[0];
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
  StgClosure *two = rope_of(elems[1]);
  if (two == NULL || !con_is(two, 3, 2, 1)) return 9;
  TF.two_info = (StgWord)LUN(two)->header.info;
  if (LW(two, TW_SZ) != 5 || !con_is(LP(two, TW_L), 2, 1, 0) || !con_is(LP(two, TW_R), 2, 1, 0)) return 10;
  if (LW(LP(LP(two, TW_L), 0), CH_COUNT) != 3 || LW(LP(LP(two, TW_R), 0), CH_COUNT) != 2) return 11;
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
  switch (op) {
    case 0: res = unison_jit_text_append(ctx, elems[0], elems[1]); break;
    case 1: res = unison_jit_text_cut(ctx, elems[0], arg, 1); break;
    case 2: res = unison_jit_text_cut(ctx, elems[0], arg, 0); break;
    case 3: return unison_jit_text_size(elems[0]);
    case 4: return unison_jit_text_eq(elems[0], elems[1]);
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
