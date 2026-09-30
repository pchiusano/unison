// M0 spike 2: using the GHC runtime from C, inside an unsafe foreign call.
// Everything here stands in for what JIT-compiled code will do.

#include "Rts.h"

// A boxed array reaches C as a pointer to its first element. The header is just before it.
static StgMutArrPtrs *array_header(StgClosure **elems) {
  return (StgMutArrPtrs *)((StgWord *)elems - sizeofW(StgMutArrPtrs));
}

// ---------------------------------------------------------------------------
// Layout probe (decision D7)

#define MAX_PAYLOAD 16

typedef struct {
  StgWord info;       // info pointer, to write into new closures of this kind
  StgWord ptr_tag;    // low bits of a pointer to this constructor
  StgWord type;       // closure type from the info table
  StgWord ptrs;       // number of pointer fields, which come first
  StgWord nptrs;      // number of non-pointer fields, which follow
  StgWord con_tag;    // constructor number from the info table
  StgWord raw[MAX_PAYLOAD];    // payload words, as found
  StgInt match[MAX_PAYLOAD];   // for pointer fields: index in elems of the same object, or -1
} Probe;

// elems[0] is a sample closure. elems[1..n-1] are the values that were put in its
// pointer fields. Matching is done here, in one call, because addresses can change
// at any GC and no GC can happen during an unsafe call.
void rt_probe(StgClosure **elems, StgInt n, Probe *out) {
  StgClosure *tagged = elems[0];
  StgClosure *c = UNTAG_CLOSURE(tagged);
  const StgInfoTable *itbl = get_itbl(c);
  out->info = (StgWord)c->header.info;
  out->ptr_tag = GET_CLOSURE_TAG(tagged);
  out->type = itbl->type;
  out->ptrs = itbl->layout.payload.ptrs;
  out->nptrs = itbl->layout.payload.nptrs;
  out->con_tag = itbl->srt;
  StgWord total = out->ptrs + out->nptrs;
  for (StgWord i = 0; i < MAX_PAYLOAD; i++) {
    out->raw[i] = 0;
    out->match[i] = -1;
    if (i >= total) continue;
    out->raw[i] = (StgWord)c->payload[i];
    if (i < out->ptrs) {
      StgClosure *field = UNTAG_CLOSURE(c->payload[i]);
      for (StgInt k = 1; k < n; k++)
        if (UNTAG_CLOSURE(elems[k]) == field) out->match[i] = k;
    }
  }
}

// ---------------------------------------------------------------------------
// Marking an array after native code has stored into it

void rt_mark(StgClosure **elems, StgInt lo, StgInt hi) {
  StgMutArrPtrs *arr = array_header(elems);
  SET_INFO((StgClosure *)arr, &stg_MUT_ARR_PTRS_DIRTY_info);
  for (StgInt card = lo >> MUT_ARR_PTRS_CARD_BITS;
       card <= hi >> MUT_ARR_PTRS_CARD_BITS; card++)
    *mutArrPtrsCard(arr, card) = 1;
}

// ---------------------------------------------------------------------------
// Allocation

// Describes how to build one kind of cons cell: a constructor with
// `words` payload words, where four of them vary per cell.
typedef struct {
  StgWord info;
  StgWord ptr_tag;
  StgWord words;
  StgInt off_ref;       // pointer: the type's Reference, copied from elems[ref_slot]
  StgInt off_con;       // word: constructor tag
  StgInt off_head_u;    // word: the head's unboxed value
  StgInt off_head_b;    // pointer: the head's type tag closure, from elems[nat_slot]
  StgInt off_tail_u;    // word: unused half of the tail
  StgInt off_tail_b;    // pointer: the tail
  StgWord con;
} ConsLayout;

// Prepends cells to the list in elems[list_slot], numbered from `first` upwards,
// until `count` cells are made or `budget` words are allocated. Returns how many
// were made. Calls allocate() once per cell, which is the slow way; batching comes later.
StgInt rt_build_list(StgClosure **elems, StgInt list_slot, StgInt ref_slot,
                     StgInt nat_slot, const ConsLayout *l, StgInt first,
                     StgInt count, StgInt budget, StgInt mark) {
  Capability *cap = rts_unsafeGetMyCapability();
  StgInt made = 0;
  StgInt used = 0;
  StgClosure *list = elems[list_slot];
  while (made < count && used + (StgInt)(l->words + 1) <= budget) {
    StgClosure *c = (StgClosure *)allocate(cap, l->words + 1);
    used += l->words + 1;
    SET_HDR(c, (const StgInfoTable *)l->info, CCS_SYSTEM);
    c->payload[l->off_ref] = elems[ref_slot];
    c->payload[l->off_con] = (StgClosure *)l->con;
    c->payload[l->off_head_u] = (StgClosure *)(StgWord)(first + made);
    c->payload[l->off_head_b] = elems[nat_slot];
    c->payload[l->off_tail_u] = (StgClosure *)(StgWord)0;
    c->payload[l->off_tail_b] = list;
    list = (StgClosure *)((StgWord)c | l->ptr_tag);
    made++;
  }
  elems[list_slot] = list;
  if (mark) rt_mark(elems, list_slot, list_slot);
  return made;
}

// ---------------------------------------------------------------------------
// Polling

static StgRegTable *reg_table(Capability *cap) {
  // A Capability starts with the function table, then the register table.
  return (StgRegTable *)((char *)cap + sizeof(StgFunTable));
}

// 1 if the runtime has asked this capability's thread to stop.
StgInt rt_poll(void) {
  return reg_table(rts_unsafeGetMyCapability())->rHpLim == NULL;
}

// Spins until the runtime asks this thread to stop, polling each time round.
// Returns the number of polls, or -1 if `max` was reached first.
StgInt rt_spin(StgInt max) {
  StgRegTable *reg = reg_table(rts_unsafeGetMyCapability());
  for (StgInt i = 0; i < max; i++)
    if (*(StgPtr volatile *)&reg->rHpLim == NULL) return i;
  return -1;
}
