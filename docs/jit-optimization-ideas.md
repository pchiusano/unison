# JIT: optimization ideas

Things noticed along the way that aren't in any milestone plan yet. Each entry says what was
observed, what the change would be, and roughly what it might buy. When one is picked up it
moves into a milestone plan; when one is measured and found not worth it, say so here rather
than deleting it.

## Representation

- **Flat `GDataG`.** A constructor with three or more fields holds a `Seg`, which is a tuple of
  a `ByteArray` and an `Array`. A tuple's fields can't be unpacked, so the closure points at two
  lifted boxes, each pointing at the real array. Reading one field is three dependent loads
  (box, array, element); building the constructor allocates five objects and 15 + 2n words for n
  fields, where a flat closure would be one object of 3 + 2n words (info, reference, tag, then
  the fields). The tree benchmark's `Node` has four fields, so this is on its hot path. This is
  an interpreter change (`GClosure`, `dataBranch`, `buildData`, serialization of values), not a
  JIT one, so it needs its own plan. Noted 2026-09-30 during M3.

- **Unboxed values without the tag write.** Every unboxed slot carries a type-tag closure in
  `bstk`. Native code keeps the word in a register but still stores the tag pointer whenever the
  slot reaches the stack. The slot-kind mechanism from M3 (booleans as `i1`) could carry "known
  Nat/Int/Float, tag not written" the same way, so the tag store happens only where the value
  escapes. Small, local to the code generator.

## Allocation

- **Bump allocation inline.** M3 calls `allocate` (through a C wrapper) once per `Pack`. GHC's
  own code allocates by bumping `Hp` against `HpLim` in the register table, with a call only
  when the block is full. Doing the same in generated code removes a call per constructor. It
  ties the code to the register table layout (already relied on for the poll) and to how
  `rCurrentAlloc`/nursery blocks work; check it against the debug RTS.

- **One allocation for several `Pack`s.** The design's original plan was one `allocate` per
  straight-line run. A single `Pack` needs no care: its fields are already evaluated, so nothing
  can exit between the allocation and the last field store. The only thing to respect when
  batching is that a run may contain an instruction that exits (`DIVN` on zero, a `ForeignCall`,
  an unsupported primitive) between two `Pack`s, and the objects after such an exit would be
  left unwritten in the nursery, which the debug RTS's heap walker would object to. So the batch
  boundary is "the next instruction that can exit", not "the end of the run": consecutive
  `Pack`s and the arithmetic between them share one call. Simple, and combines with the
  previous item.

## Calls

- **Workers with register arguments** (M6 in the plan). A self-recursive function's worker
  calls itself directly with LLVM arguments; only the wrapper touches the Unison stack. Exit
  status has to travel back through the register chain. See the M6 notes in the plan.

- **Direct calls within a module.** Functions compiled together still call each other through
  their cells (a load and a null test). They could call by symbol, letting LLVM inline; the
  cell path would remain for the `callee` stress mode and for callees that failed to compile.

- **Hoist the self cell load.** A function's own cell is loaded before every self non-tail
  call; loading it once at entry is enough (the cell can't change under a running function).

- **Move the pending-arguments check to entry.** `Yield` compares `ap` with `fp` before every
  return. It's one compare, but it could be done once at entry instead.

## Stack and frames

- **Dedicated C stack for native runs** (D13). GHC worker threads get 512 KB stacks, so the
  native call budget is about 250 KB, roughly 1600 frames of a small function; deeper recursion
  bounces through the interpreter every 1600 frames. Switching to a large malloc'd stack in
  `unison_jit_enter` would make that many times rarer. Cheap to try; needs care with signal
  handlers and with anything that inspects the stack.

- **Frame records for inline bindings.** A frame nested inside k inline `Let` bindings writes
  k + 1 records on unwind. Fine unless unwinds turn out to be frequent.
