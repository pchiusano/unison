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
  This is also where batching should start to pay (Paul, 2026-10-01). Today a direct call in
  a module still passes everything through the Unison stack: the caller stores the arguments
  to `ustk` and `bstk`, the callee loads them at entry, the results come back the same way,
  and LLVM can't remove those stores when it inlines, because the stacks are reachable from
  `Ctx`. So a direct call saves only the cell load and null test, which is the 8% M5 measured
  on the cross-definition benchmark. Workers should therefore cover every direct call within
  a module, not only self-recursion: then an inlined callee is plain SSA code in its caller.
  Re-measure the batch rule (the bar for callers, B) once workers exist.

- **Direct calls within a module.** Done in M5: functions compiled together call each other by
  symbol, and LLVM can inline them.

- **Private copies of compiled callees, for inlining across definitions.** A callee gets hot
  and is compiled before its caller does (see [jit-m5.md](jit-m5.md)). M5's batches take the
  callers that are in use along with the callee, but a caller that shows up later still calls
  through the cell. When such a caller gets hot, its module could include its small,
  already-compiled callees again under private names with internal linkage, called directly,
  so LLVM can inline them; the callee's cell keeps its own code for everyone else. Costs: the
  callee is compiled once per caller module, and its exits and frames are registered again.
  Worth trying on a benchmark where a hot loop calls a small function from another
  definition. Noted 2026-09-30.

- **Compile a function's hot re-entry points together.** Each re-entry function asked for is a
  module of its own. Requests for the same function that are queued together could share a
  module, and then call each other directly. Noted 2026-09-30.

- **Calls through dynamic-scope references (`App (Dyn i)`).** Calling an ability handler
  (`Counter.next` inside `handle`) resolves the function through the handler environment,
  which lives in Haskell (`HEnv`), not on the Unison stack, so native code exits. Passing the
  current `denv` into `Ctx` (or a pointer to it) would let native code resolve it with a map
  lookup in C, or a call-out could return the value. Seen in the test transcript's ability
  loops: one exit per handler call. Noted 2026-09-30 during M4.

- **Hoist the self cell load.** A function's own cell is loaded before every self non-tail
  call; loading it once at entry is enough (the cell can't change under a running function).

- **Move the pending-arguments check to entry.** `Yield` compares `ap` with `fp` before every
  return. It's one compare, but it could be done once at entry instead.

## Code size

- **Re-entry functions on demand.** Done in M5 for the `on` mode (see [jit-m5.md](jit-m5.md)):
  a function's first compile is linear in its size, and a re-entry function is generated when
  a counter says it is used. Still generated with the function: the continuation of a call-out
  with no fast path, which could be counted too.

- **Shared frame write-back for exits.** Every exit block stores every live slot back to the
  Unison stack, so a function with many exits and a large frame is mostly exit code
  (`Duration.toText`: one function, 37 exits, 398 KB of IR). Exits at the same depth could
  share one write-back block that takes the exit index as a phi, or the write-back could be
  a loop over the allocas. Smaller IR means less LLVM time. Noted 2026-09-30.

- **Fallback: tail-call the body function at the end of an inline binding.** If lazy generation
  turns out not to be enough, an inline binding could end with a `musttail` call to the body
  function instead of having the body inlined after it, so every body exists once. This costs
  the fast path its registers: the binding's results and every live slot travel through the
  Unison stack at each `let`, which is what the interpreter does, and LLVM can no longer
  optimize across the boundary. Noted 2026-09-30 during M4; demoted to a fallback when M5 was
  planned. M5 measured the case it is for: with a threshold of 1, so that every re-entry point
  met is generated, the benchmark transcript spends 8.5 s compiling against 0.8 s normally.

## Builtins

- **Native `Text` operations.** Text-heavy code bounces through the trampoline on every
  `Text.++`, `Text.size`, `Int.toText` and so on, and each bounce also unwinds every native
  caller above it, which makes their `let` re-entry points hot (`Duration.toText` has about 40
  call-out continuations, nearly all for `Int.toText` and `++`). A Unison `Text` is a
  `Rope Chunk` with all fields strict: `Empty | One !Chunk | Two !Int !Rope !Rope`
  (`lib/unison-util-rope`), and `Chunk !Int !T.Text` unpacked (a character count, then the
  UTF-8 byte array, offset and length; `parser-typechecker/src/Unison/Util/Text.hs`). `Text`
  itself is a newtype. So native code or a C helper can pattern match on it directly, with
  layouts from the startup probe as for the data closures, and strict fields mean no thunks
  or indirections to worry about. In rough order of value and ease:
  - `Text.size`: the count is in the root node. A few loads, no allocation.
  - `Text.++`: the `Empty` cases return the other side; the balanced case allocates one `Two`
    node (four words). The unbalanced cases (`appendL`/`appendR`) and the merge of two small
    chunks (a new byte array and two `memcpy`s) can be a C helper called directly from native
    code, or stay on the call-out as the slow path at first.
  - `Text.==` and comparison: the single-chunk case is a length compare and `memcmp`;
    anything else calls out.
  - `Nat.toText` / `Int.toText`: a C helper that formats into a fresh byte array and wraps it
    in `One (Chunk n text)`.
  - `take`, `drop`, `uncons` and friends index by character over UTF-8, so they need a scan;
    later, probably as C helpers.

  The pattern is M4's for `Ref` and arrays: a native fast path with the existing call-out as
  the slow path, so the interpreter's implementation stays the source of truth for the hard
  cases. Things to check: the result should be the structure the Haskell code would build
  (the rope's balancing and the 512-character chunk threshold affect the cost of later
  operations, though not their results); allocation goes through the budgeted allocator; the
  debug runtime's heap checks cover the new objects. Prioritize by the call-out counts from
  `UNISON_JIT_STATS` on real programs. `Bytes` is the same `Rope` over byte chunks
  (`lib/unison-util-bytes`) and would follow the same way. Each builtin done this way also
  removes a call-out continuation and cools the re-entry points of its callers. Noted
  2026-09-30 after M4.

## Stack and frames

- **Dedicated C stack for native runs** (D13). GHC worker threads get 512 KB stacks, so the
  native call budget is about 250 KB, roughly 1600 frames of a small function; deeper recursion
  bounces through the interpreter every 1600 frames. Switching to a large malloc'd stack in
  `unison_jit_enter` would make that many times rarer. Cheap to try; needs care with signal
  handlers and with anything that inspects the stack.

- **Frame records for inline bindings.** A frame nested inside k inline `Let` bindings writes
  k + 1 records on unwind. Fine unless unwinds turn out to be frequent.
