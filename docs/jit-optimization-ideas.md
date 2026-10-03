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

- **Workers with register arguments.** Done in M6 (see the design, "Workers"). Left over:
  - Re-entry functions still pass everything through the stack. One that is hot (a loop
    that exits every iteration re-enters every iteration) could get a worker-like body.
  - Functions that return several values keep the uniform form.
  - The registers saved for exit paths (see jit-m6.md's learnings): half of `fib`'s
    per-call cost. Ideas: write only the slots the continuation can read (needs liveness
    for frame slots, and a look at what captured continuations do with dead slots); or
    keep type tags out of registers by passing unboxed values without them (see
    "Unboxed values without the tag write").
  - The type-tag closures are loaded through the pool at entry (two loads); they could be
    fields of `Ctx`.

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

- **Tiny native functions called from the interpreter.** Done in M6 step 8: the verdict
  "too small to enter" keeps the interpreter interpreting them. A leaner entry for leaf
  functions would be the other way.

- **Native ability requests.** A request (`App (Dyn i)`) is an exit today, and so is
  everything a handler starts with (`RMatch`, `Capture`, `InLocal`, `SetAff`, `Reset`),
  because the handler environment and `K` belong to the interpreter. The affine case (the
  handler runs and returns to the requester, no continuation captured) is the one that
  could be made native without continuation capture: it needs the dynamic environment
  readable from native code, `RMatch` as a match on the request's closure, and `InLocal`
  and `SetAff` as updates to an environment native code can see. Looked at in M6 step 6
  and left for a plan of its own; until then the static rule keeps request-heavy code
  interpreted. Noted 2026-10-01.

- **Build IR without `String`.** The generator builds each module as Haskell `String`s
  (megabytes of them), which is most of what the compile thread allocates. That
  allocation is shared with the running program: more GCs, and major ones sooner (M6:
  a one-shot 20 ms benchmark absorbing a 26 ms collection during warm-up). A builder over
  byte arrays, or LLVM's C API for building IR directly, would cut it. Noted 2026-10-01.

- **Hoist the self cell load.** A function's own cell is loaded before every self non-tail
  call; loading it once at entry is enough (the cell can't change under a running function).

- **Move the pending-arguments check to entry.** Done for workers in M6: the wrapper
  checks, and workers have no `ap`. Functions with the uniform signature still compare
  before every return.

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

- **Native `Text` operations.** (Size, `++`, take, drop and equality were done in M6 step
  5b; what is left is in "More of `Text` natively" below. The rest of this entry is the
  original note, and the rope it describes was replaced by a finger tree on 2026-10-02:
  see "The rope itself" below.) Text-heavy code bounces through the trampoline on every
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

- **`Bytes` operations.** Done 2026-10-02: the C rope functions take a `RopeKind`, and
  `Bytes.size`, `++`, `take`, `drop`, `at` and `flatten` are native, as is universal `==`
  on two texts or two bytes (see the progress log, "The bytes helpers"). Left as
  call-outs: `indexOf` (a search over the chunks, with the needle usually one chunk),
  `fromList`/`toList` (a walk of a list of Nats into a fresh array, and back), the
  `encodeNat*`/`decodeNat*`/`index*` foreign functions (a few bytes read at a position:
  `rope_chunk_at` finds them, with a copy only when they straddle two chunks), and
  comparison. The exit counts of a bytes-heavy program (a parser, a hash) say which first.
- **More of `Text` natively.** Done in M6: size, `++`, take, drop, equality. Left as
  call-outs: `Nat.toText` and `Int.toText` (a C helper that formats into a fresh byte
  array), `uncons`/`unsnoc`, comparison (`<=`, `<`), `indexOf`, and everything that is a
  foreign function rather than a primitive. Universal `==` on two texts is native since
  2026-10-02 (with the bytes work); universal `<`, `<=` and `compare` on texts aren't. The
  exit counts of a text-heavy program say which to do next. Since the rope became a finger tree the first and last chunks are the
  heads of two lists at the top of the structure, so `uncons`, `unsnoc` and a character
  lookup are a few loads and one small allocation, and `rope_chunk_at` (used by equality)
  already finds the chunk holding a position.
- **The rope itself** (`Unison.Util.Rope`, a finger tree of chunks since 2026-10-02;
  numbers in the progress log):
  - *`Json.toText` is 20% slower than on the old rope and the cause is not found* (the
    progress log has what was tried). Worth one more look at the interpreter's `catt`
    path: the same `"[" ++ x ++ "]"` is three times faster through the C helper.
  - *Appending two large texts is slower than it was.* The size-balanced tree it replaced
    made one node when the two sides were within a factor of two of each other (20 to 30
    ns); the finger tree packs the inner digits into nodes at every level (150 to 290 ns
    when both sides have a middle, which takes more than about 20 chunks each). It is the
    Deque's append, so whatever makes that cheaper (see "The list structure itself") helps
    here too. A text built by appending short pieces doesn't go through this path.
  - *A chunk with room to grow.* Appending a short piece to a text copies the last chunk
    into a new array each time (up to the threshold's worth of characters). A last chunk
    with spare capacity that the next append writes into, as a string builder does, would
    make it one small copy. A persistent structure needs to know that nobody else has
    appended to the same array (an owner's fill mark, checked and advanced atomically), so
    this is a design of its own.
  - *Characters of one byte.* A chunk whose character count equals its byte length is
    all ASCII, and indexing, taking and dropping in it need no scan. `Data.Text`'s
    operations don't know that; the chunk instances in `Unison.Util.Text` and the C
    helpers (`utf8_prefix`) could check it. Chunks loaded from a literal are up to 512
    characters, and the scan is most of `Text.at`, `take` and `drop` on them.
  - `take` and `drop` build the part of the rope before the cut and then `snoc` the cut
    chunk's piece, which allocates the top level twice. Building it once would take the
    10 to 20% they trail the old rope by on texts of short chunks.
  - Equality and comparison in Haskell go through lazy lists of chunks (as before), and in
    C find each next chunk by its position, which costs a walk down per chunk. A cursor
    that remembers where it is would make both a single pass.
  - The threshold (64) was chosen from 16, 32, 64 and 128 on the rope benchmark: 64 and
    128 halve the cost of walking and comparing against 32 and cost nothing when building;
    128 makes appending pieces of 40 characters and indexing short-chunk texts slower.
  - Any change here has to be made in the C helpers too (see the progress log).
- **What is left of lists.** Every list primitive is native since 2026-10-02 (the C
  helpers are ports of `Unison.Util.Deque`). What still leaves native code or costs more
  than it should:
  - The list operations that are foreign functions rather than primitives (`List.sort`,
    conversions from `Text` and `Bytes`): call-outs like any other foreign function.
  - Every operation is a call into C, including the common push and pop, which are a
    dozen instructions. Generating those two cases inline (the digit has room; the digit
    keeps an item) and calling C for the rest would remove the call and let LLVM see the
    allocation.
  - A list literal is one call per element plus one to wrap the result. A helper that
    takes the elements from the stack and builds the tree in one pass (what `fromListN`
    does) would be better for long literals.
  - `SPLL`/`SPLR` do a take and a drop, two walks to the same place; a single split would
    share the walk.
- **Partial applications of function values.** M6 step 7 builds a partial application
  natively when the function is known at code generation. A function value applied to too
  few arguments (`g = f x` where `f` is itself a closure) still resumes in the interpreter:
  whether the call is under-saturated is only known at run time, so the closure-call code
  needs a second continuation that joins the body with the new closure instead of a call's
  result. The helper already handles closures with arguments captured. Also: more than
  four arguments at once.
- **The list structure itself** (`Unison.Util.Deque`, a strict finger tree, the runtime's
  list since 2026-10-02; numbers in the progress log, where it is called Deque2, its name
  while the structure it replaced still existed):
  - *JSON parsing is slower than it was on `Data.Sequence`, and the list is not why.* With
    the JIT off: 7.2 µs per document on `Data.Sequence`, 10.2 µs on the old Deque, 10.1 µs
    on Deque2, whose pushes are faster than both; complex parsing 10.7, 18.1, 18.1 µs. So
    something else changed with the first swap. Candidates: the conversion at the
    `ANF.Value`/`Term.List` boundary, `fromList`, or the parser's use of `Sq.empty` and
    `|>` no longer fusing with something. A profile of that one benchmark is the place to
    start. `List.range` per element (42 ns to 102 ns, a very large list built in one go)
    is in the same position.
  - The rope's append chooses which side's digit to copy when both sides have no middle
    (the shorter one); `Deque.append` always extends the left prefix, so `acc ++ [x, y]`
    on a list of up to twenty elements copies `acc`'s cells. The same choice is a few
    lines there and in `lv_append`.
  - *Where it trails `Data.Sequence`:* `append` of two large pieces (1.5 to 1.8 times),
    `drop` (1.1 to 1.3), `fromList` of a long list (3 times; it is a fold of `snoc`, and
    the bulk `fromListN` measured no faster for a reason not yet found). Append and drop
    spend their time reading digit lists cell by cell on data that is not in the cache.
    Digits that are one object, without the array calls, are the thing to try: an array
    with an offset so pops don't copy, filled by single writes.
  - *What was tried and lost while it was built:* array digits below the top level (pushes
    and pops about 1 ns slower, append only 15% faster; half of append's time was then
    inside `copySmallArray#`, `memmove` and `newSmallArray#`, which are calls into the
    runtime even for three elements), and a single array-only node constructor in place
    of the inline node of eight leaves (pushes 7% slower, pops 20 to 45%, `sum` and
    `toList` 60 to 70%).
  - Any change here has to be made in the C helpers too (see the progress log).

  The structure it replaced (worst-case O(1) pushes and pops, about twice the code, slower
  on every operation measured) was deleted on 2026-10-02; it is in the branch's history,
  in the M5a commit.
  `Unison.Util.Skews` (two skew binary lists back to back, also Paul's) is in the same
  package as a possible alternative: it compiles, nothing uses it, and it hasn't been
  measured against either structure.

## Release

Taken out of M6 on 2026-10-01 so that milestone could be about performance on real programs;
for a later milestone.

- **Compiled programs** (`run.compiled`, the standalone path in `Interface.hs` that
  restores a code cache from a file) build their combinators without native code cells, so
  the JIT does nothing there. Noticed 2026-10-01, not checked further.
- **Static linking of LLVM** (D4 said dynamic first, static before release).
- **Linux x86-64**, then the remaining Unix platforms (D6). Skipped so far: every spike and
  test has run on macOS arm64 only.
- **Documentation** for users: the modes, the environment variables, what to expect.
- **Whether the JIT is on by default**, and whether the `jit` package flag stays.
- **Native versions of more builtins** as call-out statistics from real programs show them
  to be hot (see "Builtins").

## Stack and frames

- **Call-outs without unwinding: native code on its own C stack.** Today an exit in a callee
  unwinds every native caller, and each comes back through a re-entry point. If native code
  ran on a C stack of its own, a call-out could switch back to the Haskell thread's stack,
  let the interpreter run the one instruction, and switch back in, with all the native frames
  still there. The obstacle is the GC: it can run during that instruction and move objects,
  so every parked native frame would need its live values on the Unison stack before any
  call and would have to reload them afterwards (a flag set by the trampoline could make the
  reload conditional). There would also be a parked stack per Unison thread and per nested
  evaluation, and exceptions thrown by the instruction would have to discard it. Considered
  when M6 was planned and set aside in favour of making exits rarer and cheaper; worth
  revisiting if exits still dominate after M6. Noted 2026-10-01.

- **Dedicated C stack for native runs** (D13). GHC worker threads get 512 KB stacks, so the
  native call budget is about 250 KB, roughly 1600 frames of a small function; deeper recursion
  bounces through the interpreter every 1600 frames. Switching to a large malloc'd stack in
  `unison_jit_enter` would make that many times rarer. Cheap to try; needs care with signal
  handlers and with anything that inspects the stack.

- **Frame records for inline bindings.** A frame nested inside k inline `Let` bindings writes
  k + 1 records on unwind. Fine unless unwinds turn out to be frequent.
