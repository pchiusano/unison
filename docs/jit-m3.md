# M3: data

Working plan for milestone M3 of the [implementation plan](jit-implementation-plan.md). Written
before starting, 2026-09-30; the checkboxes are ticked as steps land. Each step is a commit.

**Goal.** Native code can build and take apart data values without leaving native code, and can
do so in a long loop without the heap growing without bound. Exit criteria (from the plan): the
test transcript passes under all stress modes with the debug runtime's heap sanity checks on; a
native loop that allocates heavily runs in bounded memory; the binary tree benchmark is at least
3× faster than the interpreter. Also in this milestone, because it falls out of the `DMatch`
work: booleans stay in registers.

## Order of work

- [x] **1. The constant pool grows.** (done 2026-09-30) The single global pool from M1 becomes appendable:
  `Unison.Runtime.JIT.Pool` gets `poolIndex :: PoolKey -> IO Int`, which interns a `Reference`,
  an enumeration constructor (`Enum r t`), or a boxed literal (text, bytes, ...) and returns its
  index, growing the array when needed. Old arrays are kept alive, so native code that read an
  address at entry is never left holding a freed one. The trampoline passes whatever array is
  current. Check: `Lit` of a text literal and `Pack r t ZArgs` compile to a pool load and no
  longer exit.
- [x] **2. Layouts and the allocator.** (done 2026-09-30; the hand-written IR check was skipped in favour of step 5's) The layout probe (`JIT.Layout`) learns the field order
  of `GData1`, `GData2`, `GDataG`, and of the two arrays in a `Seg` (`ByteArray#` and
  `Array#`), by planting distinct values in samples and reading the payload back. `Ctx` gets the
  capability pointer, set at entry, and the address of the runtime's `allocate` is defined as a
  symbol for generated code. A C helper `unison_jit_alloc_words(cap, n)` wraps `allocate` so
  the IR can call it (D9: cold code in C). Check: a hand-written IR function allocates a
  `GData1` and the interpreter prints it.
- [x] **3. Booleans as `i1`.** (done 2026-09-30) Each slot has a *kind* in the generator: boxed (the default,
  what M1 and M2 assume), or a boolean held as an `i1` register with no closure built. Comparison
  primitives produce the second kind. A `DMatch` on such a slot is a branch on the `i1`. The
  closure is materialized (`select` between the pool's true and false) only when the slot is
  written to the Unison stack: as a call argument, at a `Yield`, in an exit or unwind path, or
  into a constructor field. Kinds follow the structure of the code: saved and restored around
  branch arms and inline bindings, and results of calls are boxed. Check: `fib`'s test is one
  compare and branch after O2.
- [x] **4. Reading data.** (done 2026-09-30) `DMatch` on `GData1`, `GData2` and `GDataG`: after the tag test, the
  fields are pushed the way `dataBranch` pushes them (first field on top; a `Seg` copied in
  order). `NMatch` (numeric match on the unboxed word) is a `Match`. The default arm of a
  `DMatch` whose scrutinee has an unexpected pointer tag (a `Foreign` map, say) still exits.
  Check: the tree lookups no longer exit.
- [x] **5. Allocation: `Pack` with one or two fields.** (done 2026-09-30; one `allocate` call per `Pack`, see the decisions) A straight-line run of instructions is
  scanned for its allocation in words; one `unison_jit_alloc_words` call at the start of the run
  gets the space, and each `Pack` carves its object out of it: info pointer from the layout,
  `Reference` from the pool, tag constant, fields from the slots, and a correctly tagged pointer
  into the result slot. The allocation budget in `Ctx` is charged per run and checked by the
  entry poll (a `Reenter` exit); the trampoline refills it. Stress mode `alloc=N` sets the
  budget to `N` words. Check: `Cons.map` builds its list natively.
- [x] **6. `Pack` with three or more fields.** (done 2026-09-30) `GDataG` holds a `Seg`: a `ByteArray#` of the
  unboxed words and an `Array#` of the closures, both allocated in the same run and filled in
  argument order. Check: tree inserts no longer exit at `Pack`.
- [x] **7. Memory checks.** (done 2026-09-30: `churn` test added; test transcript, its `alloc=64,poll=7` stress run and the benchmark transcript all pass under `+RTS -DS` with no assertion failures; 20 M cells summed in bounded memory) A test that runs a native allocating loop long enough to need many
  collections, watched with `+RTS -s` for bounded residency. The transcript and stress matrix
  under the debug runtime (`stack build --ghc-options=-debug`) with `+RTS -DS`, which checks
  heap invariants at every GC: this is what verifies the write barrier and the pointer tags.
  Stress mode `alloc=64` in the matrix from here on.
- [x] **8. Measure.** (done 2026-09-30; numbers in the progress log under "M3 measurements". M3 complete.) Optimized build, both transcripts, results in the progress log. Update the
  plan's status.

## What M3 compiles

Everything M1 and M2 compile, plus:

| MCode | Native code |
| --- | --- |
| `Lit` of a boxed literal (`MT`, `MM`, `MY`) | pool load |
| `Pack r t ZArgs` | pool load of a prebuilt `Enum` |
| `Pack r t` with 1, 2, or n fields | allocate `GData1`, `GData2` or `GDataG`, fill, tag the pointer |
| `DMatch` on a constructor with fields | tag test, then the fields are pushed |
| `NMatch` | compare and branch on the unboxed word |
| comparisons feeding a `DMatch` | one branch, no closure |

Still exiting: `App`, `Jump`, `RMatch`, `ForeignCall` and the primitives implemented as foreign
calls, `Die`, over-application in `Yield`, `DMatch` on foreign-backed values. Those are M4.

## Decisions made while planning M3

- **One global pool, not one per module.** The design describes a pool per module, but native
  code in one module calls into another without going through the trampoline, so there is no
  moment to switch pools. One growable array with compile-time indices does the same job.
  Entries are interned, so two modules using the same `Reference` share an index.
- **`allocate` is called once per `Pack`, through a C wrapper,** not once per straight-line run
  as planned. A run can exit partway (a division by zero, an unsupported instruction), and then
  the words allocated for the `Pack`s after the exit would be left uninitialized in the nursery.
  The copying collector never looks at them, but the debug runtime's sanity checker walks the
  heap linearly and does, so a per-run chunk would need its unused tail filled with a dummy
  object on every exit path. Per-`Pack` calls keep every allocated word initialized before
  anything else can happen. A `DataG` still takes its five objects from one call. Inlining bump
  allocation is an M6 question.
- **The allocation budget is charged in words, per `Pack`,** so the poll at the next function
  entry sees it. The budget is a fraction of the nursery (default 256K words =
  2 MB); the trampoline's own allocation after a `Reenter` gets the GC to run.
- **Slot kinds live in the generator's state,** not in the MCode. Only booleans have a second
  kind for now; unboxed numbers already live in registers (the `%u` allocas). The same mechanism
  will carry "known unboxed, tag closure not yet written" later, which is the other half of the
  boxing cost.
- **A `Seg`'s arrays sit behind boxes.** `GDataG`'s `Seg` is a tuple, and a tuple's fields can't
  be unpacked, so the closure holds pointers to the lifted `ByteArray` and `Array` boxes, each
  one pointer to the real array. Reading a field of a constructor with three or more fields is
  therefore three dependent loads (box, array, element); building one allocates five objects
  (constructor, two boxes, two arrays). Found when the first version read the boxes as arrays
  and crashed. A flat representation for such constructors is worth considering later, but it is
  an interpreter change.
- **Constructor arities come from the data declarations.** A `DMatch` arm for constructor `u`
  must know how many fields to push, and the closure kind (`Data1`, `Data2`, `DataG`) follows
  from that. The interface registers every type's arities with the JIT as it loads them.
- **Enumerations come from the pool, never from `allocate`.** `Pack r t ZArgs` is a constant.

## Learnings and questions

Written after the milestone, from the discussion of it.

- **`Pack` allocation is one `allocate` per constructor, with nothing that can exit between the
  call and the last field store.** Batching several `Pack`s into one call is safe too, as long
  as the batch stops at the next instruction that can exit; that is now in
  [jit-optimization-ideas.md](jit-optimization-ideas.md), where the bigger win, inline bump
  allocation, also lives.
- **`GDataG` deserves a flat representation.** The `Seg` tuple costs two boxes and two arrays
  per constructor and three dependent loads per field. This is an interpreter change and is
  recorded in the ideas document.
- **The benchmark numbers were taken on a busy machine.** They show the ratios but not the
  absolute costs; re-measure on an idle machine before quoting them anywhere.
- **What surprised us.** That LLJIT resolves symbols from the process by itself (no
  `defineSymbol` for C helpers); that `startJIT` gets called twice because two runtimes are
  created per process; that the arity of a constructor isn't recoverable from a closure with
  three or more fields without the data declaration, which the interface now registers.
- **Open question.** The allocation budget is a fraction of the nursery and relies on the
  interpreter's next heap check to trigger the GC. This worked in the memory test (bounded
  residency, 400 budget exits over 20 M cells), but it hasn't been tried with a small nursery
  (`+RTS -A256k`) or on a capability shared with other busy Haskell threads.
