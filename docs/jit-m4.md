# M4: call-outs and function values

Working plan for milestone M4 of the [implementation plan](jit-implementation-plan.md). Written
2026-09-30, before starting; the checkboxes are ticked as steps land. Each step is a commit.

**Goal.** The two things that still make compiled code slower than the interpreter go away:
instructions native code can't run get done by the interpreter *without* abandoning the rest
of the function, and calls to function values (`App` on a closure) are native calls. Exit
criteria (from the plan): map and fold over a user-defined cons list at least 3× faster; the
array entries at least 3× faster; nothing in the suite slower than the interpreter, including
the list entries.

## Order of work

- [x] **1. `CallOut` exits.** (done 2026-09-30; see the learnings for the `pushCount` table and the trampoline's check) A new exit kind: `CallOut cix instr rest reentry`, where `instr`
  is the MCode instruction to run, `rest` the section after it, and `reentry` the native
  function that continues from there. Any `Ins` the generator doesn't compile becomes one:
  `ForeignCall`, the primitives with no native version (`POWN`, `CAST`, the text and bytes
  ops, `REFR`/`REFW`/`REFN`, ...), `Print`, `Fork`, and so on. The trampoline runs the
  instruction with the interpreter's `exec`, which gives back the stack, the handler
  environment and `K` as the interpreter would have them, then enters `reentry` with that
  state. If `exec` reports an exception (a `ForeignCall` with the catch flag whose call
  failed), the trampoline does exactly what `eval'` does: wraps the failure, pushes a `Push`
  frame for `rest`, and applies the current exception handler; no re-entry. Check: `Ref`
  and text operations in the test transcript run without a `Resume`, visible in the statistics.
- [x] **2. Re-entry functions after call-outs.** (done 2026-09-30; the re-entry point is a cell held by the exit, not a bare pointer, since the re-entry function can itself exit with `GrowStack` or `Reenter`) For each call-out, the generator emits a
  second LLVM function in the same module, `u<grp>_<i>_r<n>`: the code for the rest of the
  function starting just after the instruction, at depth `d + 1` (the instruction's one result
  is on the stack). It loads its slots, does the entry stack check and poll, and generates
  `rest`. Its address is written into the `Exit` once the module is compiled; that is how the
  trampoline finds it (see the decisions). For a call-out inside an inline binding, the function
  is generated with that binding as its *frame base*: the interpreter enters it with `fp` at the
  binding's base, so it recovers the function's frame pointer as `fp - base`, writes frame
  records only for bindings nested inside that one (the outer ones are already on `K`), and the
  binding's own `Yield` returns `OK` so the interpreter pops the `Push` frame it holds for the
  body. Check: `refLoop` in the benchmarks runs as one native entry per iteration rather than
  one per `Ref` operation, and a call-out inside an `if` bound by a `let` re-enters too.
- [x] **2b. Re-entry for `Let`s inside bindings.** (done 2026-09-30; `letInIf` in the test transcript) The same frame-base mechanism gives M2's
  leftover its re-entry point: the body of a `Let` nested in an inline binding is generated
  from the parent function with the binding as frame base, and a cell for it (from the cell
  pool) is what the `Let` node and the `Push` frame carry, instead of the null cell attached
  today. Check: `letInBinding` in the test transcript shows a re-entry, not a `Resume`.
- [x] **3. Calls to function values.** (done 2026-09-30; also the interpreter's `apply` now enters native code for a compiled closure, which it never did before) `App` whose target is a stack slot (`Stk i`) or the
  environment (`Env`, a known combinator used as a value): the generator checks the pointer tag
  for `GPAp`, reads the arity, the captured-argument count (the length of the captured `Array`)
  and the native code cell from the unpacked `GCombInfo`, and if `arity == supplied + captured`
  and the frame has no pending arguments, lays the frame out as `apply` does (the supplied
  arguments, then the captured segment above them) and calls through the cell: a `musttail` in
  tail position, a status-checked call in a `Let` binding. A null cell, an over- or
  under-saturated call, or a non-`GPAp` value exits with `Resume` (plus frame records in a
  binding) as today. The `GPAp` layout comes from the probe. Check: `Cons.map` and `applyN` run
  natively; `runChoose` (continuations as values) still passes.
- [x] **4. `Ref` and arrays.** (done 2026-09-30; also universal `==`, `<`, `<=` and `compare` on two unboxed values of the same type, which `i == n` on Nats compiles to) Native `Ref.read` and `Ref.write`, and array read, write and
  size for the mutable array types the benchmarks use. A `Ref` is a `Foreign` wrapping a
  `MutVar#`; reading is a few loads through the probe's layouts, writing needs the runtime's
  `dirty_MUT_VAR` barrier (a C helper, cold path per D9), and array writes need the card mark
  the `bstk` marking already does. Bounds checks exit to the interpreter so the error is the
  interpreter's. Check: the `Ref` and array entries in the benchmarks are native.
- [x] **5. Tests and stress.** (done 2026-09-30: `catching`, `callOutTest`, `letInIf`, `arrayTest` and the "Function values" section in the test transcript; the full matrix and the debug-runtime runs pass, see the progress log) Transcript additions: a foreign call that raises and is caught
  inside a native function; a call-out inside an inline binding; a partially applied function
  called through several layers; over- and under-application through native code. The full
  matrix, including `callee` (which also makes function-value calls exit) and the debug-runtime
  run with `+RTS -DS`.
- [x] **6. Measure.** (done 2026-09-30; numbers in the progress log under "M4 measurements". The suite has no array entry, so the plan's "array entries 3× faster" criterion is untested: arrays are covered for correctness by `arrayTest` and run without exits. M4 complete.) Optimized build on an idle machine, both transcripts, results in the
  progress log. Update the plan's status and the learnings section below.

## What M4 compiles

Everything M1 to M3 compile, plus:

| MCode | Native code |
| --- | --- |
| `Ins i rest` for any `i` the generator has no native version of | `CallOut` exit; the interpreter runs `i`, native code continues at `rest` |
| `App` on a `GPAp` with exactly the right number of arguments | native call through the closure's cell |
| `Prim1 REFR`, `Prim2 REFW`, `MutableArray.size`/`read`/`write` | inline loads and stores with the write barrier; the call-out is the slow path (a wrong closure kind, an index out of bounds) |
| `EQLU`, `LEQU`, `LESU`, `CMPU` on two unboxed values with the same type tag | a word comparison; anything else calls out |
| `App (Env f) ZArgs` (a known combinator used as a value) | a pool constant: the `PAp` with nothing captured, built once |

Still exiting: `Jump` (captured continuations), `RMatch`, `Die`, over- and under-application,
and `DMatch` on foreign-backed values (maps).

## Decisions made while planning M4

- **Every unhandled instruction is a call-out, not a curated list.** `exec` runs any
  instruction and returns the state the interpreter would continue with (`henv`, stack, `K`),
  and native code only depends on the stack, so the trampoline can hand all of that back and
  re-enter. This covers `Reset`, `Capture` and the like as well; if one of them turns out to
  need more than that, it goes back to `Resume`.
- **Re-entry inside an inline binding uses a frame base.** When native code exits from inside
  an inline `Let` binding, the frame records turn the native picture (one frame for the whole
  function, the binding inlined) into the interpreter's (a fresh frame for the binding, a `Push`
  for the body on `K`). Code that re-enters there has to be generated in the interpreter's
  picture, or the slots would be read at the wrong offsets and the body would run twice. That
  only takes one number, the binding's base, used in three places: the function's frame pointer
  is `fp - base` at entry, records are written only for bindings inside this one, and yields and
  exits measure from the base. The first draft of this plan deferred this and let such
  call-outs `Resume`; there was no real saving in that.
- **How the trampoline finds a re-entry point.** The `CallOut` exit record holds the function
  pointer. When the generator meets an instruction it can't compile, it emits the exit and a
  second `define` in the same module for the rest of the function from that point; after the
  module is compiled, the address of that function is written into the exit. The trampoline
  reads the exit by index, runs the instruction, and calls the address. Nothing else ever needs
  to look it up, which is why these points don't get cells the way `Let` bodies do (those are
  entered by `yield` from anywhere, so their pointer has to travel with the `Push` frame).
- **Function values: exactly saturated only,** as the design says. Over-application means the
  callee returns into pending arguments, which the M1 `Yield` already hands to the interpreter;
  under-application allocates a new `GPAp` and is left for M6.
- **The captured-argument count is read from the array, not the closure.** A `GPAp`'s `Seg`
  is the same tuple of boxes as `GDataG`'s, so the count is the `Array#`'s element count, two
  loads away. Cheap, and avoids adding a field to `GPAp`.

## Learnings and questions

Written as the milestone went along; to be tidied when it is done.

- **MCode's `Ins` doesn't say how many values an instruction pushes.** `Prim1 LOAD` pushes two,
  `TRCE` none, most one. The generator keeps a table, `pushCount`, listing every constructor so
  that a new instruction fails to compile until it is classified, and the trampoline checks that
  the stack pointer moved by exactly that much before re-entering; otherwise it resumes. A wrong
  entry costs time, never correctness.
- **Re-entry points need cells after all.** The plan said a call-out's re-entry function could be
  a bare pointer in the exit, but that function can itself exit with `GrowStack` or `Reenter`,
  both of which mean "call this function again", and the trampoline does that through a cell.
- **The tables are filled from the first generation pass.** Exits and frames used to be
  registered from the first pass, which worked while both passes produced identical entries. With
  cells for auxiliary functions in them (placeholders in the first pass) the second pass's
  entries have to replace them. Easy to get wrong silently: the symptom was a null cell.
- **`Boolean` has no registered arities.** It's a builtin reference, not in `builtinDataSpec`, so
  `DMatch` on a boxed boolean (the result of a call-out, say) found no arities and resumed. The
  pointer tag says whether a value is an enumeration, so such matches are compiled for that
  case only.
- **The interpreter's `apply` never looked at the cell.** A function value applied by the
  interpreter (a thunk passed to a handler, `repeat`'s action) always ran interpreted, whatever
  the JIT had compiled. Now it enters native code like `enter` does.
- **Generation must not follow the code's structure blindly.** A `Let` inside an inline binding
  gets an auxiliary function for its body, and the body is also generated inline after the
  binding; nested k deep, that is 2^k copies. `Duration.toText` in base (a chain of `if`s each
  binding more `if`s, with a call-out in every arm) made the generator eat all memory, and
  benchmarks that print a duration never finished. Auxiliary functions are now memoized per
  (section, depth, frame base). That group still compiles to 208 auxiliary functions in 6 s,
  because each `Let` body inside a binding exists twice (inline, and as a function). An option
  recorded in the ideas document: end an inline binding with a tail call to the body function
  instead of inlining the body.
- **Diagnostics added on the way:** `UNISON_JIT_DISABLE=app,apply,ref,array,cmp,callout` turns
  features off one by one (how the bug above was bisected); `UNISON_JIT_STATS_EVERY=N` prints
  the exit counts every N exits, for evaluations that never finish; the compile log now has a
  "compiling" line before each group, so a hung compile is visible; a "partly interpreted" log
  line says why a branch arm or binding fell back to the interpreter.
- **`UNISON_JIT_TRACE` can't be used on programs that load base**: something in the load phase
  makes it exhaust memory. Not investigated.
- **A lazily built field is an indirection, not a box.** The first version of the pool constant
  for a combinator-as-value used `nullSeg`, whose two array boxes are CAFs; native code read
  through the indirection and refused every closure call (a resume per call, quietly slow, not
  wrong). Native code now checks a segment box's pointer tag before reading through it, in
  closure calls and `DataG` matches, and leaves the value to the interpreter otherwise.
- **The benchmark's biggest remaining exit was creating closures for lambdas.** `Cons.map (x ->
  x + 1) c` builds the lambda's closure with `App (Env f) ZArgs`, which `apply` turns into a
  `PAp` with nothing captured: 1.4 million resumes in the suite. It is a constant, so it lives
  in the pool now. Partial applications *with* captured arguments (`Name`) are still call-outs;
  that is M6's under-application work.
- **Open questions.** Eager compilation of base costs seconds for pathological functions
  (`Duration.toText`: 6 s); a size cap or the tail-call idea in the ideas document would bound
  it. `App (Dyn i)` (ability handler calls) still exits once per call, see the ideas document.
