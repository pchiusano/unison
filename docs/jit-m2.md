# M2: calls and frames

Working plan for milestone M2 of the [implementation plan](jit-implementation-plan.md). Written
before starting, 2026-09-30; the checkboxes are ticked as steps land. Each step is a commit.

**Goal.** Native code can make non-tail calls, and a function can be re-entered natively after
one of its calls exited to the interpreter. Exit criteria (from the plan): the test transcript
passes under all stress modes so far (`poll`, `ustack`, `cstack`, `callee`), recursion one
million deep runs without overflowing the C stack, the continuation tests pass, and "fib 20" is
at least 5× faster than the interpreter.

## Order of work

- [x] **1. Frame records and the frame table.** (done 2026-09-30) `Ctx` gets a frame record buffer (`frames`,
  `n_frames`, `max_frames`); `unison_jit_enter` empties it on every entry. `Unison.Runtime.JIT.Frames`
  is a global table like the exits table: an entry holds what a `Push` frame needs besides the
  two runtime sizes (the `Let` body's `CombIx`, stack guard, `RSection` and re-entry cell, see
  step 3), and a record holds the entry's 1-based index plus `sp - fp` and `fp - ap`. After
  native code returns, the trampoline reads the records and builds `Push` frames on top of `k`,
  last record first, before acting on the status. No generated code writes records yet: test by
  having the C side write a record by hand under a stress setting and checking the interpreter
  continues correctly.
- [x] **2. Non-tail calls.** (done 2026-09-30) `Let` whose binding is a `Call` to a known function compiles to a
  native call. Arguments are stored above the caller's frame and the callee is entered with
  `ap = fp = sp0`, `sp = sp0 + arity`, exactly the stack `saveFrame`, `moveArgs` and `acceptArgs`
  would produce. On `OK`, the results are loaded from the stack and the `Let` body continues
  inline at depth `d + m`. On any other status, the caller writes its own slots to the stack,
  writes its frame record and returns the status. If the callee's cell is null, `Resume` at the
  `Let` itself, since the interpreter's `Let` case pushes the frame and does the call. Callees in
  the same module are called by symbol (with a null-cell check on the function that failed to
  compile, if any), others through their cell. The entry stack check covers the largest frame in
  the function, including every `Let` body. Check: "fib 20" runs without exiting.
- [x] **3. Re-entry points.** (done 2026-09-30, before step 2: it needed no code generation) The `Let` body's combinator is already in the group: `emitLet`
  records it as a `Lam` whose arity is the depth of the frame at the body, so calling it with the
  whole frame as arguments *is* resuming at the body. M1 already compiles those combinators, and
  they have cells. So a re-entry point costs no new code generation: the `Let` node and the
  `Push` frame carry the body combinator's cell, and `yield`'s `Push` case checks it after
  `restoreFrame` and `ensure`, entering native code through the trampoline if it's non-null.
  Cells are attached to `Let` nodes at resolution, alongside the combinators. Check: a call chain
  where the innermost callee exits (an ability request) finishes the callers natively, visible in
  `UNISON_JIT_STATS` as far fewer `Resume` exits.
- [x] **4. C stack guard.** (done 2026-09-30; GHC worker threads get 512 KB stacks, so about 250 KB is usable) `unison_jit_enter` puts a limit in `Ctx`: the thread's stack bounds
  (from `pthread_get_stackaddr_np`/`pthread_get_stacksize_np`) minus a reserve for the runtime's
  own C code. Before each non-tail call the generated code compares the stack pointer with the
  limit and, if it's exhausted, exits with `Resume` at the `Let` instead of calling. The frame
  record buffer is sized from the same budget so it can't overflow. Stress mode `cstack=N` sets
  the budget to `N` bytes. Check: "depth 1000000" passes with the default budget and under
  `cstack=4096`, and `UNISON_JIT_LOG` prints the thread's stack size so we know what GHC gives us.
- [x] **5. Stress mode `callee`.** (done 2026-09-30; matrix passes) `callee=N`: every `N`th call through a cell or by symbol
  treats the callee as not compiled, exercising the `Resume`-at-`Let` path, frame records and
  re-entry on ordinary programs. Then the full test matrix: off, eager, and eager with each of
  `poll`, `ustack`, `cstack`, `callee`.
- [x] **6. Tests.** (done 2026-09-30) Add to `jit-tests.md`: a `Let` whose callee performs an ability request under
  a multi-shot handler (the frame built from a record is captured and resumed twice, so the body
  re-enters twice); a non-tail chain three deep where only the innermost exits; deep mutual
  non-tail recursion.
- [x] **7. Inlined `Let` bindings** (done 2026-09-30). A `Let` whose binding is
  a section the generator can compile inline (a `Match` or straight-line instructions ending in
  `Yield`) is compiled without a call: the binding's `Yield` becomes a branch to the body. Exits
  inside the binding see the stack as the interpreter would (`ap = fp = sp0`) and write the
  enclosing `Let`'s frame record first. Without this, a function whose `Let` binds an `if`
  exits at every such `Let`.
- [x] **8. Measure.** (done 2026-09-30; numbers in the progress log under "M2 measurements". M2 complete.) Optimized build, the benchmark transcript with the JIT off and in eager
  mode, results in the progress log. Update the plan's status.

## What M2 compiles

Everything M1 compiles, plus:

| MCode | Native code |
| --- | --- |
| `Let (Call known f) cix g body` | store the arguments, C stack check, call `f`, status check; on `OK` continue into `body`; otherwise write slots and a frame record and return the status |
| `Let` whose callee's cell is null, or with a binding native code can't make (`App` to a function value, `Jump`, ability requests) | `Resume` at the `Let` (as M1, but now the function still compiles) |
| `Let` body combinators | compiled as ordinary functions by M1; used as re-entry points |
| `Let` with any other binding the generator can start on (a `Match`, arithmetic, a nested `Let`) | binding inline: its `Yield` stores the results and branches to the body; a call in tail position of the binding returns to the body; exits inside it write a frame record for the `Let` |

Still exiting: `App`, `Jump`, `DMatch` on constructors with fields, `NMatch`, `RMatch`, `Pack`,
`Unpack`, `ForeignCall`, `Die`, over-application in `Yield`. Those are M3 and M4.

## Decisions made while planning M2

- **Re-entry points are the `Let` body combinators.** The design has a re-entry function per
  `Let` generated separately and stored in a Haskell structure keyed by `CombIx`. Neither is
  needed: the MCode emitter already makes each `Let` body a combinator with the frame as its
  arguments, M1 compiles it, and its cell is where the interpreter looks. The lookup at `yield`
  becomes one load and a null test, which matters because `yield` pops a `Push` on every
  interpreted non-tail return, compiled or not. The cost is a pointer field on `Let` and on
  `Push`, and every place that builds or reflects a `Push` (continuation values, profiling,
  `reify`) has to carry it; there are about fifteen such sites.
- **A `Let` inside a binding has no re-entry point.** Its body combinator's arguments are the
  whole MCode context, which includes the enclosing function's slots *below* the binding's
  frame, so its arity is bigger than the frame the interpreter has when it pops the `Let`'s
  `Push`. Entering it natively read the wrong slots (found by the benchmark transcript, not the
  tests: `printTime` has such a `Let`). Those `Let`s get a null cell at resolution and their
  bodies are interpreted after an unwind; native code still compiles them inline. Fixing this
  properly means generating the body combinator with a frame offset, and is left for later.
- **Callers write nothing on the normal path.** A callee only touches the stack above its own
  `fp`, which is the caller's `sp`, so the caller's slots are untouched when the callee returns.
  Locals stay in allocas across the call and are written to the stack only on the unwind path,
  together with the frame record. The design's "core rule" still holds: by the time Haskell runs,
  every frame has been written.
- **The frame table is global** like the exits table, filled when a module is compiled. A record
  is 24 bytes: table index, `sp - fp` (a compile-time constant, stored anyway to keep the
  trampoline simple), `fp - ap`.
- **The C stack check reads the stack pointer** (`llvm.stacksave`, one instruction on arm64) and
  compares it with the limit in `Ctx`. No depth counter. The budget is the pthread stack minus a
  reserve; a dedicated large stack is still deferred (D13). Exiting at the limit costs one
  interpreted `Let` per limit hit, after which the callee runs natively again, so deep recursion
  alternates between native and interpreted frames without ever using more C stack than the
  budget.
- **Calls stay unspecialized.** A non-tail call still passes through the uniform
  `UnisonNativeFn` type and the Unison stack. Returning results in registers is M6.
- **Inline bindings are a frame context, not a function.** The generator carries the list of
  inline `Let`s it is inside (innermost first). Every exit path writes slots up to the current
  depth, sets `ap = fp` to the innermost binding's base, and writes one frame record per
  enclosing `Let` (innermost first, with `fp - ap` only on the outermost, since inside a binding
  there are no pending arguments). `VArgV` ("everything in the frame") is relative to the
  binding's frame, not the function's.
- **`GrowStack` grows generously.** Every growth of the Unison stack unwinds the whole native
  call chain (records, `Push` frames, re-entries), and the interpreter's `ensure` grows by only
  1280 slots, which for "depth 1000000" meant 4219 unwinds. The trampoline now grows by at least
  the current size, which made it 13.
- **`Yield` with pending arguments still exits** (`ap /= fp`), also at re-entry, where `ap` is
  the caller's. Over-application through native frames is rare enough to leave for M4.
