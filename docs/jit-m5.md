# M5: compilation policy

Working plan for milestone M5 of the [implementation plan](jit-implementation-plan.md). Written
2026-09-30, before starting; the checkboxes are ticked as steps land. Each step is a commit.

**Goal.** The JIT becomes usable outside tests: `UNISON_JIT=on` compiles what gets hot, in the
background, while the program keeps running. Exit criteria (from the plan): with `on`, the
benchmarks reach at least 90% of their eager-mode speed after warm-up; starting `ucm` and running
a short program is no slower than with the JIT off; the test suites pass with `on` as well as
`eager`. Added while planning: with `on`, the code generated for a group is linear in its size,
because re-entry functions are generated only when they turn out to be used.

## What exists already

Every combinator has a [native code cell](jit-design.md#native-code-cells) from the moment it is
loaded, and the interpreter already counts calls made while the cell is null (`bumpNativeCount`,
in `enter` and `apply`). The compiler takes one group (a top-level definition with its local
functions and `Let` bodies) and produces one LLVM module, synchronously, on the thread that
loaded the code. The LLVM calls are `safe` foreign calls. `UNISON_JIT` knows `off` and `eager`;
`on` is parsed by the command line but not implemented. So M5 is mostly plumbing: a trigger, a
queue, a thread, and a batch.

Every [re-entry point](jit-design.md#re-entry-points) is generated eagerly today, together with
its function, and each one contains the whole rest of the function inline. Their number is
linear in the size of the function, but the total code is quadratic in the worst case. This has
been true of top-level `let` bodies since M2 (each body combinator is compiled as a full
function); M4 extended it to `let`s inside bindings and to call-out continuations.
`Duration.toText` in base is the pathological case: 254 LLVM functions, 6 s to compile. The
memoization added in M4 removed the exponential but not the quadratic.

## Order of work

- [x] **1. The `on` mode: trigger, queue, compile thread.** `Mode` gets `On`. A cell's count
  starts at minus the threshold N (`UNISON_JIT_THRESHOLD`, default 100) and `bumpNativeCount`
  says when it reaches zero; `enter` and `apply` then ask for the callee's definition to be
  compiled. A definition is never queued twice: the cell has a state (not requested, queued,
  taken by the compile thread), moved forward with a compare-and-swap, and a definition's
  state is the one on its entry combinator's cell. It is "queued" from the moment the request
  is queued and "taken" once the compile thread has put the definition in a batch, whether it
  then compiles or not. The queue is a bounded `TBQueue`;
  only a request the full queue refuses clears the flag again, and its count is set to ask
  again 1024 calls later. One compile
  thread, started with the runtime, takes requests, looks the group up in the code cache
  (`combs`, `combRefs`) and compiles it. LLVM is initialized lazily by that thread on its first
  request, so startup pays nothing. Eager mode is unchanged: synchronous, on the loading
  thread, counts start at zero and never trigger. Also here, because it is needed as soon as
  code is installed under a running program: the constant pool's old arrays get new entries
  too, and a function that uses a constant past the pool's first array checks the array's
  size at entry (stress mode `pool=N` makes the first array small). Checked: the test
  transcript passes with `UNISON_JIT=on UNISON_JIT_THRESHOLD=1` and with the default
  threshold; `UNISON_JIT_LOG=1` shows compiles happening while the program runs, each
  definition once.
- [x] **2. Re-entry functions on demand.** Reuses step 1's trigger and queue. Re-entry points
  fall into three classes:

  | Re-entry point | When it is used | Generated |
  | --- | --- | --- |
  | after a call-out with no native fast path | every time that code runs; these don't duplicate code, since each runs only to the next call-out | with the function, as before |
  | after a `let` (the body's combinator, or for a `let` inside a binding an auxiliary function) | only when something in the binding exits | when hot |
  | the slow path of an instruction with a native fast path (`Ref`, arrays, universal comparison) | only on a type or bounds miss | when hot |

  - *The compile driver works on units.* A unit is one LLVM function to generate: a
    combinator, or a re-entry function. A module is a list of units. A group's first compile
    has its entry point and local functions only.
  - *Pending units.* Everything left out has a cell and an entry in a table of pending units,
    keyed by cell: the group's `let` body combinators (put there when the group is compiled),
    and the re-entry functions the generator skipped (it hands back a `Deferred`: name,
    section, depth, frame base, cell). A pending unit's cell counts up from minus the
    threshold like any other.
  - *The trigger.* `yield`, when it pops a `Push` frame whose cell is empty, and the
    trampoline, when a call-out's continuation has no code, bump the cell's count; at zero the
    unit is looked up in the pending table and queued. The cell's request state keeps it
    from being queued twice. It is compiled as a module of its own.
  - *One table of cells per function.* A re-entry function generated later meets the same
    nested `let`s its parent met; it is given the parent's table of (section, depth, base) to
    cell, so both lead to the same re-entry functions rather than each generating its own.
  - *Eager mode keeps generating everything,* so the test matrix still exercises every
    re-entry point. For `on`, a threshold of 1 makes every re-entry point get generated on
    first use, so the lazy path is tested too.
  - The inline fast path is unchanged. A missing re-entry point costs at most the rest of one
    function body in the interpreter, since combinator bodies have no loops and every call
    checks cells.

  Checked: `Duration.toText`'s group compiles to one function, 398 KB of IR, in 183 ms on the
  `--fast` build, where it was 254 functions and 6 s. Running the benchmark transcript, `on`
  compiles 45 functions with 10 call-out continuations, generates 20 re-entry functions on
  demand and leaves 36 never asked for: 1.3 MB of IR and 0.77 s of compile time in all.
- [x] **3. Installing code under a running program.** Nothing in the generated code or the
  trampoline assumes a module is installed atomically: a function may find its callee's cell
  still null (it resumes), an exit may name an auxiliary function whose cell is not written
  yet (the trampoline interprets instead), and a `Let` body may be compiled while its binding
  runs (`yield` finds the cell and re-enters, which is the design's normal path). The stress
  mode `install=N` makes the compile thread sleep N ms before writing each function pointer
  of a module. Checked: the test transcript passes with `on`, `THRESHOLD=1`, `install=5`, and
  with `pool`, `callee`, `cstack`, `poll`, `ustack`, `alloc` on top; and on the debug runtime
  with heap sanity checks.
- [x] **4. Batching.** When a group becomes hot, the compile thread forms a batch:
  breadth-first from that group over the groups it calls (`combDeps` gives the group numbers)
  and the groups that call it (a reverse index the compile thread keeps, since the code cache
  doesn't have one), up to B groups (`UNISON_JIT_BATCH`, default 32). A group is taken if it
  isn't in a batch already and it is in use: its own request is waiting in the queue, or, for
  a callee, its entry combinator has been called at least N/2 times, or for a caller, once.
  Taking a group moves its state to "taken". The batch is one
  module: one list of units. The first version walked callees only, as this plan said, and
  formed one multi-group batch in the whole benchmark transcript; with callers, as the design
  had it, 16 of 55 modules span several definitions. See the learnings.
- [x] **5. Direct calls within a module.** A call to a combinator compiled in the same module is
  a direct `call`/`musttail call` to its LLVM symbol instead of a cell load and null test, and
  LLVM can inline it. The `callee` stress mode keeps going through cells so that path stays
  tested, and `UNISON_JIT_DISABLE=direct` turns direct calls off. Checked: `fib`'s recursive
  call is direct in the dumped IR (24 of the test transcript's 178 modules have direct calls);
  the benchmarks under `eager` are unchanged.
- [x] **6. Startup and statistics.** `UNISON_JIT_LOG` summarizes each module (functions, call-out
  continuations, re-entry functions left for later, exits, IR size, compile time);
  `UNISON_JIT_STATS` adds totals. A transcript with one small watch expression takes 1.16 to
  1.19 s with `off`, 1.17 s with `on`, 1.19 to 1.21 s with `eager` (optimized build, three runs
  each).
- [x] **7. Measure.** See "M5 measurements" in the [progress log](jit-progress.md): `on` equals
  `eager` on every benchmark, and `off` is where it was at M4.

## Decisions made while planning M5

- **The unit of compilation stays the group; a batch is a set of groups.** The generator and
  the tables already work per group, and a group is the natural unit of "called together". The
  design's "batch of supercombinators" becomes "batch of groups", which is the same thing at
  the granularity the code cache has.
- **The trigger is in the interpreter's call path, but costs one compare.** `enter` already
  bumps the counter; the count starts at minus N, so the test is "reached zero". (Comparing
  with N itself turned out to be measurable, see the learnings.) The enqueue happens once per
  group.
- **Dropping a request is fine, queueing one twice is not.** The queue is bounded; if it is
  full, the function is still interpreted, and its count is set to ask again 1024 calls
  later. The cell's state is set to "queued" before the request goes on the queue, so a definition is
  either not asked for, or pending or done, and only the first can be queued; the compile
  thread never sees a duplicate.
- **Hotness is any combinator's count** (changed while implementing; the plan said the entry
  combinator's). Local functions have their own cells and counts, and whichever combinator of
  a group reaches N first gets the group compiled. That covers a local loop that is hot while
  its top-level entry is called once (a `go` inside a one-shot `main`) with no extra rule. A
  group considered for a batch is judged by its entry combinator's count.
- **Re-entry functions are generated when hot, not with their function, and not never.** Whether
  a re-entry point is *reachable* is nearly always yes: any callee can exit at its entry poll,
  its stack check, or the C stack guard. Whether it is *used* is a dynamic property, so it is
  counted. And it can't simply be dropped: one rare exit deep in a call chain unwinds every
  native caller, and each of them needs its re-entry point when its callee returns.
  `depth 1000000` hits the same one about a million times, and any function above a frequent
  call-out hits its re-entry points on every call. Two alternatives were considered and
  rejected:
  - *One function with an entry-index switch.* Every `let` boundary on the fast path becomes a
    merge point, which hurts LLVM's optimization of the fast path, and the entry branch is
    shared between normal calls and re-entries.
  - *Static analysis of which re-entry points are reachable.* Nearly all are, for the reason
    above, so it would remove almost nothing.
- **LLVM is used from the compile thread only** (and from the loading thread in eager mode,
  which is a different process configuration). LLJIT allows concurrent use, but keeping it to
  one thread avoids thinking about it. Lookups happen on that thread too, right after
  compilation.
- **Direct in-module calls are part of M5, not M6.** Batching without them changes nothing
  that a benchmark can see. They were in the ideas document; this is where they pay off.

## Learnings and questions

- **Callees get hot before their callers, so a batch has to include callers.** A function is
  called at least as often as the function that calls it in a loop, so it reaches the
  threshold first; when its caller gets hot later, the callee is already compiled. The plan
  walked callees only (the design said dependents and dependencies; the plan narrowed it
  without saying so), and the benchmark transcript produced one multi-group batch. Walking
  callers too, with "called at least once" as the bar for a caller, gives 16 multi-group
  modules out of 55.
- **What batching costs and buys.** With callers the benchmark transcript compiles about 100
  functions instead of 50, 4.1 MB of IR instead of 1.4, 1.46 s instead of 0.74 s. The original
  eight benchmarks don't change speed: their hot loops are each inside one definition. So a
  ninth was added on 2026-10-01, "Calls across definitions" (three definitions: a loop over
  1 to 1000, a loop counting Collatz steps, and the step function, about 60000 calls). It
  takes 7.9 ms interpreted, 291 µs with `eager`, with `on` and `UNISON_JIT_BATCH=1`, or with
  direct calls disabled, and 267 µs with batching: 8% for twice the compile work. Whether
  that is a good trade is open; the knobs are the bar for callers, B, and whether a batch
  should stop at definitions that are small enough to inline. A caller that gets hot after
  its callee was compiled still calls it through the cell; the private-copies idea in the
  [ideas](jit-optimization-ideas.md) covers that.
- **The hottest callee was the one a batch missed.** The new benchmark showed it at once: the
  loop's request was queued, and while it waited the step function reached the threshold
  and queued its own, so when the batch was formed the step function was "already asked
  for" and was compiled alone, behind a cell. The flag became a state with three values (not
  requested, queued, taken by the compile thread): a batch takes a neighbour whose request is
  still queued, and that request is dropped when it comes off the queue.
- **Open: one run measured "Apply a function argument 10000 times" at 12 µs instead of 48.**
  Once, with `UNISON_JIT_BATCH=1`. Not reproduced and not explained; what `on` compiles
  depends on timing, so some ordering of compiles may produce better code for that loop.
- **The interpreter's counting has to be one compare with a constant.** The first version of
  the trigger compared the count with the threshold (a value read from the configuration),
  skipped the shared cell with another comparison, and added a count to `yield`. With the JIT
  off, the interpreter was 5 to 20% slower on the suite (fib 1.39 to 1.56 ms, list map 86 to
  102 µs). Counting up from minus the threshold, so that hot is "reached zero", brought it back
  to the M4 numbers. Lesson: re-measure `off` after touching `enter`, `apply` or `yield`.
- **"Pending" is a state, not a count.** The plan had the interpreter ask again whenever the
  count was past the threshold, with the compile thread dropping duplicates. Paul asked for a
  definition never to be queued twice, and for the state to live on the supercombinator
  rather than in a separate set. The cell now has a request state moved by compare-and-swap (first a flag, then three values, see above);
  the count only says when to ask the first time. Re-entry functions use the flag on their
  own cell (the pending table only says what to generate).
- **The constant pool needed care once code appears under running code.** A native run holds
  the pool array it was entered with. Code installed during the run may use a constant added
  after the array last grew. Entries are now written to the old arrays as well, and a function
  using an index past the first array's size checks the size at entry and exits to be entered
  again. Eager mode had the same hole in principle (a thread loading code while another runs).
- **Lazy generation by the numbers.** Benchmark transcript: `eager` compiles everything that
  is loaded, 524 functions with 494 auxiliary functions, 45.6 MB of IR, 17 s. `on` compiles 45
  functions and 30 auxiliary or re-entry functions, 1.3 MB, 0.77 s, and reaches the same
  speeds. About a third of the re-entry points that got a cell were ever asked for.
- **With every re-entry point in use, the total is still quadratic.** With `THRESHOLD=1` the
  benchmark transcript generates 175 re-entry functions and spends 8.5 s compiling, most of it
  on the `let` bodies of `Duration.toText` (each about 200 ms and 300 KB of IR). That is the
  case the plan accepted: they are generated because they are used. The fallback in the ideas
  document (the body as a tail call) is still there if real programs behave like this.
- **One function's IR is large.** `Duration.toText`'s single function is 398 KB of text with 37
  exits: every exit block writes the whole frame back, slot by slot. Sharing that code
  between exits is in the ideas document.
- **Open: call-out continuations are still generated with their function.** They don't
  duplicate code, and nothing measured so far says gating them by a counter is worth it.
- **Open: re-entry functions are compiled one per module.** A function with k hot re-entry
  points costs k small compiles. Queued requests for the same function could be compiled
  together.
- **Open: which combinator's count means hot.** Any combinator of a group reaching the
  threshold compiles the group, including a local loop inside a function called once. A
  batch member is judged by its entry combinator only.
