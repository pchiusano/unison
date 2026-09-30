# Unison JIT Implementation Plan

Sep 29, 2026 · Paul Chiusano

Companion to the [JIT design](jit-design.md). The design says what we're building and why. This document says how it gets built: which choices the implementation makes, in what order the work happens, and how we know each step is correct.

**How to review this.** Each choice that needs a decision is numbered (D1, D2, …) and gives a recommendation and the alternatives. The [sign-off checklist](#sign-off-checklist) at the end lists them all. Anything not marked as a decision follows from the design doc.

## Principles

1. **Remove the biggest unknowns first.** The riskiest parts are not code generation. They're linking xLVM, and doing things to the GHC runtime from native code (allocating, marking arrays, polling). Milestone 0 tests those in isolation, before any compiler is written.
2. **The interpreter is the specification.** Every milestone is tested by running the same programs with and without the JIT and comparing results.
3. **Always shippable.** The JIT is behind a build flag and a runtime switch, both off by default, until the last milestone. With it off, behaviour and performance must be unchanged.
4. **Grow the supported subset of MCode gradually.** Anything not yet supported is an exit to the interpreter, so each milestone produces something that runs real programs.

## Implementation choices

### Toolchain and build

**D1. Bind to LLVM through a small C shim of our own.** The shim lives in `unison-runtime/cbits`, uses LLVM's C API, and exposes about six functions to Haskell: create the JIT, add a module from IR text, look up a symbol, register an address under a name, fetch the last error, and dump diagnostics.

| Alternative | Why not |
| --- | --- |
| The `llvm-hs` Haskell bindings | They lag well behind current LLVM releases, and we need very little of the API. |
| Calling the LLVM C API function by function from Haskell | More foreign imports to maintain, for no benefit. |

**D2. Generate LLVM IR as text.** A Haskell module builds the IR with a small builder (fresh names, blocks, typed values) and prints it. LLVM parses it in memory. If parsing ever shows up in compile times, the builder can be retargeted to the C API without touching the code generator. There is no separate backend-neutral IR.

| Alternative | Why not |
| --- | --- |
| LLVM bitcode | Bitcode is the binary form of the same IR, and it's harder to produce, not easier. It's a bit-level stream with variable-width fields and its own abbreviation tables, and in practice it's only ever written by LLVM's own libraries. Text can be generated with string building, read by a person, and pasted into LLVM's command-line tools when debugging. |
| Building IR in memory through LLVM's C API | Many more foreign calls, and the IR can't be inspected without asking LLVM to print it. Kept as the fallback if parsing text is too slow. |

**D3. Pin one LLVM major version,** the current stable release when milestone 0 starts. Textual IR changes between major versions, so supporting a range isn't worth it.

**D4. Link LLVM dynamically at first, and statically before release.** Dynamic linking through `llvm-config` is enough for development and CI. Static linking is attempted in the last milestone. If it proves hard, we ship with dynamic linking and the JIT turns itself off when the library is missing.

**D5. A package flag, `jit`, off by default.** It's declared in `unison-runtime/package.yaml`, next to the existing `stackchecks` and `arraychecks` flags, and turned on with `stack build --flag unison-runtime:jit`. With the flag off, nothing links against LLVM and the JIT modules compile to stubs. Contributors who don't work on the JIT never need LLVM installed.

**D6. Build and test on macOS arm64 and Linux x86-64 first.** The JIT's own code should be portable to anywhere LLVM and GHC both run, and nothing in the code generator is written per platform. This decision is about where we build and run the tests during development, since a platform that isn't tested can't be claimed to work. The few places that do depend on the platform:

| What | Why it varies |
| --- | --- |
| Finding and linking LLVM | differs per operating system and package manager |
| Finding the thread's C stack bounds, for the stack guard (D13) | a different system call on macOS and Linux |
| Guaranteed tail calls | supported by LLVM on x86-64 and arm64; to be confirmed on each in M0 |

Linux on arm64 and macOS on x86-64 should follow with little work. Windows is out of scope for this plan's milestones, mainly because of linking.

### Runtime representation

**D7. Closure layouts are probed at startup, not hard-coded.** At startup, Haskell builds one sample of each closure the JIT reads or writes (`GData1`, `GPAp`, a type tag, and so on) and passes it to a C function, which reads the info pointer and works out field offsets from the info table. Generated code gets these as constants. A layout that isn't what the code generator can handle turns the JIT off with a logged message, so a GHC upgrade fails safely.

The result is a Haskell record, `Layout`, with one entry per closure type: its info pointer, its size, and the offset of each field. It's built once and kept with the rest of the JIT's global state. Only the code generator reads it. When it emits code to read or build a closure, it writes the offsets and the info pointer into the IR as constants. Native code therefore has no layout data to look up at run time.

**D8. A hotness counter is stored next to each native code cell.** The cell grows from 8 to 16 bytes: the function pointer, then a call counter. The interpreter already reads the cell on every call, so incrementing the counter touches the same cache line. Increments are not atomic. Losing a few counts under contention doesn't matter.

**D9. Runtime helpers are split by how hot they are.**

| Helper | Where it lives | Why |
| --- | --- | --- |
| Allocation fast path, frame record append, polls | LLVM IR, in a prelude included in every batch | so LLVM can inline them |
| Calls into the GHC runtime (`allocate` when a new block is needed), card marking on return, diagnostics | C, in `cbits`, registered with the JIT by name | they're off the fast path and need the runtime's headers |

**D10. `Ctx` is one C struct per Haskell thread that runs Unison code,** allocated with `malloc` when the thread first enters native code and freed when it finishes. Its layout is defined once, in a C header, and the Haskell side uses offsets generated from that header.

### Code generation

**D11. Within a function, Unison stack slots are local variables, not memory in the Unison stack.**

*The problem.* MCode is written against the Unison stack: "add slot 0 and slot 1, push the result". The obvious translation does exactly that, with a load from `ustk` and a store to `ustk` for every operation. But LLVM can't optimize those stores away. The Unison stack is memory that the interpreter can see, so as far as LLVM knows, every store might matter to someone. A loop counter would be written to memory and read back on every iteration.

*The approach.* At the start of each function, the generated code declares one local variable for each stack slot the function uses. In LLVM these are called `alloca`s. MCode operations read and write the local variables, and the real Unison stack is only touched at the boundaries:

| When | What's copied |
| --- | --- |
| On entry | the arguments, from the Unison stack into locals |
| Before a call that passes arguments on the Unison stack | the arguments, from locals to the Unison stack |
| On an exit to the interpreter | every slot that's in scope at that point, from locals to the Unison stack |
| On a normal return | the result, from a local to the Unison stack |

*Why this is fast.* Local variables that nothing else can see are what LLVM optimizes best. Its standard passes turn them into values held in registers, and then the usual optimizations apply. The loop counter lives in a register for the whole loop.

*Why this is easy.* The code generator stays simple. "Slot *i*" in MCode becomes "local variable *i*", and the generator never has to work out which values belong in registers. LLVM does that.

*Why exits still work.* The design's core rule is that the Unison stack must be correct whenever control returns to the interpreter. The copies on exit paths ensure that. They cost nothing on the normal path, since they're only executed when an exit happens.

This is in from the first milestone, because it's what makes numeric loops fast and it's cheaper to build in than to retrofit.

**D12. Between functions, arguments and results pass through the Unison stack until the last milestone.** D11 covers values inside one function. When one native function calls another, the arguments are still written to the Unison stack and read back by the callee. The design doc describes an optimization for calls within a batch: a worker function that takes its arguments in registers. That's deferred to [M6](#m6-performance-and-release-l), the final milestone, which is performance work chosen from measurements. It comes after everything is working.

**D13. The C stack guard compares the stack pointer against a limit in `Ctx`.** The trampoline sets the limit on entry from the thread's actual stack bounds, minus a fixed reserve for runtime helpers. Switching to a dedicated stack is deferred: we'll do it only if measurements show too much recursion falling back to the interpreter.

**D14. Optimization level: LLVM's standard `O2` pipeline.** Compilation is in the background, so compile time matters less than code quality. We log compile time per batch from the start, and revisit if it's a problem.

**D19. List operations are call-outs at first.** A Unison `List` is a Haskell `Data.Sequence`, a finger tree, stored as a foreign value. Native code can't work on one directly: the tree's internals are lazy, and native code can't evaluate a thunk.

*How list pattern matching compiles.* The existing compiler has already split it into two steps, and the JIT treats them differently:

| Step | In MCode | In native code |
| --- | --- | --- |
| Take the list apart | a primitive, such as `VWLS` for `h +: t`. It produces a small ordinary data value: "empty", or "element and rest". | a call-out: the interpreter runs the primitive |
| Branch on the result, and bind `h` and `t` | an ordinary match on that data value | native |

The other list primitives (`CONS`, `SNOC`, `IDXS`, `SIZS`, `CATS`, `SPLL`, `SPLR`, `VWRS`) are call-outs in the same way.

*What this costs.* A loop over a list makes at least one call-out per element, so code dominated by list operations will gain little from the JIT at first, and could be slower. Statistics on call-outs will show how much this matters for real programs.

*What can be done later,* as candidates for [M6](#m6-performance-and-release-l):

| Option | Idea | Cost |
| --- | --- | --- |
| Native fast paths | Most `viewl`, `cons` and `snoc` operations on a finger tree only touch the strict parts at its ends. Native code handles those cases and calls out for the rest. | ties the JIT to the internals of the `containers` library |
| A different list representation | replace `Data.Sequence` with a strict structure designed to be read by native code | a large change to the runtime, well beyond the JIT |

### Compilation policy

**D15. One background compilation thread, fed by a queue.** The interpreter enqueues a supercombinator when its counter reaches N. The compile thread forms a batch, generates IR, and calls LLVM through a `safe` foreign call, so other Haskell threads keep running. It then writes the function pointers into the cells.

**D16. An eager mode for testing.** With `UNISON_JIT=eager`, every supercombinator is compiled synchronously when loaded, with no counters and no background thread. All correctness testing uses this mode, because it's deterministic and it compiles code that would never get hot in a short test. There's a command-line option as well, `--jit eager`, which takes the same values as the environment variable and overrides it.

**D17. Batching starts simple.** Until milestone 5, a batch is one top-level definition with its local functions. Milestone 5 implements the combined strategy: walk breadth-first from the hot function through known calls, including only functions with at least N/2 calls, up to B functions. Defaults: N = 100, B = 32. Both can be overridden with the environment variables in D18. In eager mode neither applies: there are no counters, and each definition is compiled by itself when it's loaded.

### Configuration and diagnostics

**D18. Runtime settings are environment variables,** plus one command-line option, `--jit`, for the mode. The others can become command-line options once the feature is stable.

| Variable | Effect |
| --- | --- |
| `UNISON_JIT`, or `--jit` on the command line | `off` (default), `on`, or `eager` |
| `UNISON_JIT_THRESHOLD`, `UNISON_JIT_BATCH` | N (default 100) and B (default 32). Ignored in eager mode. |
| `UNISON_JIT_LOG` | log each batch: functions, compile time, code size |
| `UNISON_JIT_DUMP_IR` | write each batch's IR to a directory |
| `UNISON_JIT_STATS` | at exit, print counts per exit site and per call-out |
| `UNISON_JIT_STRESS` | see [Testing](#testing) |

## Code layout

New Haskell modules under `Unison.Runtime.JIT`:

| Module | Contents |
| --- | --- |
| `Types` | `Exit`, the exits table, the frame table, `Ctx` offsets |
| `Cells` | allocating cells, reading and installing function pointers, counters |
| `Layout` | the startup probe and the resulting closure layouts |
| `LLVM` | foreign imports for the C shim |
| `IR` | the IR text builder |
| `Codegen` | MCode to IR: sections, instructions, exits, re-entry points |
| `Codegen.Prims` | native implementations of `Prim1` and `Prim2` operations |
| `Trampoline` | entering native code, building `K` frames, acting on the status |
| `Compile` | the queue, batching, the background thread, eager mode |

New C files in `unison-runtime/cbits`: `jit_llvm.c` (the shim), `jit_rt.c` and `jit_rt.h` (runtime helpers and `Ctx`), `jit_probe.c` (layout probe).

Changes to existing code, kept as small as possible:

| File | Change |
| --- | --- |
| `MCode.hs` | new cell-address field in `GCombInfo`; the `Lam` pattern supplies null |
| `Machine.hs` | check the cell at `Call` and known `App`; check for a re-entry point in `yield`; attach cells during resolution |
| `MCode/Serialize.hs` | none expected, since fields are written one by one |
| `package.yaml` | the `jit` flag, C sources, LLVM link options |
| `unison-cli/src/ArgParse.hs` | the `--jit` option |

## Development process, IMPORTANT

Use `stack build --fast` when iterating to get things compiling.

Use `stack build --fast && stack exec unison -- -C jit_codebase transcript.fork transcripts/idempotent/jit-tests.md` to run a transcript with the various jit tests. You'll have to create the `jit-tests.md` file. You can pattern it after others in that directory.

Basically, the transcript is markdown where blocks can define Unison code, which will be evaluated. If the output doesn't match what is expected, the transcript will fail.

You can put benchmarks in the same transcript or in a separate one.

## Testing

Testing is the largest part of the work, since the failure mode of most bugs here is silent heap corruption.

**Differential testing.** The existing suites (`unison-src/tests`, `unison-src/builtin-tests`, the transcripts, the runtime unit tests) run a second time in CI with `UNISON_JIT=eager`. Results must be identical.

**Stress modes.** Most exit paths are rare in normal runs, so they need to be forced. `UNISON_JIT_STRESS` takes a list of:

| Mode | What it forces | What it tests |
| --- | --- | --- |
| `poll` | every Nth entry poll fires | `Reenter`, state written back at loop heads |
| `cstack` | a C stack limit of a few frames | frame records, re-entry points, deep recursion |
| `ustack` | the Unison stack starts tiny | `GrowStack` |
| `alloc` | an allocation budget of a few words | budget exits, GC running between native runs |
| `callee` | calls to compiled callees are treated as not compiled, at random | exits during calls, in both `Let` and tail position |

The differential suites run under each mode, and under all of them together.

**GC checking.** The stress runs are repeated with GHC's debug runtime and its heap sanity checks (`+RTS -DS`), and with a very small nursery so collections are frequent. This is what catches a missing card mark or a bad closure layout near its cause.

**Targeted tests.** These live in the `jit-tests.md` transcript described under [Development process](#development-process-important), so they run with one command while iterating. They're Unison programs, plus hand-written MCode in the runtime's unit tests where a case can't be reached from source, for cases the suites may not cover: continuations captured below several native frames and resumed twice, exceptions raised by a call-out, over- and under-saturated calls to function values, `killThread` on a thread in a native loop.

**Performance.** 

For every entry where no gain is expected, the requirement is that it doesn't get noticeably slower.

**Benchmarks to add.** These go in the `jit-tests.md` transcript, or in the suite if you'd prefer them there:

| Benchmark | Measures |
| --- | --- |
| A loop that sums up the numbers 0 to a million | loop and arithmetic performance |
| naive fibonacci | function call overhead and recursion | 
| Map and fold over a user-defined cons list, with a lambda | list manipulation, calls to function values, allocation, matching |
| Insert and lookup in a user-defined binary tree | allocation and matching |
| A loop that applies a function argument N times | calls to function values alone |
| Mutate a Ref 10000 times | loop that does mutation |

## Milestones

Sizes are relative: S is days, M is a week or two, L is several weeks. They're estimates for ordering, not commitments.

Before doing anything, I would write `jit-tests.md`, including the benchmarks, and get it compiling and running. You'll need to know a bit of Unison syntax, you can reference the other transcripts and check out https://www.unison-lang.org/docs/at-a-glance/. You can use `Clock.monotonic()` to get a `time.Duration`, you can subtract durations, and `printLine (Duration.toText d)` to print out a duration `d`.

### M0: spikes (M)

Three throwaway experiments, each a small standalone program.

| Spike | Question it answers |
| --- | --- |
| LLVM | Can a Stack-built Haskell program link LLVM, compile IR text, and call the result through an `unsafe` foreign call, on both target platforms? Does `musttail` work with the C calling convention on both? |
| Runtime | From C inside an `unsafe` call: can we allocate closures with `allocate` that Haskell then reads correctly? Store them into a `MutableArray` and mark it so they survive GC? Read the context-switch flag? Does the layout probe work? All under the debug runtime with a tiny nursery. |
| Overhead | What does the extra `GCombInfo` field and a cell check on every call cost the interpreter? Measured with the benchmark suite. |

**Exit criteria:** the first two questions answered yes, and interpreter slowdown under 2% across the benchmark suite. If the runtime spike fails, the design needs revisiting before going further.

### M1: skeleton and numeric loops (L)

Everything needed to run one compiled function end to end, with the smallest useful subset of MCode.

- Cells, `Ctx`, the exits table, the trampoline, the interpreter's check at `Call`.
- Eager mode and the environment variables.
- Code generation for: literals, unboxed `Prim1`/`Prim2` arithmetic and comparisons, `Match` on unboxed values, `Yield`, self tail calls, tail calls to known functions.
- Entry checks: Unison stack room (`GrowStack`) and the preemption poll (`Reenter`).
- Everything else is a `Resume` exit.
- Differential testing and the `poll` and `ustack` stress modes in CI.

**Exit criteria:** the suites pass in eager mode. "Count to 1 million" is at least 10× faster than the interpreter. A native infinite loop can be killed with `killThread`.

### M2: calls and frames (L)

- Non-tail native calls with the status check.
- Frame records, the frame table, building `K` frames in the trampoline.
- Re-entry points for `Let`, and the check in `yield`.
- The C stack guard.
- Stress modes `cstack` and `callee`.

**Exit criteria:** the suites pass under all stress modes so far. Recursion one million deep runs without overflowing the C stack. The continuation tests pass. "fib1" is at least 5× faster.

### M3: data (L)

- The constant pool.
- Allocation: `Pack`, boxed results, the allocation budget.
- Reading data: `Unpack`, `DMatch`, `NMatch`, tag tests.
- Marking `bstk` on return to Haskell.
- Stress mode `alloc`, and the GC checking runs.

**Exit criteria:** the suites pass under all stress modes with heap sanity checking on. A native loop that allocates heavily runs in bounded memory. "NatMap.fromList" and the user-defined binary tree benchmark are at least 3× faster.

### M4: call-outs and function values (M)

- `CallOut` exits and their re-entry points, for `ForeignCall` and uncompiled primitives, including the exception case.
- Calls to function values: exactly saturated `GPAp`.
- Native array read, write and size, and `Ref.read` and `Ref.write`.

**Exit criteria:** map and fold over a user-defined cons list are at least 3× faster. The array entries are at least 3× faster. No entry in the suite is slower than with the interpreter, including the list entries.

### M5: compilation policy (M)

- Counters, the threshold, the compile queue and background thread.
- Batching across definitions.
- Logging and statistics.

**Exit criteria:** with `UNISON_JIT=on`, the benchmarks reach at least 90% of their eager-mode speed after warm-up. Starting `ucm` and running a short program is no slower than with the JIT off. The suites pass with `on` as well as `eager`.

### M6: performance and release (L)

Driven by measurements from M5, so the list is provisional.

- Worker functions with register arguments for calls within a batch.
- Native versions of whichever builtins the call-out statistics show to be hot. List operations are the likely first candidate (D19).
- Under-saturated calls to function values.
- Static linking of LLVM.
- The remaining Unix platforms.
- Documentation, and the decision on whether to turn the JIT on by default.

## Risks

| Risk | Likelihood | Response |
| --- | --- | --- |
| The GHC runtime can't safely be used the way the design assumes | low, but fatal | M0's runtime spike tests this first |
| Linking LLVM is painful in Stack, Nix and CI | medium | the `jit` flag keeps it away from everyone else; dynamic linking first |
| The interpreter gets slower for everyone | medium | measured in M0; if over 2%, guard the checks behind the build flag |
| Heap corruption bugs that are hard to reproduce | high | stress modes and sanity checking from M1, never deferred |
| Exits are so frequent that real programs don't speed up | medium | per-site statistics from M1; M4 targets the two known causes |
| List-heavy code doesn't speed up, and most Unison code uses lists | high | D19: measure call-outs, then native fast paths in M6 |
| A GHC upgrade changes closure layouts or runtime internals | certain, eventually | the startup probe turns the JIT off safely; the M0 runtime spike becomes a regression test |

## Out of scope

As in the design: compiling handlers and continuations natively, saving compiled code to disk, and specialization of higher-order functions. Also Windows, until after M6.

## Sign-off checklist

- [x] D1. LLVM binding through our own C shim
- [x] D2. IR generated as text, not bitcode; no intermediate IR
- [x] D3. One pinned LLVM version
- [x] D4. Dynamic linking first, static before release
- [x] D5. Package flag `jit`, off by default
- [x] D6. Build and test on macOS arm64 and Linux x86-64 first; Windows out of scope
- [x] D7. Closure layouts probed at startup
- [x] D8. Hotness counter stored next to the cell (16-byte cells)
- [x] D9. Hot helpers in an IR prelude, cold helpers in C
- [x] D10. `Ctx` as a C struct per Haskell thread
- [x] D11. Stack slots as local variables within a function, from the first milestone
- [x] D12. Register-argument workers deferred to M6
- [x] D13. C stack guard by stack pointer comparison; dedicated stack deferred
- [x] D14. LLVM `O2`
- [x] D15. One background compile thread
- [x] D16. Eager mode for testing, with a `--jit` command-line option
- [x] D17. Simple batching until M5; defaults N = 100, B = 32
- [x] D18. Environment variables for configuration, plus `--jit`
- [x] D19. List operations as call-outs at first
- [x] Milestone order and exit criteria
- [x] Testing approach: differential, stress modes, GC checking
