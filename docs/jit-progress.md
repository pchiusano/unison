# Unison JIT: progress log

Working notes for whoever picks this up next, human or Claude. Read this first, then the
[design](jit-design.md) and the [implementation plan](jit-implementation-plan.md).
Update it whenever a step finishes or something non-obvious is learned.

## Status

| Step | State |
| --- | --- |
| Design doc | done, signed off |
| Implementation plan | done, all decisions signed off (2026-09-29) |
| Pre-M0: `jit-tests.md` transcript with benchmarks | done |
| M0: spikes | done |
| M1: skeleton and numeric loops | done 2026-09-30. Test matrix passes; "Sum 0 to 1 million" 104× faster; everything else exits and is the same or slower until M2-M4 |
| M2: calls and frames | done 2026-09-30. Test matrix (7 configurations) passes; "fib 20" 16× faster; "depth 1000000" runs natively with the C stack guard. Data and list code is slower until M3/M4. See [jit-m2.md](jit-m2.md) |
| M3: data | done 2026-09-30. Constructors are built and matched natively (all arities), booleans stay in registers; heap sanity checks pass; tree benchmark 6× faster. List/function-value/Ref code is slower until M4. See [jit-m3.md](jit-m3.md) |
| M4: call-outs and function values | done 2026-09-30. Call-outs with native re-entry (also inside inline bindings), native closure calls, `Ref` and mutable arrays, universal comparison on unboxed values, combinators as constants. Whole suite faster than the interpreter (4× to 210×); debug-runtime checks pass. See [jit-m4.md](jit-m4.md) |
| M6: real programs | done 2026-10-02 ([jit-m6.md](jit-m6.md) has every step with what was done). Part 1: no `suite` entry more than 10% slower in steady state. Workers: `fib 20` 41 µs. Lists are `Unison.Util.Deque` now, with native list, text and partial-application operations: `List.map` 0.43×, `List.foldLeft` 0.30×, `List.at` 0.20× of the interpreter, the text benchmarks 2.7× and 3.3× faster. Left for later: `Bytes`, the list repair cases, tuning the deque |
| M5: compilation policy | done 2026-09-30. `UNISON_JIT=on` compiles what gets hot on a background thread, generates re-entry functions on demand, and matches eager mode's speed on the whole suite with 1/35 of the IR; the interpreter with the JIT off is unchanged. See [jit-m5.md](jit-m5.md) |
| M0 spike 1: LLVM | done on macOS arm64. Linux skipped for now. |
| M0 spike 2: GHC runtime from C | done on macOS arm64 |
| M0 spike 3: interpreter overhead | done: under 2%, cell field and check kept |

## How to build and run

- Iterating on compile errors: `stack build --fast`
- **Benchmarking: `stack build` (optimized). Never benchmark a `--fast` build.**
- Run the JIT transcript, from the repo root:
  `stack exec unison -- -C jit_codebase transcript.fork unison-src/transcripts/idempotent/jit-tests.md`
- `transcript.fork` runs on a throwaway copy of the codebase, so `jit_codebase` itself is never modified.
- **Stack does not rebuild when only the optimization level changes.** After any `stack build --fast`,
  a plain `stack build` compiles nothing and leaves the unoptimized binary in place. So the two
  builds live in separate work dirs (since 2026-09-30): `.stack-work` is the `--fast` tree and
  `.stack-work-opt` the optimized one. Build and run the optimized binary with
  `stack build --work-dir .stack-work-opt --flag unison-runtime:jit` and
  `stack exec --work-dir .stack-work-opt unison -- ...`. Dependencies are shared between the
  trees, only local packages are built twice, and switching rebuilds nothing. Don't run two
  Stack builds at once. Check by running the benchmarks: on an optimized build
  "Count to 1 million" in `suite` takes about 40 ms on this machine class; on a `--fast` build, about 3 s.
- `stack.yaml` builds everything with `-fno-omit-yields`, so compiled Haskell loops that don't allocate
  can still be preempted. Native code gets no such help, which is why the design polls.
- GHC 9.10.3, Stack resolver lts-24.38. Stack prints warnings about untested GHC/Cabal versions; they're harmless.

## Building with the JIT

- The JIT is behind the package flag `jit`. Every build command for JIT work needs
  `--flag unison-runtime:jit`, otherwise Stack reconfigures the package without it (and rebuilds).
- Iterating: `stack build --fast --flag unison-runtime:jit unison-runtime` (compile errors fast),
  then `stack build --flag unison-runtime:jit` to relink the executable.
- Running with the JIT: `UNISON_JIT=on` (compile what gets hot, in the background; the mode
  meant for use) or `UNISON_JIT=eager` (compile everything as it is loaded; for testing), or
  `unison --jit on|eager`. `UNISON_JIT_THRESHOLD=N` (default 100) is the number of interpreted
  calls before a definition, or a re-entry point, is compiled; `UNISON_JIT_BATCH=B` (default
  32) the most definitions compiled together. `UNISON_JIT_EXIT_COST=E` (default 7) is the
  cost of an exit in the rule that leaves exit-heavy functions interpreted (0 compiles
  everything; see "What isn't compiled" in the design). `UNISON_JIT_ENTRY_COST=C` (default
  3): a compiled function that saves less than this per call is still interpreted when the
  interpreter is the caller (0 always enters native code). Diagnostics: `UNISON_JIT_LOG=1`
  (compile log, including why a combinator or part of one was left to the interpreter),
  `UNISON_JIT_TRACE=1` (every native entry and exit, on both the C and Haskell sides; unusable
  on programs that load base, it exhausts memory during the load), `UNISON_JIT_DUMP_MCODE=1`,
  `UNISON_JIT_STATS=1` (exit counts after each evaluation), `UNISON_JIT_STATS=each` (the
  exits taken since the program last wrote output, printed after each write: per-benchmark
  exits for a suite that prints a line per benchmark), `UNISON_JIT_STATS_EVERY=N` (with
  stats: also every N exits, for evaluations that never finish), `UNISON_JIT_DISABLE=a,b,...`
  (turn features off for bisecting a bug: `app`, `apply`, `ref`, `array`, `cmp`, `callout`,
  `direct`, `worker`, `list`, `text`, `name`). With stats, the first line is the compile totals: modules, functions, auxiliary
  functions, re-entry functions generated on demand and never asked for, IR size, time.
- Seeing the generated code: `--jit-dump-ir DIR` (or `UNISON_JIT_DUMP_IR=DIR`) writes one `.ll`
  file per definition into `DIR`, with the MCode of each function as a comment above its IR and a
  line for each combinator that wasn't compiled and why; `--jit-dump-ir -` prints to stderr. Two
  versions of each module are written: `unison_<n>.ll` is the text handed to LLVM, and
  `unison_<n>.opt.ll` is the module after the `default<O2>` pipeline, which is what runs. To see
  the arm64 assembly: `/opt/homebrew/opt/llvm/bin/llc -O2 unison_<n>.opt.ll -o out.s`. For example:
  `stack exec unison -- --jit eager --jit-dump-ir - -C jit_codebase transcript.fork t.md 2> t.ll`.
- `unison-runtime.cabal` was regenerated by Stack's hpack 0.37.0 (the committed one said 0.38.1),
  so the version line in it changed. That's expected.
- Stack.hs and MCode.hs carry `{-# OPTIONS_GHC -O2 -funbox-strict-fields #-}` under `#ifdef UNISON_JIT`,
  because the closure layouts the code generator relies on only exist in optimized builds; without
  it a `--fast` build makes the layout probe (correctly) turn the JIT off.
- A build can hang at "Configuring unison-runtime" if macOS is waiting for the user to approve
  running a binary that Cabal's configure step invokes (`pkg-config --version` was the case on
  2026-09-30). The process sits in an unkillable state until the prompt is answered. Answer the
  prompt; nothing in the project is wrong.
- **Heap sanity checking** (GC-checked runs): a third work dir holds a `--fast` build linked
  against the debug RTS, built once with
  `stack build --fast --flag unison-runtime:jit --ghc-options=-debug --work-dir .stack-work-debug`
  (rebuilds are incremental like any other tree). `stack exec --work-dir .stack-work-debug unison -- +RTS --info`
  says `rts_thr_debug`. Run transcripts through it with `+RTS -DS -RTS` at the end of the
  command line: the RTS checks every heap object at every GC, which catches a bad info pointer,
  tag, or a missed write barrier. About 10× slower. (Until 2026-09-30 this was done by editing
  `unison-cli-main/package.yaml`; the separate tree needs no edits.)
- **Never run two Stack builds at once, and never leave a killed one behind.** A build killed by a
  timeout leaves `Cabal-simple` and `ghc` processes holding the build lock, and later builds hang
  silently on it ("still blocking for directory lock"). Check with `pgrep -fl "stack build|Cabal-simple"`
  and kill them, then `stack clean unison-runtime`.

## Test and benchmark transcripts

| File | What it is | Codebase it needs |
| --- | --- | --- |
| `unison-src/transcripts/idempotent/jit-tests.md` | correctness tests with known answers. Builtins only, prompt `scratch/main>`. The file contains its own expected output. | any, including empty |
| `unison-src/transcripts-manual/jit-benchmarks.md` | `jitSuite`, the benchmarks written for the JIT. Prompt `jit-tests/main>`. Timings print to the console. | `jit_codebase` |
| `unison-src/transcripts-manual/jit-suite.md` | the older, broader `suite` (lists, maps, text, JSON, abilities). About 5.5 minutes per run. The test of whether the JIT is usable on library code (M6). | `jit_codebase` |

- The JIT test matrix: run `jit-tests.md` with `UNISON_JIT=off`, `UNISON_JIT=eager`, and
  `UNISON_JIT=eager` with `UNISON_JIT_STRESS=` each of `poll=3`, `ustack=4`, `cstack=2048`,
  `callee=3`, and all four together (`callee=2,cstack=4096,poll=5,ustack=4`); the output must be
  identical each time. All passed on 2026-09-30 after M2 step 6 (about 2 minutes each).
  `UNISON_JIT_STATS=1` prints exit counts after every evaluation; the counts for "depth 1000000"
  are the quickest way to see whether frames are unwinding more than they should.
- Since M5 the matrix also has `UNISON_JIT=on` with the default threshold, with
  `UNISON_JIT_THRESHOLD=1` (every re-entry point is generated on first use, which tests the lazy
  path), and `on` with `THRESHOLD=1` and `UNISON_JIT_STRESS=install=5` alone and together with
  `pool=8,callee=2,cstack=4096,poll=5,ustack=4,alloc=64`. `install=N` sleeps N ms before each
  function of a module is installed; `pool=N` makes the constant pool's first array N entries,
  so that it grows and functions check its size. Eager mode gets `pool=8` in its combined run.
  All passed on 2026-09-30. On the debug runtime with `-DS`, these passed with no assertion
  failures: the tests with `on` and `THRESHOLD=1`, with `on`, `THRESHOLD=2` and
  `install=5,pool=8,alloc=64,poll=7`, and with `eager`; and the benchmark transcript with `on`
  and `THRESHOLD=1`.
  With the `--fast` binary, `off` takes about 160 s and `on` or `eager` 10 to 25 s, because the
  tests are loops.
- What `on` mode compiles depends on timing (the compile thread races the program), so two runs
  don't exercise exactly the same paths. `THRESHOLD=1` is the most repeatable.
- Running a transcript writes `<name>.output.md` next to it. For the idempotent one, copy the output
  over the `.md` when the change is intended. Delete stray `.output.md` files before committing.
- The benchmark transcript takes about a minute. In its console output the first benchmark's label
  ("Sum 0 to 1 million") is overwritten by the transcript's progress line, so the first timing appears
  without a label.
- Unison gotchas found while writing the tests:
  - With builtins only there is no `Nat.-`, `Nat.==`, `Nat.<` and so on. The test transcript defines
    them in a prelude stanza, followed by `update`.
  - Bare `==` and `<` resolve to the polymorphic `Universal` versions unless the file starts with
    `use Nat + - * / == < > <= >=`.
  - Definitions in one `unison` stanza are not visible in the next unless a `ucm` stanza runs `update`.
    Put watch expressions (`> expr`) in the same stanza as the definitions they use.

## M0 spike results

### Spike 1: LLVM (`jit-spikes/llvm/`)

Run with `jit-spikes/llvm/run.sh`. It builds with `stack ghc`, outside the Stack project, and links
LLVM dynamically through `llvm-config`. Result on macOS arm64, LLVM 23.1.2, 2026-09-29: all pass.

| Question | Answer |
| --- | --- |
| Can a Haskell program built with the project's GHC link LLVM and call it? | yes |
| Can it compile IR text and call the result through an `unsafe` foreign call? | yes |
| Do allocas become registers at O2 (decision D11)? | yes: 100 million loop iterations in 74 ms, 0.7 ns each. The interpreter's comparable loop is about 69 ns per iteration. |
| Does `musttail` work with the C calling convention and the uniform signature? | yes: 100 million tail calls between two functions, through function pointers loaded from cells, in 31 ms and without growing the C stack |
| Can a cell's address be a constant in the IR? | yes, with `inttoptr` |
| Can generated code call a C helper by name? | yes, by registering the address with `LLVMOrcAbsoluteSymbols` |
| Are errors in the IR reported without crashing? | yes, with line and column |
| Compile time | about 9 ms to parse and optimize a one-function module, and 8 ms to generate code, on first use. Not measured for larger modules. |

Things learned about the LLVM 23 C API:

- There is no function to get the `LLVMContext` out of a thread-safe context any more. Create the
  context with `LLVMContextCreate` and wrap it with `LLVMOrcCreateNewThreadSafeContextFromLLVMContext`.
- Code is generated lazily, at the first `LLVMOrcLLJITLookup` of a symbol in the module, not when the
  module is added.
- Optimization is a separate step, `LLVMRunPasses(module, "default<O2>", targetMachine, options)`,
  run before the module is handed to the JIT.
- The shim needed six functions, as D1 predicted: init, add module, lookup, define symbol, last error,
  target triple.

Not done: the same spike on Linux x86-64. Paul decided to skip Linux for now (2026-09-29).

### Spike 2: the GHC runtime from C (`jit-spikes/runtime/`)

Run with `jit-spikes/runtime/run.sh` (normal runtime), `run.sh debug` (debug runtime, heap sanity
checks on every GC, 64 KB nursery) or `run.sh nomark` (negative test). It links the real
`unison-runtime` package, so the project must be built first. Result on macOS arm64, GHC 9.10.3,
2026-09-29: all pass, in both normal and debug mode.

| Question | Answer |
| --- | --- |
| Does the layout probe work on the real `Closure` types? | yes. It finds the info pointer, pointer tag and the offset of every field of `Enum`, `Data1` and `Data2` by building a sample with recognizable values. |
| Can C allocate closures with `allocate` that Haskell reads correctly? | yes. C built a 2 million cell list of `Data2` closures over 3419 separate unsafe calls, with GCs in between. Haskell pattern matching read back every cell. |
| Is marking the array's header and cards enough for the GC? | yes. With marking, the debug runtime's sanity checks pass. Without it (`run.sh nomark`) the sanity check aborts at `rts/sm/Sanity.c` line 530. So the test detects the bug it's meant to, and adding the array to the mutable list is not needed. |
| Can marking be done once per return to Haskell? | yes. The spike stores with no barrier and marks once before returning. |
| Does memory stay bounded when native code allocates with a budget? | yes. 5.5 GB allocated as garbage in 4096-word budgets; peak memory in use 230 MB, most of which is the list from the previous test. |
| Can native code see the runtime's request to stop? | yes, by reading `rHpLim` from the register table and comparing with null. A spinning loop saw it within 30 ms every time (normal runtime), which matches the runtime's 20 ms timeslice plus a 10 ms tick. |

Layout facts found by the probe (GHC 9.10.3, arm64). These are what D7's probe will compute at
startup, recorded here for reference, not to be hard-coded:

| Closure | Pointer tag | Payload, in order |
| --- | --- | --- |
| `Enum ref tag` | 2 | ref pointer; tag word |
| `Data1 ref tag (Val u b)` | 3 | ref pointer, b pointer; tag word, u word |
| `Data2 ref tag (Val u1 b1) (Val u2 b2)` | 4 | ref, b1, b2 pointers; tag, u1, u2 words |
| unboxed type tag closure | 7 | one pointer |

- Pointer fields come first, then the other fields, each group in declaration order.
- The pointer tag is the constructor's position in `GClosure` plus one (`GPAp` is 1), capped at 7.
  Tag 7 means "read the constructor number from the info table".
- A boxed array reaches C as a pointer to its first element. Its header is
  `sizeofW(StgMutArrPtrs)` words before that.
- `Capability` is opaque in the public headers, but it starts with a `StgFunTable` followed by the
  `StgRegTable`, both public. So `rHpLim` is at `(char *)cap + sizeof(StgFunTable)`, in the register table.
- `rts_unsafeGetMyCapability()` and `allocate()` are both in the public headers, via `Rts.h`.

Not covered by this spike, to check when they come up in M3 and M4:

- Carving several objects out of one `allocate` call.
- Objects large enough to take the runtime's large-object path.
- The write barrier for `MutVar` (`Ref.write`), which is different from the one for arrays.

### Spike 3: interpreter overhead of the native code cell

The `GCombInfo` field and the check in `enter` (read the cell's code pointer, branch on null, bump
the counter) are in the code for good; see commit history. Measured with the optimized build, three
runs each, 2026-09-29. Baseline numbers are from before the change.

| Benchmark | Baseline | With cells | Change |
| --- | --- | --- | --- |
| Sum 0 to 1 million | 65.9 to 68.7 ms | 67.5 to 68.6 ms | none measurable |
| fib 20 | 1.33 to 1.37 ms | 1.35 ms | none measurable |
| Cons list: map with a lambda | 81 to 84 µs | 82 to 83 µs | none measurable |
| Cons list: foldLeft with a lambda | 84 to 87 µs | 86 to 88 µs | about +1% |
| Binary tree: 1000 inserts | 1.66 ms | 1.65 to 1.69 ms | none measurable |
| Binary tree: 1000 lookups | 760 to 775 µs | 766 µs | none measurable |
| Apply a function argument 10000 times | 1.13 ms | 1.15 ms | about +1.5% |
| Mutate a Ref 10000 times | 839 to 850 µs | 840 to 859 µs | about +1% |

All within the 2% the plan allows. The correctness transcript still passes, so cells are attached
without breaking code loading.

**Stack rule, refined:** after building any package with `--fast`, run `stack clean <package>`
before the optimized `stack build`, or that package stays unoptimized. (Superseded 2026-09-30 by
the separate `.stack-work-opt` tree, see "How to build and run".) Rebuilding just
`unison-runtime` and relinking takes a few minutes; a full clean build takes about 15.

## M1 measurements

2026-09-30, optimized build with the `jit` flag, `jitSuite`, one run each. Steps 1-3 of M1 only:
no non-tail native calls, no allocation, no call-outs, so anything but a tight loop exits.

| Benchmark | `UNISON_JIT=off` | `UNISON_JIT=eager` | Change |
| --- | --- | --- | --- |
| Sum 0 to 1 million | 66.5 ms | 0.64 ms | 104× faster (M1 target was 10×) |
| fib 20 | 1.35 ms | 1.82 ms | 35% slower: every call exits at the `Let` (M2) |
| Cons list: map with a lambda | 82 µs | 156 µs | slower: allocation and matching exit (M3) |
| Cons list: foldLeft with a lambda | 85 µs | 159 µs | slower: same |
| Binary tree: 1000 inserts | 1.65 ms | 1.69 ms | same |
| Binary tree: 1000 lookups | 769 µs | 797 µs | same |
| Apply a function argument 10000 times | 1.15 ms | 1.38 ms | slower: unknown calls exit (M4) |
| Mutate a Ref 10000 times | 840 µs | 1.21 ms | slower: `Ref` ops are foreign calls (M4) |

The slowdowns are the cost of an exit followed by interpretation of the rest of the function, as
the design predicts; they are what M2 to M4 remove.

## M2 measurements

2026-09-30, optimized build with the `jit` flag, `jitSuite`, one run each. Non-tail calls,
frame records, re-entry and inline `Let` bindings are in; allocation, data matching with fields,
call-outs and calls to function values still exit.

| Benchmark | `UNISON_JIT=off` | `UNISON_JIT=eager` | Change |
| --- | --- | --- | --- |
| Sum 0 to 1 million | 67.4 ms | 0.64 ms | 105× faster |
| fib 20 | 1.37 ms | 86 µs | 16× faster (M2 target was 5×) |
| Cons list: map with a lambda | 85 µs | 183 µs | slower: `Pack`, `DMatch` with fields, calls to function values exit (M3, M4) |
| Cons list: foldLeft with a lambda | 85 µs | 120 µs | slower: same |
| Binary tree: 1000 inserts | 1.65 ms | 2.44 ms | slower: more of the code now compiles and exits deeper in (M3) |
| Binary tree: 1000 lookups | 760 µs | 1.59 ms | slower: `DMatch` on a constructor with fields exits (M3) |
| Apply a function argument 10000 times | 1.16 ms | 1.17 ms | same |
| Mutate a Ref 10000 times | 835 µs | 1.32 ms | slower: `Ref` ops are foreign calls (M4) |

The tree and list entries got slower than in M1 because functions that used to be refused
(they started with a `Let`) now compile, run natively until the first `Pack` or field match,
and exit there; each exit and re-entry costs more than interpreting the whole function. M3
removes those exits.

## M3 measurements

2026-09-30, optimized build with the `jit` flag, `jitSuite`, best of three runs. The machine was
busy with a system process during these runs, so the absolute numbers are 1.5-2× worse than
the M2 ones (the interpreter's "Sum 0 to 1 million" went from 67 ms to 106 ms); compare within
the row, and re-measure on an idle machine before quoting.

| Benchmark | `UNISON_JIT=off` | `UNISON_JIT=eager` | Change |
| --- | --- | --- | --- |
| Sum 0 to 1 million | 106 ms | 0.53 ms | 200× faster (inflated by the noisy baseline) |
| fib 20 | 2.20 ms | 145 µs | 15× faster |
| Cons list: map with a lambda | 153 µs | 409 µs | slower: the call to `f` is an `App` to a function value (M4) |
| Cons list: foldLeft with a lambda | 169 µs | 222 µs | slower: same |
| Binary tree: 1000 inserts | 3.47 ms | 547 µs | 6× faster (M3 target was 3×) |
| Binary tree: 1000 lookups | 1.52 ms | 139 µs | 11× faster |
| Apply a function argument 10000 times | 2.20 ms | 2.90 ms | slower: unknown calls exit (M4) |
| Mutate a Ref 10000 times | 1.84 ms | 3.09 ms | slower: `Ref` ops are foreign calls (M4) |

Against the M2 interpreter numbers (measured on a quiet machine) the tree entries are 3× and
5.5× faster, so the exit criterion holds either way. What's left slow is exactly M4's list:
calls to function values and foreign calls.

## M4 measurements

2026-09-30, optimized build (`.stack-work-opt`) with the `jit` flag, `jitSuite`, idle machine,
best of three runs. Commit 298123434 plus the combinator-as-value constant.

| Benchmark | `UNISON_JIT=off` | `UNISON_JIT=eager` | Change |
| --- | --- | --- | --- |
| Sum 0 to 1 million | 67.0 ms | 320 µs | 210× faster |
| fib 20 | 1.39 ms | 87 µs | 16× faster |
| Cons list: map with a lambda | 85 µs | 21.5 µs | 4× faster (M4 target was 3×) |
| Cons list: foldLeft with a lambda | 86 µs | 5.0 µs | 17× faster |
| Binary tree: 1000 inserts | 1.67 ms | 254 µs | 6.6× faster |
| Binary tree: 1000 lookups | 769 µs | 67 µs | 11× faster |
| Apply a function argument 10000 times | 1.17 ms | 49 µs | 24× faster |
| Mutate a Ref 10000 times | 950 µs | 59 µs | 16× faster |

Nothing in the suite is slower than the interpreter any more. The exits left in the suite are
`Ref.new` (a call-out per `refLoop` call), the preemption polls, and `CAST` in base's time
functions; the loops themselves run without leaving native code. What the suite doesn't
exercise: partial application with captured arguments (`Name`, a call-out), ability handler
calls (`App (Dyn i)`, a resume), and over-application.

## M5 measurements

2026-09-30, optimized build (`.stack-work-opt`) with the `jit` flag, `jitSuite`, best of three
runs (two for `off`).

| Benchmark | `UNISON_JIT=off` | `UNISON_JIT=on` | `UNISON_JIT=eager` |
| --- | --- | --- | --- |
| Sum 0 to 1 million | 66.4 ms | 313 µs | 314 µs |
| fib 20 | 1.39 ms | 88.6 µs | 88.6 µs |
| Cons list: map with a lambda | 86.3 µs | 21.2 µs | 20.6 µs |
| Cons list: foldLeft with a lambda | 86.0 µs | 5.01 µs | 5.01 µs |
| Binary tree: 1000 inserts | 1.67 ms | 248 µs | 252 µs |
| Binary tree: 1000 lookups | 763 µs | 66.2 µs | 64.0 µs |
| Apply a function argument 10000 times | 1.16 ms | 48.0 µs | 48.1 µs |
| Mutate a Ref 10000 times | 828 µs | 59.8 µs | 60.3 µs |

- `on` is within 3% of `eager` everywhere (the exit criterion was 90%), and `off` is where it
  was at M4.
- What was compiled during the benchmark transcript, with batches over callees only (the
  table above): `on`, 65 modules, 45 functions, 10 call-out continuations, 20 re-entry
  functions on demand (36 more never asked for), 1.3 MB of IR, 0.77 s of compile time. `eager`,
  193 modules, 524 functions, 494 auxiliary functions, 45.6 MB of IR, 17 s.
- 2026-10-01, batches over callers as well (and the "requested" flag in the cell, which is now
  24 bytes): `on` compiles 58 modules, 99 functions, 44 call-out continuations, 26 re-entry
  functions on demand, 4.0 MB of IR, 1.4 s. Speeds are the same as in the table (`on`: 314 µs,
  88.5 µs, 21.0 µs, 5.01 µs, 248 µs, 63.0 µs, 48.1 µs, 59.6 µs; `off`: 66.0 ms, 1.38 ms,
  85.2 µs, 85.1 µs, 1.66 ms, 760 µs, 1.15 ms, 827 µs). Those eight have no hot loop that calls
  across definitions, so they only show what batching costs.
- 2026-10-01, a ninth benchmark, "Calls across definitions: Collatz steps for 1 to 1000" (a
  loop calling a loop calling a small function, three definitions, about 60000 calls):

  | `off` | `on` | `on`, `UNISON_JIT_BATCH=1` | `on`, `UNISON_JIT_DISABLE=direct` | `eager` |
  | --- | --- | --- | --- | --- |
  | 7.85 ms | 267 µs | 290 µs | 286 µs | 291 µs |

  Batching with direct calls is worth 8% here. What `on` compiled in that transcript: with
  batching 49 modules, 106 functions, 4.1 MB of IR, 1.46 s; with `BATCH=1` 73 modules, 52
  functions, 1.4 MB, 0.74 s.
- The whole benchmark transcript takes 62 to 64 s with `off`, 66 s with `on`, 81 to 85 s with
  `eager` (most of it is typechecking and the benchmark library's fixed running time).
- Startup: a transcript with one small watch expression takes 1.16 to 1.19 s with `off`, 1.17 s
  with `on`, 1.19 to 1.21 s with `eager`.

- 2026-10-01, two text benchmarks added to `jitSuite` ("Text: append "hi" 10000 times",
  "Text: drop 1, 100000 times"), every operation a call-out today. `off`: 1.53 ms and
  13.1 ms. `on`: 1.51 ms and 13.0 ms.

## The full `suite` with the JIT on (after M5)

2026-10-01, optimized build, `run suite` in `jit_codebase`, one run each, no statistics. This
is the first time `suite` was run with the JIT. It is the input to [jit-m6.md](jit-m6.md).

| Benchmark | `off` | `on` | |
| --- | --- | --- | --- |
| Mutate a Ref 1000 times | 111.6 µs | 13.1 µs | 8.5× faster |
| Mutate a local Remote.Ref 10k times | 19.7 ms | 104 ms | 5.3× slower |
| Do 10k arithmetic operations | 88.2 µs | 90.2 µs | same |
| List.map increment (range 0 1000) | 142 µs | 201 µs | 1.4× slower |
| List.map murmurHash (range 0 1000) | 392 µs | 600 µs | 1.5× slower |
| Multimap.fromList (range 0 1000) | 65.8 µs | 70.7 µs | 1.1× slower |
| Stream functions | 6.01 ms | 14.4 ms | 2.4× slower |
| Value.serializeUncompressed (10k element map) | 11.5 ms | 11.2 ms | same |
| Value.serializeCompressed (10k element map) | 20.3 ms | 20.8 ms | same |
| Value.deserializeCompressed (10k element map) | 23.4 ms | 27.0 ms | 1.2× slower |
| Json.toText (per document) | 7.07 µs | 7.43 µs | same |
| Json parsing (per document) | 7.20 µs | 7.85 µs | 1.1× slower |
| Json complex parsing (per document) | 10.7 µs | 11.3 µs | 1.1× slower |
| Json complex decoding (per document) | 62.9 µs | 122 µs | 1.9× slower |
| Decode Nat | 167 ns | 200 ns | 1.2× slower |
| Generate 100 random numbers | 83.9 µs | 58.7 µs | 1.4× faster |
| List.foldLeft | 1.23 ms | 2.24 ms | 1.8× slower |
| Count to 1 million | 43.5 ms | 323 µs | 135× faster |
| Count to N (per element) | 83 ns | 4 ns | 21× faster |
| Count to 1000 | 82.9 µs | 4.92 µs | 17× faster |
| CAS an IO.ref 1000 times | 128 µs | 326 µs | 2.5× slower |
| List.range (per element) | 43 ns | 46 ns | 1.1× slower |
| List.range 0 1000 | 59.9 µs | 63.9 µs | 1.1× slower |
| Set.fromList (range 0 1000) | 31.5 µs | 33.4 µs | 1.1× slower |
| Map.fromList (range 0 1000) | 29.1 µs | 31.3 µs | 1.1× slower |
| NatMap.fromList (range 0 1000) | 2.65 ms | 1.88 ms | 1.4× faster |
| Map.lookup (1k element map) | 202 ns | 252 ns | 1.2× slower |
| Map.insert (1k element map) | 275 ns | 323 ns | 1.2× slower |
| Shuffle a 1000 element array | 1.34 ms | 1.73 ms | 1.3× slower |
| Mutably mergesort a 1000 element array | 3.83 ms | 3.48 ms | 1.1× faster |
| List.at (1k element list) | 151 ns | 203 ns | 1.3× slower |
| Text.split / | 2.95 µs | 4.39 µs | 1.5× slower |
| Two match | 5.69 ms | 383 µs | 15× faster |
| Four match | 5.86 ms | 393 µs | 15× faster |
| Thirty match | 5.35 ms | 67.0 µs | 80× faster |
| fib1 | 144 µs | 291 µs | 2.0× slower |
| fib2 | 334 µs | 822 µs | 2.5× slower |
| fib3 | 334 µs | 839 µs | 2.5× slower |

- 9 faster, 5 the same, 24 slower. The cause is exits: 107 million over the run (60 M resumes,
  47 M call-outs), broken down in jit-m6.md.
- How to run it: `unison-src/transcripts-manual/jit-suite.md`, about 5.5 minutes per mode.
- Statistics used to distort timings badly (counting an exit summed every site's counter).
  Since M6 step 0 counting is one atomic add, and a run with `UNISON_JIT_STATS=each` is at
  most 7% slower than a plain one on the entries that exit most. Still, quote timings from
  runs without statistics.
- Re-measured at M6 step 0 (same day, same build plus the statistics change): every entry
  within a few percent of this table, except `Remote.Ref` (80 ms with `on`, was 104).
- To look at the MCode of a site named in the statistics (`CIx ... <group> <n>`): run with
  `UNISON_JIT=off UNISON_JIT_DUMP_MCODE=1`, kill it once loading is done, and search the
  dump for `<group>:<n>:`.

## M6 measurements

`suite`, optimized build, one run each; the ratio is to `off` (measured again at step 0:
within a few percent of the table above). Only entries that moved or are over the 15% line
are listed per step; the full table comes at step 8.

- 2026-10-01, step 1 (the static exit rule, E = 7). 389 functions left interpreted, 117
  compiled plus 110 re-entry functions, 5 MB of IR, 2.1 s of compile time over the run.
  Over 15% slower: `Remote.Ref` 1.39×, `List.map increment` 1.18×, `Stream` 1.16×,
  `Decode Nat` 1.18×, `CAS` 1.18×, `List.at` 1.22×, `fib1` 1.96×, `fib2`/`fib3` 1.97×.
  Close to it: `Map.lookup` 1.15×, `Map.insert` 1.11×, JSON decoding 1.10×. `jitSuite`:
  no change (319 µs, 89.7 µs, 21.6 µs, 5.03 µs, 250 µs, 63.4 µs, 48.2 µs, 59.2 µs, 269 µs,
  1.45 ms, 12.7 ms).
- 2026-10-01, step 2 (generator gaps). `fib1` 144 µs to 11.9 µs, "Do 10k arithmetic
  operations" 89.5 µs to 1.56 µs, `NatMap.fromList` 2.65 ms to 403 µs, `Shuffle` 0.84×.
  Still over 15%: `Remote.Ref` 1.25×, `List.map increment` 1.19×, `Stream` 1.18×,
  `Decode Nat` 1.16×, `CAS` 1.17×, `List.at` 1.21×, `fib2`/`fib3` 1.96×.
- 2026-10-01, step 3 (cheaper round trip: about 37 ns, was about 67). Over 15% now:
  `Remote.Ref` 1.22×, `fib2`/`fib3` 1.20×. Everything else is at or below 1.07×:
  `List.map increment` 1.01×, `Stream` 1.01×, `Decode Nat` 0.92×, `CAS` 0.87×, `Map.lookup`
  0.94×, `List.at` 0.93×, JSON decoding 1.04×. `jitSuite` with `on`: 318 µs, 88.6 µs,
  21.2 µs, 5.07 µs, 272 µs, 64.0 µs, 48.2 µs, 58.9 µs, 260 µs, and the text benchmarks
  1.04 ms and 9.16 ms (`off`: 1.48 ms and 12.8 ms; they were at break-even before).
- 2026-10-01, step 4 (builtins get cells). No change beyond noise except `List.foldLeft`
  0.99× to 1.08×. Over 15%: `Remote.Ref` 1.20×, `fib2`/`fib3` 1.19×. `jitSuite` unchanged.
- 2026-10-01, step 8 (the rule counts exits of callees and weighs recursive arms; small
  functions aren't entered from the interpreter; `Any` registered). Two runs of `on` and a
  fresh `off`, ratios to `off`:

  | Benchmark | `off` | `on`, run 1 | `on`, run 2 |
  | --- | --- | --- | --- |
  | Mutate a Ref 1000 times | 113 µs | 0.10 | 0.10 |
  | Mutate a local Remote.Ref 10k times (one run of 20 ms) | 19.7 ms | 1.22 | 1.30 |
  | Do 10k arithmetic operations | 88.9 µs | 0.02 | 0.02 |
  | List.map increment | 145 µs | 1.01 | 1.00 |
  | List.map murmurHash | 396 µs | 1.01 | 1.01 |
  | Multimap.fromList | 63.9 µs | 0.99 | 1.04 |
  | Stream functions | 5.80 ms | 1.01 | 1.08 |
  | Value.serialize, compressed, deserialize | 10.9, 20.0, 21.9 ms | 1.00, 1.01, 1.01 | 1.00, 1.02, 1.04 |
  | Json.toText, parsing, complex parsing | 7.05, 7.19, 10.7 µs | 1.00, 1.01, 1.00 | 1.08, 1.02, 1.01 |
  | Json complex decoding | 62.1 µs | 1.04 | 1.05 |
  | Decode Nat | 167 ns | 1.08 | 0.94 |
  | Generate 100 random numbers | 83.5 µs | 0.51 | 0.51 |
  | List.foldLeft | 1.20 ms | 1.00 | 1.07 |
  | Count to 1 million, to N, to 1000 | 43.1 ms, 82 ns, 83.2 µs | 0.01, 0.05, 0.06 | same |
  | CAS an IO.ref 1000 times | 127 µs | 1.09 | 0.90 |
  | List.range (per element), 0 1000 | 42 ns, 58.1 µs | 1.02, 1.00 | 1.05, 1.01 |
  | Set.fromList, Map.fromList | 31.4, 29.0 µs | 1.00, 1.00 | 1.03, 1.07 |
  | NatMap.fromList | 2.65 ms | 0.14 | 0.14 |
  | Map.lookup, Map.insert | 199, 274 ns | 1.07, 1.04 | 0.97, 0.99 |
  | Shuffle a 1000 element array | 1.32 ms | 0.74 | 0.75 |
  | Mutably mergesort a 1000 element array | 3.57 ms | 0.20 | 0.20 |
  | List.at | 146 ns | 1.10 | 0.94 |
  | Text.split / | 2.87 µs | 1.02 | 1.02 |
  | Two, Four, Thirty match | 5.64, 5.83, 5.30 ms | 0.08, 0.07, 0.01 | same |
  | fib1 | 144 µs | 0.08 | 0.08 |
  | fib2, fib3 | 336, 336 µs | 0.99, 1.00 | 1.00, 1.00 |

  `jitSuite` with `on`: 315 µs, 88.9 µs, 22.5 µs, 5.01 µs, 270 µs, 63.8 µs, 48.1 µs,
  58.8 µs, 270 µs, 1.06 ms, 9.07 ms. The two regimes and `Remote.Ref` are discussed in
  jit-m6.md. The tree-insert benchmark has been at 270 µs since step 3 (250 before): to
  look at with workers.
- Debug-runtime (`-DS`) runs, 2026-10-01. After part 1: the tests with `on` and
  `THRESHOLD=1` and with `eager`, and the benchmark transcript with `on` and `THRESHOLD=1`,
  passed with no assertion failures; the run with `install=5,pool=8,alloc=64,poll=7` failed
  with "native code disappeared from its cell", the install race described in jit-m6.md
  step 9, fixed there. After workers (commit e91d83367) all four pass with no assertion
  failures: 7.5 min, 7.5 min, 9 min and 4 min.
- 2026-10-01, steps 9 to 11 (workers). `jitSuite`, optimized build, one run each:

  | Benchmark | `off` | `on` | `on`, no workers | `on`, `BATCH=1` | `eager` |
  | --- | --- | --- | --- | --- | --- |
  | Sum 0 to 1 million | 66.4 ms | 319 µs | 322 µs | 320 µs | 319 µs |
  | fib 20 | 1.40 ms | 41.2 µs | 73.6 µs | 41.4 µs | 41.1 µs |
  | Cons list: map with a lambda | 86.9 µs | 17.2 µs | 21.3 µs | 17.0 µs | 16.7 µs |
  | Cons list: foldLeft with a lambda | 86.8 µs | 5.12 µs | 4.89 µs | 4.96 µs | 4.86 µs |
  | Binary tree: 1000 inserts | 1.69 ms | 232 µs | 276 µs | 243 µs | 223 µs |
  | Binary tree: 1000 lookups | 768 µs | 63.0 µs | 67.2 µs | 77.1 µs | 74.1 µs |
  | Apply a function argument 10000 times | 1.17 ms | 49.1 µs | 48.7 µs | 48.9 µs | 48.5 µs |
  | Mutate a Ref 10000 times | 834 µs | 50.9 µs | 51.4 µs | 51.5 µs | 52.7 µs |
  | Calls across definitions (Collatz) | 7.86 ms | 69.4 µs | 262 µs | 258 µs | 264 µs |
  | Text: append "hi" 10000 times | 1.49 ms | 1.08 ms | 1.07 ms | 1.06 ms | 1.06 ms |
  | Text: drop 1, 100000 times | 12.9 ms | 9.19 ms | 9.15 ms | 9.09 ms | 9.04 ms |

  "No workers" is `UNISON_JIT_DISABLE=worker`; it already has the branch weights, which is
  why its `fib 20` is 74 µs and not M5's 89. `suite` with workers: as at step 8, except
  `fib1` 11.9 µs to 5.9 µs, `NatMap.fromList` 380 µs to 294 µs, `Two match` 430 µs to
  403 µs; `Remote.Ref` (the one-shot entry) read 1.42× in this run.
- Since M6 the test matrix adds `UNISON_JIT_DISABLE=worker` (the uniform form everywhere)
  and the costs set to zero (`UNISON_JIT_EXIT_COST=0 UNISON_JIT_ENTRY_COST=0`) in the
  stress runs, so that the rules don't hide code from the tests. A test run can go on a copy
  of the transcript in another directory, which lets several run at once.
- Scripts used for these runs live in the session scratch directory and are easy to
  recreate: run the transcript, strip ANSI codes, take the lines that start with a time.

- 2026-10-02, steps 5, 5b and 7 (lists on `Unison.Util.Deque`, native list, text and
  partial-application helpers). `suite`, optimized build, one run each. "`off` before" is
  the interpreter on `Data.Sequence` (the step 8 run); the last column is `on` against
  `off` on the same build.

  | Benchmark | `off` before | `off` | `on` | `on` / `off` |
  | --- | --- | --- | --- | --- |
  | List.map increment | 145 µs | 151 µs | 64.8 µs | 0.43 |
  | List.map murmurHash | 396 µs | 402 µs | 412 µs | 1.02 |
  | List.foldLeft | 1.20 ms | 1.16 ms | 343 µs | 0.30 |
  | List.at | 146 ns | 130 ns | 25 ns | 0.20 |
  | List.range 0 1000 | 58.1 µs | 45.1 µs | 44.7 µs | 0.99 |
  | List.range (per element) | 42 ns | 102 ns | 103 ns | 1.01 |
  | Multimap.fromList | 63.9 µs | 82.9 µs | 85.4 µs | 1.03 |
  | Set.fromList, Map.fromList | 31.4, 29.0 µs | 27.4, 25.8 µs | 28.5, 27.5 µs | 1.04, 1.07 |
  | Json.toText | 7.05 µs | 8.43 µs | 8.42 µs | 1.00 |
  | Json parsing, complex parsing | 7.19, 10.7 µs | 10.2, 18.1 µs | 10.2, 18.2 µs | 1.00, 1.00 |
  | Json complex decoding | 62.1 µs | 64.3 µs | 59.3 µs | 0.92 |
  | Generate 100 random numbers | 83.5 µs | 84.5 µs | 39.4 µs | 0.47 |
  | Shuffle a 1000 element array | 1.32 ms | 1.27 ms | 636 µs | 0.50 |
  | Text.split / | 2.87 µs | 3.26 µs | 3.29 µs | 1.01 |
  | Stream functions | 5.80 ms | 5.72 ms | 6.08 ms | 1.06 |
  | Mutate a local Remote.Ref (one run of 20 ms) | 19.7 ms | 19.7 ms | 26.3 ms | 1.34 |

  Everything not listed is as at step 8. The `on` column is from before step 7 (partial
  applications), which the suite barely exercises. The interpreter's own regressions from
  the swap are the "`off` before" against "`off`" columns: JSON parsing, `Multimap.fromList`
  and `List.range` per element (see jit-m6.md and the ideas).

  `jitSuite` (`off`, then `on`): 65.7 ms / 319 µs, 1.39 ms / 41.2 µs, 86.6 µs / 16.7 µs,
  86.9 µs / 4.85 µs, 1.67 ms / 203 µs, 762 µs / 56.9 µs, 1.16 ms / 48.3 µs, 828 µs / 50.8 µs,
  7.73 ms / 68.0 µs, text append 1.48 ms / 548 µs (was 1.08 ms), text drop 11.7 ms / 3.54 ms
  (was 9.19 ms).
- Since these steps the test matrix has `UNISON_JIT_DISABLE=list,text,name` in place of
  nothing, and the transcript has sections for lists, text and partial applications. When
  several runs start at the same moment one of them can fail at once with an error about
  `unison.sqlite3-wal` or `credentials.json.lock` (the codebase copy racing another run's);
  run that one again.

- Debug-runtime (`-DS`) runs after steps 5, 5b and 7 (commit 88136dc40), 2026-10-02, all
  with no assertion failures: the tests with `on` and `THRESHOLD=1` (7 min), with `eager`
  (9 min), with `on`, `THRESHOLD=2`, both costs 0 and `install=5,pool=8,alloc=64,poll=7`
  (8 min), and the benchmark transcript with `on` and `THRESHOLD=1` (3.5 min). The four ran
  side by side, each on its own copy of its transcript.
- A last `suite` and `jitSuite` with `on` after step 7: no entry moved (`fib 20` read
  45.7 µs in that run, 41.2 µs in the one before).

## The list representation: `Unison.Util.Deque` (2026-10-01)

A Unison `List` is now a `Unison.Util.Deque Val` instead of a `Data.Sequence` (Paul's
structure, in `lib/unison-util-rope`; every field strict, so native code can read and build
lists without meeting a thunk). `USeq`, `WrapSeq` and the five runtime modules that touch
lists use it; `ANF.Value`'s lists and `Term.List` are still `Data.Sequence` and convert at
the boundary.

- Tests: `stack build --fast --flag unison-runtime:jit --test unison-util-rope` (about a
  minute). They compare with `Data.Sequence` as a model and check the structure's
  invariants (`valid`) after every step.
- Benchmark against `Data.Sequence`:
  `stack build --work-dir .stack-work-opt --flag unison-runtime:jit --bench unison-util-rope`
  (about 6 minutes; `--ba "--csv FILE"` for the numbers).
- `Deque.hs` is compiled with `-O2 -funbox-strict-fields` in every build: the JIT's C
  helpers depend on its constructor layouts.
- Because every package depends on `unison-util-rope`, a change to it rebuilds all the local
  packages in whichever work dir is built next (about 5 minutes for `--fast`).

Time per operation, Deque with its ratio to `Data.Sequence` (below 1: the Deque is faster),
optimized build, after the tuning done on 2026-10-01:

| Operation | n = 10 | n = 100 | n = 10,000 | n = 1,000,000 |
| --- | --- | --- | --- | --- |
| snoc (cons is the same) | 5.7 ns (1.07×) | 14.0 (1.85×) | 14.6 (1.42×) | 26.6 (0.92×) |
| uncons | 8.9 (1.07×) | 11.2 (1.05×) | 10.3 (0.90×) | 10.4 (0.88×) |
| unsnoc | 3.6 (0.44×) | 12.4 (1.18×) | 10.4 (0.91×) | 10.4 (0.90×) |
| snoc n, then uncons n | 14.5 (1.04×) | 25.8 (1.37×) | 25.3 (1.06×) | 35.7 (0.86×) |
| queue at a steady size | 26.4 (1.77×) | 21.6 (1.10×) | 24.0 (0.93×) | 46.8 (0.85×) |
| lookup | 9.8 (1.01×) | 18.7 (0.78×) | 47.9 (0.49×) | 82.3 (0.45×) |
| take | 12.8 (0.99×) | 66.4 (1.89×) | 207 (1.48×) | 367 (1.47×) |
| drop | 14.9 (1.14×) | 94.8 (2.68×) | 220 (1.57×) | 378 (1.56×) |
| take/drop within 4 of an end | 15.9 (1.28×) | 27.8 (2.13×) | 13.1 (1.01×) | 13.1 (0.98×) |
| append of two halves | 20.9 (1.89×) | 175 (5.4×) | 376 (4.0×) | 468 (2.8×) |
| append of 1 to 4 elements | 100 (3.5×) | 100 (3.5×) | 74 (2.6×) | 74 (2.6×) |
| `foldl'`, per element | 4.9 (0.43×) | 4.0 (0.39×) | 3.7 (0.36×) | 4.0 (0.38×) |
| toList, per element | 5.0 (0.79×) | 4.4 (0.55×) | 4.1 (0.50×) | 15.8 (1.01×) |
| `==`, per element | 14.4 (1.32×) | 8.3 (0.77×) | 8.2 (0.72×) | 32.8 (0.82×) |

The snoc row flatters `Data.Sequence`, which defers work into thunks; "snoc then uncons" is
the fair comparison. Left for later: `append` (builds each level's seam as a list, then packs
it), `take`/`drop` around 100 elements, the queue pattern on very small lists.

## Baseline: interpreter only

Measured 2026-09-29 on the optimized build of branch `jit` (commit c5bcd5ae7, no JIT code yet),
macOS arm64. These are the numbers the JIT has to beat. Time is per run of the benchmark body.

Benchmarks written for the JIT (`jitSuite`):

| Benchmark | Interpreter |
| --- | --- |
| Sum 0 to 1 million | 68.66395ms |
| fib 20 | 1.371103ms |
| Cons list: map with a lambda (1000 elements) | 84.286µs |
| Cons list: foldLeft with a lambda (1000 elements) | 87.023µs |
| Binary tree: 1000 inserts | 1.662015ms |
| Binary tree: 1000 lookups | 760.471µs |
| Apply a function argument 10000 times | 1.129111ms |
| Mutate a Ref 10000 times | 838.72µs |

The existing suite (`suite`), run once by hand for reference. The benchmark transcript doesn't run it:

| Benchmark | Interpreter |
| --- | --- |
| Mutate a local Remote.Ref 10k times | 19.115ms |
| Do 10k arithmetic operations | 88.75µs |
| List.map increment (range 0 1000) | 139.277µs |
| List.map murmurHash (range 0 1000) | 395.484µs |
| Multimap.fromList (range 0 1000) | 65.98µs |
| Stream functions | 5.705434ms |
| Value.serializeUncompressed (10k element map) | 10.614256ms |
| Value.serializeCompressed (10k element map) | 20.456287ms |
| Value.deserializeCompressed (10k element map) | 22.095175ms |
| Json.toText (per document) | 7.158µs |
| Json parsing (per document) | 7.238µs |
| Json complex parsing (per document) | 10.702µs |
| Json complex decoding (per document) | 65.454µs |
| Decode Nat | 172ns |
| Generate 100 random numbers | 82.865µs |
| List.foldLeft | 1.177485ms |
| Count to 1 million | 42.914662ms |
| Count to N (per element) | 80ns |
| Count to 1000 | 80.07µs |
| CAS an IO.ref 1000 times | 128.068µs |
| List.range (per element) | 46ns |
| List.range 0 1000 | 60.964µs |
| Set.fromList (range 0 1000) | 31.458µs |
| Map.fromList (range 0 1000) | 29.114µs |
| NatMap.fromList (range 0 1000) | 2.599734ms |
| Map.lookup (1k element map) | 200ns |
| Map.insert (1k element map) | 272ns |
| Shuffle a 1000 element array | 1.306739ms |
| Mutably mergesort a 1000 element array | 3.578984ms |
| List.at (1k element list) | 143ns |
| Text.split / | 2.908µs |
| Two match | 5.606368ms |
| Four match | 5.912143ms |
| Thirty match | 5.406546ms |
| fib1 | 138.082µs |
| fib2 | 314.239µs |
| fib3 | 311.092µs |

## Facts about the environment

- `jit_codebase/` (repo root) has a project `jit-tests`, branch `main`. It contains the
  `@pchiusano/misc-benchmarks` suite (`suite`, `fibsuite`, `fib1`, `time`, `printTime`, `repeat`, ...)
  and these libraries: `unison_base_4_5_0`, `unison_json_1_3_4`, `unison_cloud_20_15_1`,
  `mitchellwrosen_benchmark_2_0_0`.
- Transcript stanzas must use the prompt `jit-tests/main>`.
- LLVM 23.1.2 is installed with Homebrew (`brew install llvm`, 2026-09-29). It is keg-only, so it is
  not on the PATH: use `/opt/homebrew/opt/llvm/bin/llvm-config`. This is the pinned version (D3).
- Machine: macOS, arm64.

## Log

- 2026-09-29: plan signed off. Confirmed `stack build --fast` succeeds on branch `jit`.
- 2026-09-29: wrote both transcripts. `jit-tests.md` passes (25 stanzas). First benchmark run was on an
  unoptimized binary (dated Sep 26) and is about 70x too slow, so those numbers were discarded.
  Ran `stack clean` and `stack build` to get an optimized binary, then recorded the baseline above.
  Pre-M0 is done.
- 2026-09-29: installed LLVM 23.1.2. Wrote and ran M0 spike 1 (LLVM); it passes on macOS arm64.
  Next: M0 spike 2, the GHC runtime from C.
- 2026-09-29: wrote and ran M0 spike 2 (runtime); passes in normal and debug mode, and the negative
  test fails as it should. Next: M0 spike 3, interpreter overhead of the cell field and check.
- 2026-09-30: M1 complete (see "M1 measurements").
- 2026-09-30: M2 steps 1-6. Re-entry points turned out to need no code generation: the MCode
  emitter already makes every `Let` body a combinator whose arguments are the whole frame, so the
  body combinator's native code (compiled by M1) is the re-entry point, and `Let` and `Push` carry
  its cell. Facts learned: GHC's worker threads have 512 KB C stacks on macOS
  (`pthread_get_stacksize_np` reports 536576), so the native call budget is about 250 KB after the
  reserve, roughly 1600 frames of a small function; "depth 1000000" hits the C stack guard 615
  times and each hit costs one interpreted `Let`. The Unison stack growth policy matters more:
  see the `GrowStack` note in [jit-m2.md](jit-m2.md).
- 2026-09-30: M3 complete. Lessons: a `Seg`'s arrays are behind lifted boxes (see jit-m3.md);
  `allocate` per `Pack` rather than per run because of the sanity checker; the interface
  registers data type arities with the JIT since `DMatch` arms need them; a second runtime in
  the same process calls `startJIT` twice, so it now no-ops the second time; LLJIT resolves
  process symbols by itself, so C helpers called from IR need no `defineSymbol`.
- 2026-09-30: M2 step 7 (inline `Let` bindings). The benchmark transcript then failed with
  "applying non-function" although the test matrix passed: a `Let` inside a binding has a body
  combinator whose arity isn't the interpreter's frame depth (see the decision in jit-m2.md), so
  re-entering it read the wrong slots. Lesson: the test transcript uses builtins only, and base
  library code (`printTime`) has shapes it doesn't. Run the benchmark transcript as a test too,
  with the `--fast` binary, before calling a milestone done.
- 2026-09-30: M4 steps 1, 2 and 2b. Every instruction without a native version is now a
  call-out, and the code after it is an auxiliary LLVM function in the same module (named
  `u<grp>_<i>_r<n>`, with its own cell). The same mechanism, with a *frame base*, gives
  re-entry inside inline bindings and to `Let`s nested in them, so M2's "applying non-function"
  limitation is gone. Lessons: MCode's `Ins` doesn't say how many values an instruction pushes
  (`Prim1 LOAD` pushes two, `TRCE` none), so the generator keeps a `pushCount` table and the
  trampoline verifies the stack pointer before re-entering. The exit and frame tables were
  registered from the first generation pass, whose cells were placeholders; now the second
  pass's entries replace them. The interpreter's `apply` never looked at the callee's cell, so
  function values (thunks passed to handlers, say) always ran interpreted; it now enters native
  code like `enter`. A `DMatch` on a type whose arities aren't registered (`Boolean` is a
  builtin reference, not in `builtinDataSpec`) is still compiled for the enumeration case, since
  the pointer tag identifies it. A `UNISON_JIT_LOG=1` line "partly interpreted" now says why a
  branch arm or binding fell back.
- 2026-09-30: M4 steps 3 to 6. Native closure calls; `apply` enters native code; `Ref`, mutable
  arrays and universal comparison native; combinators-as-values from the pool. Two bugs worth
  remembering: (1) the generator went exponential on `Duration.toText` (a `Let` inside an inline
  binding got an auxiliary function per occurrence, and the body was inlined too), which showed
  up as every benchmark "hanging" while the compiler ate memory; auxiliary functions are now
  memoized per (section, depth, base). Bisected with the new `UNISON_JIT_DISABLE`. (2) The
  pool's `PAp` for a combinator used `nullSeg`, whose boxes are lazy CAFs: native code read
  through an indirection and refused every closure call, silently. Segment boxes are now
  checked for an evaluated pointer tag before use. Also: the debug-RTS binary lives in its own
  work dir now (see "How to build and run"), and Stack work dirs replace `stack clean`.
- 2026-09-30: M5 complete. Things worth remembering: (1) the interpreter's hot-count test
  must be a comparison with a constant. The first version compared with the configured
  threshold and counted in `yield` too, and made the interpreter 5 to 20% slower with the JIT
  off; cells now count up from minus the threshold to zero. Re-measure `off` whenever `enter`,
  `apply` or `yield` change. (2) Callees get hot before callers, so batches of more than one
  definition are rare and direct calls only matter within a definition. (3) The constant pool
  had to be made safe for code that is installed while native code runs (see jit-m5.md). (4)
  The compile driver now works on *units* (one LLVM function each: a combinator or a re-entry
  function), and a table of pending units, keyed by cell, holds the re-entry functions that
  were not generated. (5) A definition is never queued twice (see the next entry).
- 2026-10-01: M6 steps 0 to 4, 6, 8 and 9 to 11 (see jit-m6.md and "M6 measurements").
  Paused there at Paul's request; lists (5), text (5b) and partial applications (7) are
  open. Things worth remembering: (1) the static exit rule needed to count a callee's
  exits against its caller, and to weigh recursive arms, before it judged ability-heavy
  code right. (2) A round trip allocates nothing now; its results come back in spare words
  of the unboxed stack. (3) Workers were only half the gain on `fib`: branch weights and a
  fast entry for base cases were the rest, both because of registers LLVM saves for exit
  paths. (4) A function can be running before its own cell is filled (direct calls), so
  the trampoline waits for a cell rather than failing. (5) A second runtime in the process
  reuses group numbers, so function names get a suffix. (6) One-shot timings early in a
  run see the compile thread's allocation as a major GC.
- 2026-10-01: after Paul's review of M5. Batches now walk callers as well as callees, as the
  design said from the start (the M5 plan had narrowed it to callees); the compile thread
  keeps the reverse index, since the code cache only records callees. The "requested" state
  moved from a separate set to a flag in the native code cell, set by compare-and-swap
  (`claimNativeCell`); a definition's flag is its entry combinator's. Then a benchmark with a
  hot loop calling across definitions was added, and it showed that a batch missed exactly
  the hottest callee (its own request had been queued while the caller's waited), so the flag
  became three states and a batch takes a neighbour whose request is still queued. Result: 8%
  on that benchmark for about twice the compile work.
- 2026-10-02: M6 finished (steps 5, 5b, 7), after Paul's go-ahead. The order it went in:
  Paul's `Unison.Util.Deque` got its module name, a test suite against `Data.Sequence`
  (with an invariant checker) and a benchmark; the benchmark showed it 2 to 12 times slower
  on small lists and 4 to 9 times slower at `take`/`drop`, so the bottom-level paths,
  `take`/`drop` and `append` were rewritten to work on the grouped levels directly, which
  brought it to parity or better except for `append`. Then the runtime's lists were
  switched to it (five modules), the list, text and partial-application operations became
  C helpers called from generated code, and the layouts they depend on are checked at
  startup. Things that went wrong on the way, all found by the test matrix: two helpers
  dead-stripped by the linker (modules silently not linking), and a per-thread context
  created before the stress settings were read (a livelock under `poll` stress on one
  thread only). `Unison.Util.Skews`, another candidate structure of Paul's, sits in the same
  package untested.
