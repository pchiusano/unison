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
| M1: skeleton and numeric loops | not started |
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
  a plain `stack build` compiles nothing and leaves the unoptimized binary in place. To get an optimized
  binary: `stack clean` and then `stack build`. If that build is interrupted, plain `stack build` resumes it. Check by running the benchmarks: on an optimized build
  "Count to 1 million" in `suite` takes about 40 ms on this machine class; on a `--fast` build, about 3 s.
- `stack.yaml` builds everything with `-fno-omit-yields`, so compiled Haskell loops that don't allocate
  can still be preempted. Native code gets no such help, which is why the design polls.
- GHC 9.10.3, Stack resolver lts-24.38. Stack prints warnings about untested GHC/Cabal versions; they're harmless.

## Test and benchmark transcripts

| File | What it is | Codebase it needs |
| --- | --- | --- |
| `unison-src/transcripts/idempotent/jit-tests.md` | correctness tests with known answers. Builtins only, prompt `scratch/main>`. The file contains its own expected output. | any, including empty |
| `unison-src/transcripts-manual/jit-benchmarks.md` | `jitSuite`, the benchmarks written for the JIT. Prompt `jit-tests/main>`. Timings print to the console. It does not run the existing `suite`, which takes several minutes; run that by hand when needed. | `jit_codebase` |

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
before the optimized `stack build`, or that package stays unoptimized. Rebuilding just
`unison-runtime` and relinking takes a few minutes; a full clean build takes about 15.

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
