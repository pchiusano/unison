# The `jit` branch against `trunk`: what the diff is made of

Measured on 2026-10-06 with `git diff --numstat` from the merge base with `trunk` (`84b95a623`), after
the M0 spike programs were deleted. Lines are added plus removed.

The branch also carries about 1,300 lines that are not JIT work: the GADT, GADT-indexed abilities
and pattern-matching error-message branches that were merged into `jit` ahead of `trunk`
(`parser-typechecker`, `unison-syntax` and their transcripts). Everything below excludes them.

JIT diff: 23,634 lines added, 349 removed, 23,983 in all.

## By kind of file

| Kind | Files | Added | Removed | Lines | Share |
| --- | --- | --- | --- | --- | --- |
| New Haskell: JIT modules (`Runtime/JIT.hs`, `Runtime/JIT/*.hs`) | 12 | 7,586 | 0 | 7,586 | 32% |
| Markdown docs (`docs/jit/*.md`, `JIT/design.md`, `JIT/internals.md`) | 13 | 5,437 | 0 | 5,437 | 23% |
| C (`cbits/jit_rt.c` 3,934, `jit_llvm.c` 167, `jit_rt.h` 79) | 3 | 4,180 | 0 | 4,180 | 17% |
| Transcripts: tests 2,182, benchmarks 273 | 3 | 2,455 | 0 | 2,455 | 10% |
| New Haskell: `Deque`, `Skews`, rope tests and benchmarks | 7 | 2,446 | 0 | 2,446 | 10% |
| Existing Haskell, modified | 16 | 1,290 | 343 | 1,633 | 7% |
| Build config and CLI flags | 7 | 240 | 6 | 246 | 1% |

The modified existing code is concentrated in three files:

| File | Added | Removed | What changed |
| --- | --- | --- | --- |
| `lib/unison-util-rope/src/Unison/Util/Rope.hs` | 595 | 196 | rebuilt on the `Deque` finger tree, with a layout C can read |
| `unison-runtime/src/Unison/Runtime/Machine.hs` | 286 | 84 | the trampoline: `runNative`, exit handling, counting |
| `unison-runtime/src/Unison/Runtime/MCode.hs` | 254 | 8 | the `NativeCell` type and its operations |
| `Stack.hs`, `Interface.hs`, `Machine/Types.hs`, `Foreign/Function.hs`, `ArgParse.hs` | 132 | 38 | frame records, startup, config, foreign-call hooks, flags |
| eight other files | 23 | 17 | one to six lines each |

## By role in the design

| Area | What it is | Where | Lines | Share |
| --- | --- | --- | --- | --- |
| Documentation | design, internals, plan, ideas, builtins, milestone writeups m1 to m7, progress log | 13 Markdown files | 5,437 | 23% |
| Code generation | MCode to LLVM IR | `Codegen.hs` 4,137, `Strict.hs` 50 | 4,187 | 17% |
| Native builtins | C implementations of the list, Text, Bytes, array, Ref and hashing primitives, and the Haskell side that registers them | `jit_rt.c` sections Lists 1,067, Ropes 846, the rest of Text and Bytes 857, Arrays and Refs 290, murmurHash 251; `Native.hs` 363 | 3,674 | 15% |
| Runtime data structures | `Deque` and `Skews`, the `Rope` rework, their tests and benchmarks, small `Text` and `Bytes` changes | `lib/unison-util-rope`, `Util/Text.hs`, `Util/Bytes.hs` | 3,257 | 14% |
| Tests and benchmarks | the transcripts | `jit-tests.md`, `jit-benchmarks.md`, `jit-suite.md` | 2,455 | 10% |
| Trampoline and interpreter integration | native cells, exits, frames, the constant pool, entering and leaving native code, partial applications | `Exits.hs`, `Frames.hs`, `Pool.hs`, `MCode.hs`, `Machine.hs`, `Machine/Types.hs`, `Stack.hs`, `Interface.hs`, `Foreign/Function.hs`, `jit_rt.h`, `jit_rt.c` sections Entering native code 136 and Partial applications 217 | 1,698 | 7% |
| Compile driver | what to compile and when, budgets, configuration, the LLVM shim | `JIT.hs` 409, `Compile.hs` 478, `Config.hs` 187, `Estimate.hs` 237, `LLVM.hs` 116, `jit_llvm.c` 167 | 1,594 | 7% |
| Closure layout and allocation | probing GHC's heap layouts at startup, allocation and the write barrier | `Layout.hs` 1,210, `jit_rt.c` sections Allocation 129, Numbers 31, layout probe 33 | 1,403 | 6% |
| Build config and CLI | packaging and flags | `package.yaml`, `.cabal`, `stack.yaml`, `hie.yaml`, `ArgParse.hs`, `Main.hs` | 278 | 1% |

The compiler proper (code generation, the driver, layout, the trampoline) is 8,900 lines, 37% of
the diff, and `Codegen.hs` alone is nearly half of that. The native builtins plus the data-structure
rework are almost as large, 6,900 lines or 29%: the cost of the M5 to M7 decision to give native
code direct access to lists, Text and Bytes. Docs and tests together are a third of the branch.

The JIT modules by size:

| Module | Lines |
| --- | --- |
| `Codegen.hs` | 4,137 |
| `Layout.hs` | 1,210 |
| `Compile.hs` | 478 |
| `JIT.hs` | 409 |
| `Native.hs` | 363 |
| `Estimate.hs` | 237 |
| `Config.hs` | 187 |
| `Pool.hs` | 168 |
| `Exits.hs` | 162 |
| `LLVM.hs` | 116 |
| `Frames.hs` | 69 |
| `Strict.hs` | 50 |
