# The `jit` branch against `trunk`: what the diff is made of

Measured on 2026-10-06 with `git diff --numstat` from the merge base with `trunk` (`a1652e3ad`, the
head of `trunk` at the time), after the M0 spike programs were deleted. Lines are added plus removed.

21,265 lines added, 349 removed, 21,614 in all. This writeup counts itself among the docs.

## By kind of file

| Kind | Files | Added | Removed | Lines | Share |
| --- | --- | --- | --- | --- | --- |
| New Haskell: JIT modules (`Runtime/JIT.hs`, `Runtime/JIT/*.hs`) | 12 | 7,601 | 0 | 7,601 | 35% |
| C (`cbits/jit_rt.c` 3,934, `jit_llvm.c` 168, `jit_rt.h` 79) | 3 | 4,181 | 0 | 4,181 | 19% |
| Markdown docs (`Runtime/JIT/*.md` and `Runtime/JIT/benchmarks/`) | 22 | 3,048 | 0 | 3,048 | 14% |
| Transcripts: tests 2,182, benchmarks 273 | 3 | 2,455 | 0 | 2,455 | 11% |
| New Haskell: `Deque`, `Skews`, rope tests and benchmarks | 7 | 2,446 | 0 | 2,446 | 11% |
| Existing Haskell, modified | 16 | 1,294 | 343 | 1,637 | 8% |
| Build config and CI | 8 | 244 | 6 | 250 | 1% |

The modified existing code is concentrated in three files:

| File | Added | Removed | What changed |
| --- | --- | --- | --- |
| `lib/unison-util-rope/src/Unison/Util/Rope.hs` | 595 | 196 | rebuilt on the `Deque` finger tree, with a layout C can read |
| `unison-runtime/src/Unison/Runtime/Machine.hs` | 290 | 84 | the trampoline: `runNative`, exit handling, counting |
| `unison-runtime/src/Unison/Runtime/MCode.hs` | 254 | 8 | the `NativeCell` type and its operations |
| `Stack.hs`, `Interface.hs`, `Machine/Types.hs`, `Foreign/Function.hs`, `ArgParse.hs` | 132 | 38 | frame records, startup, config, foreign-call hooks, flags |
| eight other files | 23 | 17 | one to six lines each |

## By role in the design

| Area | What it is | Where | Lines | Share |
| --- | --- | --- | --- | --- |
| Code generation | MCode to LLVM IR | `Codegen.hs` 4,140, `Strict.hs` 50 | 4,190 | 19% |
| Documentation | design, internals, development, ideas, builtins, this file, and 15 benchmark runs | 22 Markdown files | 3,048 | 14% |
| Native builtins | C implementations of the list, Text, Bytes, array, Ref and hashing primitives, and the Haskell side that registers them | `jit_rt.c` sections Lists 1,067, Ropes 846, the rest of Text and Bytes 857, Arrays and Refs 290, murmurHash 251; `Native.hs` 363 | 3,674 | 17% |
| Runtime data structures | `Deque` and `Skews`, the `Rope` rework, their tests and benchmarks, small `Text` and `Bytes` changes | `lib/unison-util-rope`, `Util/Text.hs`, `Util/Bytes.hs` | 3,257 | 15% |
| Tests and benchmarks | the transcripts | `jit-tests.md`, `jit-benchmarks.md`, `jit-suite.md` | 2,455 | 11% |
| Trampoline and interpreter integration | native cells, exits, frames, the constant pool, entering and leaving native code, partial applications | `Exits.hs`, `Frames.hs`, `Pool.hs`, `MCode.hs`, `Machine.hs`, `Machine/Types.hs`, `Stack.hs`, `Interface.hs`, `Foreign/Function.hs`, `jit_rt.h`, `jit_rt.c` sections Entering native code 136 and Partial applications 217 | 1,702 | 8% |
| Compile driver | what to compile and when, budgets, configuration, the LLVM shim | `JIT.hs` 410, `Compile.hs` 478, `Config.hs` 188, `Estimate.hs` 237, `LLVM.hs` 126, `jit_llvm.c` 168 | 1,607 | 7% |
| Closure layout and allocation | probing GHC's heap layouts at startup, allocation and the write barrier | `Layout.hs` 1,210, `jit_rt.c` sections Allocation 129, Numbers 31, layout probe 33 | 1,403 | 6% |
| Build config, CI and CLI | packaging, the CI flag, command-line flags | `package.yaml`, `.cabal`, `stack.yaml`, `hie.yaml`, `test.yaml`, `ArgParse.hs`, `Main.hs` | 282 | 1% |

The compiler proper (code generation, the driver, layout, the trampoline) is 8,900 lines, 41% of
the diff, and `Codegen.hs` alone is nearly half of that. The native builtins plus the data-structure
rework are almost as large, 6,900 lines or 32%: the cost of the M5 to M7 decision to give native
code direct access to lists, Text and Bytes. Docs and tests together are a quarter of the branch.

The JIT modules by size:

| Module | Lines |
| --- | --- |
| `Codegen.hs` | 4,140 |
| `Layout.hs` | 1,210 |
| `Compile.hs` | 478 |
| `JIT.hs` | 410 |
| `Native.hs` | 363 |
| `Estimate.hs` | 237 |
| `Config.hs` | 188 |
| `Pool.hs` | 168 |
| `Exits.hs` | 162 |
| `LLVM.hs` | 126 |
| `Frames.hs` | 69 |
| `Strict.hs` | 50 |
