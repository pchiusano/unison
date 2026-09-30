# M1: skeleton and numeric loops

Working plan for milestone M1 of the [implementation plan](jit-implementation-plan.md). Written
before starting, 2026-09-30; the checkboxes are ticked as steps land. Each step is a commit.

**Goal.** One compiled function can run end to end. The smallest useful subset of MCode compiles;
everything else exits to the interpreter. Exit criteria: the test transcript passes in eager mode,
"Count to 1 million" is at least 10× faster, and a native infinite loop can be killed.

## Order of work

- [x] **1. Build plumbing.** Package flag `jit` in `unison-runtime/package.yaml` (adds the C
  sources, `-DUNISON_JIT`, LLVM link options). The C shim from the LLVM spike becomes
  `cbits/jit_llvm.c`. `Unison.Runtime.JIT.LLVM` holds the foreign imports; with the flag off it
  exports the same functions as stubs that say the JIT isn't built in. LLVM's paths go in
  `stack.yaml` (`extra-lib-dirs`, `extra-include-dirs`). Check: `stack build --flag unison-runtime:jit`
  links, and `UNISON_JIT_LOG=1` prints the LLVM target triple at startup.
- [x] **2. `Ctx`, exits table, trampoline.** `cbits/jit_rt.h` defines `Ctx`; `Unison.Runtime.JIT.Ctx`
  mirrors it with offsets checked against the C side at startup. `Unison.Runtime.JIT.Exits` is the
  global exits table. The trampoline lives in `Machine.hs`, because it calls `eval` and `yield` and
  `enter` calls it. No code generation yet: test by installing a hand-written IR function that
  immediately exits, and checking the interpreter carries on correctly.
- [x] **3. Code generation for the subset.** (done 2026-09-30; `UNISON_JIT_STATS` not yet, see step 5) `Unison.Runtime.JIT.IR` (text builder) and
  `Unison.Runtime.JIT.Codegen`. Eager mode: `UNISON_JIT=eager` compiles each loaded top-level
  definition, with its local functions, as one module in `cacheAdd0` after the transaction.
  `UNISON_JIT_DUMP_IR=dir` writes each module's IR. Check: the loop tests in `jit-tests.md` run
  natively (visible from `UNISON_JIT_STATS`), and the whole transcript still passes.
- [x] **4. Entry checks and polling.** (done 2026-09-30) The Unison stack check (`GrowStack`) and the preemption poll
  (`Reenter`) at every function entry. Stress modes `poll` and `ustack`. A `killThread` test in
  the transcript.
- [x] **5. Diagnostics.** (done 2026-09-30; `UNISON_JIT_STATS` prints after each evaluation, since nothing calls the runtime's `terminate`) `UNISON_JIT_LOG` (per module: functions, compile time), `UNISON_JIT_STATS`
  (per exit site counts at exit). The `--jit` command-line option.
- [x] **6. Measure.** Numbers are in the progress log under "M1 measurements". M1 complete, 2026-09-30. Optimized build, benchmarks with the JIT off and in eager mode, results in the
  progress log. Then update the plan's status.

## What M1 compiles

| MCode | Native code |
| --- | --- |
| `Ins (Lit (MI/MN/MC/MD …))` | store the word and the type tag |
| `Ins (Prim1 op i)`, `Ins (Prim2 op i j)` for arithmetic, comparison, bitwise and conversion ops on Nat, Int, Float, Char, Boolean | inline, with the interpreter's exact semantics (checked against `Machine/Primops.hs`) |
| `Match i branches` (`Test1`, `Test2`, `TestW`, default) | compare and branch |
| `Yield args` | move the results as `moveArgs` does, return `OK` |
| `Call` to the function itself (self tail call) | move the arguments into the argument slots, branch to the entry block |
| `Call` to another known function | move the arguments, load the callee's cell, `musttail` if compiled, else `Resume` at the `Call` |
| anything else: `App`, `Let`, `Jump`, `DMatch`, `NMatch`, `RMatch`, `Die`, other instructions, `TestT` | `Resume` exit at that section |

A function whose body starts with something unsupported isn't compiled at all, since it would only
exit. There are no non-tail native calls, so no frame records and no re-entry points; those are M2.

## Decisions made while planning M1

- **Stack slots are allocas** (D11). On entry the arguments are loaded from the Unison stack. Slots
  are written back before a tail call to another function and on every exit path.
- **Unboxed values need their type tag in `bstk`**, so even M1 writes to the boxed stack: the four
  type-tag closures. These come from a single global **constant pool**, a `MutableArray` whose
  address is put in `Ctx` at every entry. Per-module pools come with `Pack` in M3.
- **No card marking in M1.** The only pointers native code stores are the type-tag closures, which
  are static objects that the GC never moves or frees, so a missed mark can't cause harm. Marking
  arrives in M3 with allocation, along with a high-water mark of the slots written.
- **`Ctx` fields for M1:** `ustk`, `bstk`, `pool` (addresses, set on entry), `stack_size`, the
  address of the capability's `HpLim` (for the poll), `ap`/`fp`/`sp` (written on exit), and stress
  counters. Status is the return value; exit indices are global (1-based), `OK = 0`, `EXIT_ERROR = -1`.
- **The exits table** is a Haskell `IORef` holding a growable array of `Exit`. A module takes a
  contiguous range when compiled. M1 has `Resume (CombIx, RSection Val)`, `GrowStack`, `Reenter`.
- **The trampoline mirrors `enter`.** It does what `enter` does before the body (`ensure`,
  `moveArgs`, `acceptArgs`), calls native code, reads `ap`/`fp`/`sp` back, and then: `OK` →
  `yield` with the results in place; `Resume` → `eval` at the section; `GrowStack` → `ensure` then
  call again; `Reenter` → `yield` to the scheduler (a Haskell `yield`), then call again.
- **Eager mode only.** `UNISON_JIT` is `off` (default) or `eager`. `on`, with counters and a
  background thread, is M5. Code loaded through `restoreCache` (compiled program files) isn't
  compiled in M1 either; it shares the "never compiled" cell.
- **One module per top-level definition** (D17), functions in it call each other directly.
