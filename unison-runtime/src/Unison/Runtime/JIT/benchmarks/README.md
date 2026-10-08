# Benchmark runs

One file per run, named `<yyyy-mm-dd>-<what-it-measured>.md`: what the run was (build, mode,
how many runs), the table, then what it showed. How to run the suites is in
[development.md](../development.md). The latest full run is what the next one is compared
against.

| Run | What it answered |
| --- | --- |
| [2026-09-29 interpreter baseline](2026-09-29-interpreter-baseline.md) | both suites with no JIT code at all |
| [2026-09-29 cell-check overhead](2026-09-29-cell-check-overhead.md) | what the native code cell costs the interpreter (under 2%) |
| [2026-09-30 M1 to M5 steps](2026-09-30-m1-to-m5-steps.md) | `jitSuite` after each early milestone, and what still exited |
| [2026-10-01 suite after M5](2026-10-01-suite-after-m5.md) | the broad suite's first run with the JIT: 24 rows slower, and the 107 M exits by kind |
| [2026-10-01 M6 exit rule and workers](2026-10-01-m6-exit-rule-and-workers.md) | each M6 step: the static exit rule, the round trip from 67 to 37 ns, workers and what each change bought `fib` |
| [2026-10-02 Deque vs Data.Sequence](2026-10-02-deque-vs-data-sequence.md) | the strict finger tree against the list it replaced |
| [2026-10-02 rope vs old rope](2026-10-02-rope-vs-old-rope.md) | the finger-tree rope against the size-balanced tree, and the threshold |
| [2026-10-02 native lists in the suite](2026-10-02-native-lists-in-the-suite.md) | the suite on the strict list with native list operations |
| [2026-10-02 text and bytes helpers](2026-10-02-text-and-bytes-helpers.md) | the text and bytes rows as they went native, and the open `Json.toText` loss |
| [2026-10-03 every benchmark](2026-10-03-every-benchmark.md) | full run midway through M7 |
| [2026-10-03 inline bump allocation](2026-10-03-inline-bump-allocation.md) | bump allocation, then the budget folded into the limit |
| [2026-10-04 private copies and re-entry batches](2026-10-04-private-copies-and-reentry-batches.md) | compile totals and the suite with copies and batched re-entry functions |
| [2026-10-04 floats and hash](2026-10-04-floats-and-hash.md) | the float loop and `murmurHashUntyped` |
| [2026-10-05 arrays and refs](2026-10-05-arrays-and-refs.md) | the array, ref and CAS rows |
| [2026-10-05 every benchmark, end of M7](2026-10-05-every-benchmark-end-of-m7.md) | the closing tables of M7 |
| [2026-10-07 LLVM loaded at run time](2026-10-07-llvm-loaded-at-run-time.md) | every benchmark after the switch to `dlopen`; unchanged from the end of M7 |
