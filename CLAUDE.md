# Notes for Claude

## JIT work (branch `jit`)

Before doing anything on the JIT, read these in order:

1. `docs/jit-progress.md`: current status, how to build and test, baseline benchmark numbers, and
   things learned the hard way. **Keep it up to date** as you finish steps or learn something
   non-obvious, since sessions run out of context and this file is how work carries over.
2. `docs/jit-design.md`: what is being built and why.
3. `docs/jit-implementation-plan.md`: the signed-off decisions (D1 to D19) and milestones (M0 to M6).

Rules:

- Iterate with `stack build --fast`. Benchmark only on an optimized build: `stack clean`, then
  `stack build`. Stack does not rebuild when only the optimization level changes.
- Correctness tests: `stack exec unison -- -C jit_codebase transcript.fork unison-src/transcripts/idempotent/jit-tests.md`
- Benchmarks: `stack exec unison -- -C jit_codebase transcript.fork unison-src/transcripts-manual/jit-benchmarks.md`
- Running a transcript writes `<name>.output.md` next to it. Don't commit those.
- The Markdown files in `docs/` are the source of truth for the design and plan. Paul edits them
  directly and sometimes leaves `Reply:` or `(TODO)` notes for you, so re-read a file before editing it.
- Commit to the `jit` branch at each checkpoint (Paul approved this on 2026-09-29). Don't push.
- Linux is skipped for now. Spikes and tests only need to pass on macOS arm64.
