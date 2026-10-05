# Unison JIT Design

Sep 26, 2026 · Paul Chiusano

## Summary

This document describes the design of Unison's new LLVM-based JIT compiler, which can execute Unison code at native speeds.

A bit of history to motivate this design: we previously attempted a JIT that aimed to compile the entirety of the language to Racket Scheme (itself compiled at runtime to native code). This effort was ultimately abandoned for a few reasons: it was quite complicated to try to cover literally everything in the Unison language, especially the dynamic code-loading and serialization support used by Unison Cloud, and other than simple arithmetic loops, we never saw major performance wins over the interpreter. Certain functional code was actually *faster* with our tuned interpreter. It was also very much an "all or nothing" effort that couldn't be used until every Unison builtin had a Scheme implementation.

This new JIT uses a different approach that allows it to be useful immediately. The idea is to compile functions to native code with [LLVM ORC](https://llvm.org/docs/ORCv2.html). But rather than needing to cover the entirety of the Unison language and its builtins, we allow native-compiled functions to "exit" back to the interpreter. Thus, delimited continuations, abilities, and any other subset of the language which is hard to compile to LLVM will continue to work fine but will defer to the interpreter at runtime. The idea is similar to what's done in a tracing JIT, where branches "off trace" revert to the interpreter, but here we're applying the idea to a function-at-a time JIT.

Native code, once compiled, is entered through an `unsafe` foreign call, using the C calling convention. Native code allocates Haskell heap objects just like any other compiled Haskell code. For instructions that can't be run in native code, the function returns in such a way that the interpreter knows to run that instruction and then re-enter native code right after. This is called a "call-out" in this design. Thus, one unsupported instruction doesn't demote the rest of the function to the interpreter. 

A few tricky concerns to be aware of for the design:

* Haskell has a moving garbage collector, so native code should never retain heap pointers after returning control back to the interpreter. Otherwise those pointers could get relocated and the native code would be left with a stale pointer.
* Native code must preemptible in the same way as other Haskell functions.
* Native code should keep the same conceptual model of the stack being growable. We do not want perfectly cromulent functional programs to crash because of a randomly low / fixed C stack limit. 

**Goals**

- No change to language semantics, the continuation representation, or serialized code.
- Large speedups on most Unison code, especially straight line code that doesn't use fancy features like abilities or delimited continuations.
- Support only a subset of `MCode`, and grow it over time. Anything unsupported runs in the interpreter, with identical behaviour.

Note that code for a definition never changes, so compiled code never needs invalidating!

**Non-goals, for now**

- Compiling `Capture`, `Reset`, `Jump` or ability handlers natively.
- Making compiled code callable as an ordinary Haskell function, or making it follow GHC's calling conventions.
- Compatibility with the Racket / `run.native` backend. This backend is defunct.
- Caching compiled code on disk across runs. If compilation is fast enough, this may never be worth it.

## Background: the runtime the JIT builds on

Our existing machinery has already, before compilation, converted the code to a set of  _supercombinators_ (occasionally "combinators" in this document). A supercombinator is a function definition in A-normal form that contains no inner functions or lambdas and which can only call other supercombinators. The body of a supercombinator is a value of type `MCode` (for "machine code"). 

Here are the relevant types:

- **MCode** (`Unison.Runtime.MCode`). Each combinator (`CombIx`) is a `GSection` tree: `Ins`, `Let`, `App`, `Call`, `Match`/`DMatch`/`NMatch`/`RMatch`, `Yield`, `Jump`, `Die`, `Exit`. Instructions (`GInstr`) include `Prim1`/`Prim2`, `Pack`/`Unpack`, `ForeignCall`, `Capture`, `Reset` and so on. Known, saturated calls are already marked as `Call`, as opposed to the general `App`.
- **Stack** (`Unison.Runtime.Stack`). It has three pointers, `ap` (arg pointer), `fp` (frame pointer) and `sp` (stack pointer), an unboxed `MutableByteArray` (`ustk`) and a boxed `MutableArray Closure` (`bstk`). Both arrays are unpinned and are reallocated when the stack grows. Instructions address slots relative to `sp`, so the layout at each program point is fixed statically. `ap` matters for frames: `saveFrame` records a frame's size as `sp - fp` and its pending-argument count as `fp - ap`, and `restoreFrame` rebuilds `ap` from that count.
- **Continuation** `K`. A chain of frames: `Push` (frame size, pending args, resumption `CombIx` + `RSection`, stack guard), `Mark`/`AMark` (prompts) and `Local`/`Keep`/`CB`. `Capture` copies stack segments plus `K` frames up to a prompt.
- **Builtins.** Arithmetic and a few other operations are `Prim1`/`Prim2` instructions. Most builtins (around 460 of them, including all of Text, Bytes and IO) are `ForeignCall` instructions that run a Haskell function on the stack. Some of these can signal an exception to be routed to the current `{Exception}` handler; the interpreter's `exec` returns a flag for that.
- **Every `Let` body has a `CombIx`.** A `Let` body is not a supercombinator: it's a position inside one, the point a non-tail call returns to. But the existing compiler gives each one a `CombIx` and stores its code under that name, so that continuations can be serialized. The JIT uses these names for [re-entry points](#re-entry-points).
- **Values.** `Closure` wraps `GClosure`: `GPAp`, `GEnum`, `GData1`, `GData2`, `GDataG`, `GCaptured`, `GForeign`, `GUnboxedTypeTag`, `GAffine`, `GBlackHole`. Unboxed values are a `ustk` word plus a type-tag closure in the matching `bstk` slot.

## Approach: a batched supercombinator JIT

The unit of compilation is a batch of supercombinators, compiled as a single LLVM module. It is highly beneficial to compile multiple related supercombinators together, so that inlining and further optimizations can be applied that cross function boundaries. We use the following scheme:

* Each supercombinator yet to be native-compiled (an "interpreted" function) keeps a count of how many times it has been called.
* When this count reaches N (default 100), it is said to be "hot" and the definition is queued (idempotently) for a background compile thread that submits batches of definitions for compilation. Each supercombinator's [native code cell](#native-code-cells) holds a state: not requested, queued, or taken by the compile thread.
* We use the dependency graph of the code to pull related definitions into the same batch. At the moment, the compile thread forms a batch by iterating breadth-first over the hot definition's interpreted dependencies and dependents (what it calls, and what calls it), up to a batch of size B. A definition is taken only if it is in use: its own request is already waiting in the queue, or, for a dependency, it has been called at least N/2 times, or for a dependent, at least once. The dependents direction tends to matter more since a callee is called at least as often as its caller, so it usually gets hot first. 
* Once compiled, pointers to native code for each supercombinator are installed while the program runs, one pointer write per supercombinator. Nothing assumes a batch appears all at once.

We also allow small functions already compiled to get compiled again and re-included as a private copy in future batches. This adds some code duplication but allows for more inlining.

Later, we may also perform dynamic specialization. For instance, a call to `foldLeft` with `(Nat.+)` might produce a specialized version of `foldLeft`, with `(Nat.+)` effectively inlined into the body.

One tricky part of the JIT is producing "re-entry" functions. Whenever native code exits for a call-out midway through a function, deferring to the interpreter, we want the continuation from that point in the function to also (eventually) get a native definition. We call these "re-etnry functions" and they are generated on-demand. Needed re-entry functions are compiled in batches, or when requests for compilation have paused briefly.  

### Uniform representation of native functions

Every native-compiled function has a wrapper with the same signature, explained below. This is sometimes called the "Unison calling convention" in this document. The uniform signature is what the interpreter calls. (Note: each supercombinator also gets compiled as a separate *worker* that takes arguments as ordinary parameters and returns its result as an unboxed LLVM tuple. The wrapper calls the worker, and within a batch, workers can call each other and themselves directly without having to stash values on the Unison stack.) 

```c
typedef STATUS (*UnisonNativeFn)(Ctx *ctx, int64_t ap, int64_t fp, int64_t sp);
```

Here, `Ctx` is a per-thread value with pointers to the Unison boxed and unboxed stacks, a writeable `ap`, `fp`, and `sp`, among other things, exact representation TBD. The stack pointers appear twice on purpose: the `ap`, `fp` and `sp` *arguments* are the values on entry to the function, and the `Ctx` fields are where native code writes the values on exit, since a C function can only return the one `STATUS`. The interpreter will twiddle the `Ctx` as needed before calling into native code, and native code will also write information back to the `Ctx` before returning.

`STATUS` is an `int64_t` where:

* `OK = 0` means the function returned normally
* `EXIT_ERROR = -1` means the function returned with an error, details stashed in `ctx`. Other negative values are reserved.
* positive values are an exit, a 1-based index into the global `Exit` table, described below

Two pieces of global state connect the interpreter and native code:

1. A *native code cell* for each supercombinator: one pointer-sized slot that is either null (the function is still being run interpreted) or holds the `FunPtr UnisonNativeFn` for its compiled code. Cells live outside the Haskell heap and are read by both the interpreter and native code. See [Native code cells](#native-code-cells).
2. The exits table, containing values of type `Exit` (defined below) which tells the interpreter what to do when returning from native code. This is an ordinary Haskell structure, since an `Exit` holds Haskell heap objects (`RSection`, `GInstr`). Native code never reads it: it only returns an index into it, as a constant baked into the generated code. (Each supercombinator has a fixed number of possible exits, known at compilation time, so it's no problem to give each supercombinator a range in the global exits table.)

```haskell
data Exit
  = Resume (RSection Val) -- interpret from this section
  | CallOut (GInstr Val) (RSection Val) Int (Ptr NativeCell)
    -- run one instruction in Haskell (it pushes this many values), then
    -- call the function in this cell, which continues with the section
    -- after the instruction. The section is interpreted instead when the
    -- cell is still empty (its code is generated on demand, or not yet
    -- installed) or when the instruction pushed a different number of
    -- values; if it raised an exception, the section is the continuation
    -- after the handler.
  | GrowStack Int (Ptr NativeCell)
    -- grow the Unison stack to fit a frame of this size, then call
    -- the function in this cell again
  | Reenter (Ptr NativeCell)
    -- a suspension point: native code has handed control back so the
    -- Haskell runtime can switch threads or collect garbage; when this
    -- thread next runs, call the function in this cell again
```

(The real type, in `Exits.hs`, also records the combinator each exit belongs to, for statistics. The exits name cells rather than code pointers because code is installed while the program runs: an exit can be taken before its continuation's cell has been written, and a call-out's re-entry function may not have been generated at all yet, see [Re-entry points](#re-entry-points).)

`Resume` is straightforward, it's just interpreted code to run on resume. `CallOut` is needed so that a single exit in the middle of a function doesn't cause the whole rest of the function to run interpreted. 

`CallOut` only applies to instructions (`GInstr`), which the interpreter runs and then resumes using the native re-entry function pointer (if non-null) or the given `RSection` (if the re-entry function hasnt' yet been compiled).

Both of these rely on [re-entry points](#re-entry-points), described below: ways back into the middle of a function's native code.

| Situation | Exit kind |
| --- | --- |
| Any instruction without a native version | call-out, re-enter after the instruction |
| Unsupported control *section* (`Jump`, `RMatch`, `Die`, …), or `Capture` / `Discard` | resume at that section |
| A call native code can't make, in a `Let` binding: the callee isn't compiled, or it's an `App` of a function value in one of the [cases left to the interpreter](#calling-function-values) | record the `Push` frame for the `Let` body, resume at the binding. Native code is re-entered at the `Let` body when the callee returns. |
| The same, in tail position | resume at the `App` / `Call` section |
| Unison stack needs to grow | `GrowStack`: the function exits at its entry check, before doing anything else. The interpreter grows the stack and calls the same function again. See [Stack guards](#stack-guards). |
| Preemption poll, or allocation budget used up | `Reenter`: the function exits at its entry poll. Once the Haskell runtime has done what it needed to, the interpreter calls the same function again. See [Preemption](#preemption). |
| Combinator returns | not an exit: status `OK`, which goes to `yield` |
| `Die`, or an error from a foreign call | not an exit: status `EXIT_ERROR`, with the message in `ctx` |

#### Re-entry points

A compiled function can exit to the interpreter for a few reasons:

* To grow the stack
* To allow for GC or thread preemption
* A call-out to have the interpreter run an instruction with no native equivalent

In each of these cases, we may have to resume the interrupted function midway through its execution.  For example, consider:

```
f n =
  x = g n      -- non-tail call
  x + 10       -- the Let body
```

If `g` returns normally, native `f` just carries on into `x + 10`, and no re-entry point is involved. If `g` exits to the interpreter, we need a re-entry function for the continuation from that point in `f`.

We generate re-entry functions eagerly for resumptions after a call-out (since these locations are statically known), but the others are generated lazily, if and when they are needed.

Properties of re-entry points:

- **Each one is a `UnisonNativeFn`.** It starts by reading its locals from the Unison stack slots, which is always possible because of [the core rule](#entering-and-exiting-native-code), and begins with the same [stack check](#stack-guards) as any native function.
- **No code is duplicated.** The code after a call-out lives only in the re-entry function.
- **They're optional.** A `Let` with no re-entry point is resumed in the interpreter, which is always correct. So they can be added gradually.
- **Native code never looks one up;** only the interpreter does. A call-out's re-entry point is a cell held by its `Exit` (a cell rather than a bare pointer because the re-entry function can itself exit with `GrowStack` or `Reenter`, which say "call this function again"). A `Let`'s re-entry point is the native code of the `Let` body's own combinator: the MCode emitter already makes every `Let` body a combinator whose arguments are the whole frame, so calling it is resuming at the body. Its [native code cell](#native-code-cells) is carried by the `Let` node and by the `Push` frame, and `yield` checks it with one load when it pops the frame.

#### Native code cells

The cell is defined by its layout, not by a type: `NativeCell` in `MCode.hs` is an empty data type used only as the pointer's tag, and the accessors there (`readNativeCode`, `bumpNativeCount`, `claimNativeCell`, `readNativeVerdict`, ...) read and write it by byte offset. The C side has no declaration at all: generated code holds a cell's address as a constant and loads the code pointer from offset 0. As a C struct it would be

```c
struct NativeCell {
  UnisonNativeFn code;   // null until compiled
  int64_t count;         // calls made while code was null, from minus the threshold
  int64_t state;         // 0 not requested, 1 queued, 2 taken by the compile thread
  int64_t verdict;       // the JIT's decision on the combinator (see readNativeVerdict)
};
```

Both the interpreter and native code need to answer the same question before calling a function: has it been compiled, and if so, where is the code? The answer is kept in one place per supercombinator, its *native code cell*.

**What a cell is.** The mutable part of a supercombinator, 32 bytes: a function pointer, a counter, a request state and a verdict. The pointer is null until the supercombinator is compiled, and after that it holds the address of its `UnisonNativeFn`. The counter counts the times the interpreter ran the code itself because the pointer was null. It starts at minus the compilation threshold, so that "hot" is the counter reaching zero, which costs the interpreter one comparison with a constant. The third word is the request state: the interpreter moves it from "not requested" to "queued" with a compare-and-swap when it asks for the supercombinator to be compiled, and the compile thread moves it to "taken" when it puts it in a batch. A definition's state is the one on its entry combinator. The fourth word is the compile thread's verdict on whether the supercombinator is worth compiling, and whether the interpreter should enter its code, (see "What isn't compiled" below).

**Where cells live.** Outside the Haskell heap, so they never move and the GC ignores them. Each supercombinator's `GCombInfo` gets a new field holding the address of its cell, next to its arity and frame size.

**Who reads a cell, and how they find it:**

| Reader | How it finds the cell |
| --- | --- |
| The interpreter, at a `Call` or `App` | from the `GCombInfo` of the supercombinator it's about to run, which it already has in hand |
| Native code calling a known function | the cell's address is a constant in the generated code |
| Native code [calling a function value](#calling-function-values) | from the `GCombInfo` inside the closure |

In each case it's one load and a test for null. If the cell is null, the interpreter interprets the function, and native code exits to the interpreter.

**How native code gets a cell's address.**

- **For a known function,** the JIT compiler puts it there. The compiler is Haskell code running in the same process. When it compiles a `Call`, it has the callee's `GCombInfo`, so it reads the cell's address from it and writes that number into the generated code as a constant. This is safe because the cell never moves, and because compiled code is never saved to disk, so the address only has to be valid in this process.

  For example, if the callee's cell is at address `0x7f3a12004e80`, which is 139887386840704 in decimal, the generated LLVM IR for the call starts like this:

  ```llvm
  %fn = load ptr, ptr inttoptr (i64 139887386840704 to ptr)   ; read the cell
  %is_null = icmp eq ptr %fn, null
  br i1 %is_null, label %exit_to_interpreter, label %do_call
  ```

- **For a function value,** native code reads it at run time. It loads the closure pointer from the boxed stack, clears the tag bits, and loads the cell's address from a fixed offset inside the closure. This relies on the address being stored in the `GPAp` closure as a raw word, which is what GHC should do with this field since `GCombInfo` is unpacked into `GPAp`. Verify this when the field is added.
- **Calls within a batch don't use cells.** The callee is in the same LLVM module, so the call refers to it directly, and LLVM can inline it.

**Lifecycle.**

- **Cells are allocated when code is loaded.** Every supercombinator gets one, whether or not it's ever compiled. The builtin combinators (`Nat.increment` and the like, which exist as ordinary MCode so that they can be passed as values) get theirs when the runtime starts.
- **Cells are attached during resolution.** Supercombinators exist in two forms. In the *unresolved* form, references to other supercombinators are `CombIx` names; this is what the compiler emits and what's read from a compiled program file. In the *resolved* form, which is what the interpreter runs and what closures contain, those references point to the supercombinators themselves. Only resolved supercombinators can run, so only they need cells: an unresolved `GCombInfo` holds a null cell address, and resolution fills in the real one. Resolution is a pure function today and allocating cells needs IO, so the loader allocates a block of cells first and passes it in.
- **Installing compiled code is one pointer write.** Readers on other threads see either null or the function pointer, and both are valid. Callers never need recompiling when a callee is compiled later.
- **Cells are never freed.** Code isn't unloaded, and a cell is 32 bytes.
- **A cell's address is only meaningful within one process.** It isn't part of the serialized form of code. The serializer for compiled program files writes a supercombinator's fields one by one, so it simply doesn't write this one, and the file format is unchanged.

**What isn't compiled** (from M6). Native code saves the interpreter's overhead, a few nanoseconds per instruction; an exit and the way back cost several times that. A function whose body is one foreign call would be slower compiled. So before a function is compiled, its MCode is walked to estimate, for an average path from entry to return, the overhead native code would save (one unit per instruction or branch, three per call or `let`, nothing for an instruction the interpreter has to run anyway) and the number of exits it is *certain* to take: instructions with no native version, calls to handlers, and calls to functions that are themselves not worth compiling. Branch arms count equally, except arms that end in an error. If the exits times E (the cost of a round trip in the same units, `UNISON_JIT_EXIT_COST`, 7 by default) exceed the saving, the function is left to the interpreter and its cell records the verdict. Callees that haven't been judged are judged on the way, so the answer doesn't depend on the order in which functions get hot, and "not worth compiling" spreads to callers that do little but call such functions. Exits that only happen on a slow path (the poll, stack growth, a fast path's miss) don't count. A call that returns to the function counts as an exit to the extent that the callee is likely to exit (its own certain exits, at most one): when a callee exits, the caller's frame is unwound with it and the rest of the caller is entered again through the trampoline. In a function that calls itself, the arms that loop or recurse weigh four times the others. What the rule can't see is the behaviour of function values: a loop that calls a closure which turns out not to be native takes an exit per iteration.

The same estimate gives a third verdict, *too small to enter*: a function whose average path saves less than entering native code from the interpreter costs (`UNISON_JIT_ENTRY_COST`, 3 units, about 20 ns) and that has no loop. It is compiled, and native callers call it directly, but when the interpreter is the caller it interprets the function as if it had no code.

**Why not a table.** An earlier version of this design kept function pointers in a global table indexed by a number assigned to each supercombinator. A `CombIx` can't be the index: it's a `Reference`, a number for the top-level definition, and a bit-packed section number that's mostly gaps. So the JIT would have needed its own numbering, and a table layout that could grow while other threads read it. Cells need neither, and a lookup is one load where the table needed two.

**Cells for re-entry points.** The existing compiler creates a `GCombInfo` for each `Let` body, and its cell is the `Let`'s re-entry point. The other re-entry points (after call-outs, and for `Let`s inside inline bindings) are auxiliary functions in the same module as their function; the JIT allocates a cell for each when it compiles the module, and the exit or frame-table entry that leads there holds it.

**Cost.** `GCombInfo` is unpacked into every partial application closure, so each of those grows by one word.

### Entering and exiting native code

**The core rule:** whenever native code hands control back to Haskell, the Unison stack (`ustk`, `bstk`, `ap`, `fp`, `sp`) will be exactly as the interpreter would have left it at that `MCode` point. Native code may do other stuff internally, keeping values in registers, using a different representation of the call stack, etc, but must produce the correct Unison stack before returning.

This has some consequences:

- Native-compiled supercombinators that have to exit back to the interpreter will need to write to the stack and construct `K` frames on demand. At the time of compilation, we have enough information to be able to construct code that will do this for each possible exit point of the function. (Each continuation point is just an `MCode` subtree that's easily available because of the A-normal form nature of the code.)
- For native-to-native calls, the caller will check whether the callee is returning normally (because it completed) or if it is exiting back to the interpreter partway through the function. If exiting back to the interpreter, the caller will record what's needed to build its own `K` frame (see [On dynamically constructing `K` frames](#on-dynamically-constructing-k-frames)) and then itself exit.
- `Capture` (for capturing a delimited continuation) won't ever be run in native code. Instead, at every point where we would capture, the native code will be exiting back to the interpreter, and producing ordinary stack segments and `K` frames as it unwinds. Thus, we don't need LLVM code to have a concept of continuations at all, it's just another unsupported language feature that exits to the interpreter whenever it's hit.

#### On dynamically constructing `K` frames 

**What a `K` frame is.** `K` (in `Stack.hs`) is the interpreter's continuation: a linked list, allocated on the Haskell heap, that says what to do after the current code returns. Each constructor in the list is one frame:

- `Push`: "when the current call returns, continue at this section." It records the caller's frame size, pending argument count, a stack guard, and the `CombIx` + `RSection` to resume at. The interpreter pushes one at every non-tail call (`Let`).
- `Mark` / `AMark`: prompts installed by ability handlers (`Reset`). `Capture` copies the list up to the matching prompt.
- `Local`, `Keep`, `CB`, `KE`: saved handler state, values kept alive, callbacks, and the empty continuation.

**The problem.** When the interpreter makes a non-tail call, it allocates a `Push` frame first, so that `K` always says where to continue afterwards. Native code doesn't do this. When native `f` calls native `g`, the fact that "`f` continues after `g` returns" exists only as a return address on the C stack. If `g` returns normally, that's all that's needed, and no `Push` frame is ever allocated. But if `g` has to exit to the interpreter, the interpreter needs a `K` that includes a frame for `f`, or it won't know to continue with the rest of `f` when `g`'s work is done.

**The approach: native code records, Haskell builds.** Native code can't construct a `Push` frame itself. A `Push` is a Haskell heap object that points to a `CombIx`, an `RSection` and the rest of `K`, and we'd rather native code know nothing about how `K` is represented. So the work is split:

- Native code writes down a *frame record*, three integers, for each frame that needs to exist.
- After native code returns, the interpreter turns the frame records into real `Push` frames.

A frame record contains:

| Field | Where it comes from |
| --- | --- |
| `Let` body id | a constant in the generated code, identifying which `Let` body to continue with |
| frame size | `sp - fp` at the call site, a runtime value |
| pending-argument count | `fp - ap` at the call site, a runtime value |

These are the same two sizes the interpreter's `saveFrame` computes. The remaining fields of a `Push` (the `CombIx`, the stack guard and the `RSection`) are fixed for a given `Let`, so they're kept in a Haskell-side *frame table* indexed by `Let` body id, filled in when the function is compiled. This is the same arrangement as the exits table: native code only ever holds an index.

Frame records are written to a buffer in `Ctx`. After every non-tail native call, the generated code does the equivalent of:

```c
STATUS r = g(ctx, sp, sp, sp);
if (r != OK) {
  // g is exiting, so we are too. Record the frame the interpreter
  // would have pushed before calling g, then pass g's status along.
  add_frame_record(ctx, LET_BODY_ID, sp - fp, fp - ap);
  return r;
}
// g returned normally: continue with the Let body
```

**Example.** The interpreter, whose continuation is `k0`, calls native `a`. Then `a` calls `b`, `b` calls `c`, and `c` reaches a `Capture`, which it can't run.

| Step | What happens | Frame records in `Ctx` |
| --- | --- | --- |
| 1 | `c` writes its live values to the stack and returns exit index 17 | none |
| 2 | `b` sees a non-`OK` status, records its frame, returns 17 | `b` |
| 3 | `a` sees a non-`OK` status, records its frame, returns 17 | `b`, `a` |
| 4 | The interpreter builds `Push b (Push a k0)` | |
| 5 | The interpreter looks up exit 17 and resumes at the `Capture` with that `K` | |

At step 5 the stack and `K` are exactly what they'd be if the interpreter had run `a`, `b` and `c` itself, and the C stack holds nothing. `Capture` works unchanged. Later, when `yield` pops the frame for `b`, it finds the [re-entry point](#re-entry-points) for that `Let` in `b` and goes back into native code.

Some details:

- **Order.** Records are written innermost caller first, since that's the order the exit passes through them. `K` has the innermost frame on top, so the interpreter builds from the last record to the first.
- **Inlined `Let`s.** Native code may compile a `Let` binding inline instead of calling it as a function. If it exits partway through that binding, the exiting function itself writes the record for that `Let` before returning. So a function's own enclosing `Let`s come first, then its callers'. Which `Let`s enclose each exit point is known at compile time.
- **Buffer size.** Each record corresponds to one native call in progress, and the C stack guard (see [Stack guards](#stack-guards)) already limits how deep those can nest, so the buffer has a fixed size with room for that many records.
- **Cost.** Nothing on the normal path beyond the status check. On an exit, a few stores per native frame, plus one `Push` allocation per frame in Haskell.

#### How the interpreter interacts with native code

Native code never calls into Haskell, and the interpreter never calls native code directly from the middle of its own logic. Every transition in either direction goes through one piece of Haskell code, the *trampoline*. It's called that because control bounces off it: native code returns to the trampoline, which decides what runs next, which may be more native code.

**When the interpreter hands off.** The interpreter looks for native code in the two places where it would otherwise start interpreting:

- **When calling a function:** `Call`, and `App` whose target is a known supercombinator. It checks the supercombinator's [native code cell](#native-code-cells) for a main entry point.
- **When returning into a function:** `yield`, when it pops a `Push` frame. It checks whether that `Let` has a [re-entry point](#re-entry-points). This is how execution gets back into native code after an exit.

If it finds native code, the interpreter sets up the call exactly as it would for an interpreted one (arguments moved into place, stack grown if needed) and then passes the function pointer to the trampoline instead of interpreting the body.

**What the trampoline does.**

1. **Fill in `Ctx`.** Write the current addresses of `ustk`, `bstk` and the constant pool, plus the limits native code checks against (C stack, allocation budget), and empty the frame record buffer. This happens on every entry, because the stack arrays may have been moved by the GC or reallocated since the last one.
2. **Call the native function** with an `unsafe` foreign call, passing `Ctx` and the current `ap`, `fp` and `sp`. Native code runs, possibly through many native-to-native calls, until the outermost function returns a `STATUS`.
3. **Read back the stack pointers** and any frame records. The C entry point copies them out of `Ctx` into a few spare words that the unboxed stack keeps past its last slot, so a round trip allocates nothing (from M6: a round trip is about 37 ns, and every allocation or table lookup on this path showed in the profile). The arrays themselves are the same ones the interpreter already holds.
4. **Build `K` frames** from any frame records, on top of the `K` the trampoline was entered with (see [On dynamically constructing `K` frames](#on-dynamically-constructing-k-frames)).
5. **Act on the `STATUS`:**

| `STATUS` | What the trampoline does next |
| --- | --- |
| `OK` | The function finished and its result is on the stack. Hand the result to the continuation with `yield`, as when an interpreted function returns. |
| exit index of a `Resume` | Continue interpreting at the recorded section. |
| exit index of a `Reenter` | Nothing, beyond being back in Haskell, where the runtime can switch threads or run a GC. Then go back to step 1 with the same function. |
| exit index of a `GrowStack` | Grow the Unison stack with the interpreter's `ensure`, then go back to step 1 with the same function. |
| exit index of a `CallOut` | Run the one recorded instruction with the interpreter's `exec`. Then go back to step 1 with the re-entry function, so the rest of the function runs natively. If the instruction raised an exception, don't re-enter: route it to the current exception handler, as the interpreter does for that instruction (the handler's continuation is the re-entry function, so the rest of the function is still native afterwards). |
| `EXIT_ERROR` | Read the error details from `Ctx` and throw the Haskell exception the interpreter would have thrown. |

**Properties worth noting.**

- **Neither stack grows across transitions.** By the time native code returns to the trampoline, all its C stack frames are gone. On the Haskell side, the trampoline's moves (`yield`, continue interpreting, re-enter native code) are all tail calls. So a program can go back and forth between native and interpreted code indefinitely.
- **The interpreter and native code interleave freely.** An interpreted function can call a native one, which can exit to the interpreter for an unknown call, whose callee may itself be native. Each side only ever sees the shared state: the Unison stack and `K`.
- **Nothing else in the interpreter changes.** Besides the checks above, the interpreter runs as it does today. When it resumes after an exit it can't tell that native code was involved, because of [the core rule](#entering-and-exiting-native-code).
- **Cost of a transition.** Entering native code costs an `unsafe` foreign call plus a few stores into `Ctx`. That's cheap, but not free, which is why native-to-native calls go direct and don't pass through the trampoline.

### Calling function values

Higher-order code is everywhere in Unison, so native code has to be able to call a function it was passed. In `List.map f xs`, the call to `f` is an `App` whose target is a value on the stack, not a supercombinator known at compile time. If each such call meant an exit to the interpreter, `List.map` would exit once per element.

**What a function value is.** For an ordinary function or lambda, it's a `GPAp` closure, which holds:

| Field | Meaning |
| --- | --- |
| `CombIx` | which supercombinator it refers to |
| `GCombInfo` | that supercombinator's arity, frame size, code, and the address of its [native code cell](#native-code-cells) |
| captured arguments | values already supplied, such as a lambda's free variables |

**What native code does.**

1. Check that the value is a `GPAp`. This is a test on the pointer's tag bits.
2. Read the arity and the number of captured arguments.
3. If captured plus supplied equals the arity, the call is exactly saturated. Copy the supplied arguments onto the Unison stack, then the captured ones above them, which is the layout the interpreter's `apply` produces.
4. Read the address of the native code cell from the closure's `GCombInfo`, and load the function pointer from the cell.
5. Call it like any other native function: a tail call if the `App` is in tail position, and otherwise a call followed by the status check.

Compared to a call to a known function, this adds a few loads and compares.

The interpreter's side of the same thing: when the interpreter applies a function value itself (`apply` on a `GPAp`), it checks the callee's cell just as `enter` does for a known callee, so a compiled function is entered natively however it was reached.

**A known combinator used as a value** (`App (Env f)` with no arguments, which is how a lambda becomes a closure) is a `GPAp` with nothing captured: a constant, kept in the [pool](#appendix-the-constant-pool) and built once.

**What still exits to the interpreter.**

| Case | Why |
| --- | --- |
| The callee isn't compiled yet | there's nothing to call; it will be compiled once it's hot |
| Too many arguments | the extra ones stay pending and are applied to the result, which needs more machinery |
| Too few arguments | the result is a new `GPAp`. Native code could allocate it, but this can wait. |
| The value isn't a `GPAp`, for example a captured continuation | rare |

These exits work as described for [calls that native code can't make](#uniform-representation-of-native-functions): in a `Let` binding, write a frame record and `Resume` at the binding; in tail position, `Resume` at the `App`.

**Later: specialization.** A hot call to `List.map` with a particular `f` can get its own copy of `List.map` with `f` inlined, which removes the indirect call altogether.

### Tail calls in native code

Loops in Unison are tail calls, so native code must run them without using any C stack.

- **A self tail call** is compiled as a branch back to the top of the function. The loop state is in the argument slots (or registers), so this is an ordinary native loop.
- **A tail call to another function** uses LLVM's `musttail`, which guarantees the call reuses the caller's C stack frame. Under the C calling convention `musttail` requires the caller and callee to have the same signature, which the uniform `UnisonNativeFn` type provides. Workers have differing signatures, so they use LLVM's `tailcc` convention, which guarantees tail calls without that restriction (see below).
- **A tail call returns the callee's `STATUS` directly** to the caller's caller, so there's no status check and no `K` frame to record.

Only non-tail calls use C stack, and those are covered by the [stack guards](#stack-guards).

### Workers: arguments and results in registers

(From M6.) The uniform signature passes everything through the Unison stack: the caller stores the arguments, the callee loads them, and the result comes back the same way. That is right for the trampoline and for calls through a cell, but between functions compiled together it is all overhead, and LLVM can't remove it, even when it inlines, because the stack is reachable from `Ctx`.

So a function whose every return yields exactly one value is compiled as a **worker**: an internal LLVM function `(ctx, fp, u1, b1, ..., un, bn) -> {status, u, b}` with LLVM's `tailcc` convention. The arguments are parameters, the result is two return values, and the Unison stack isn't written at all unless something exits. The function its cell points to becomes a **wrapper** with the uniform signature: load the arguments from the stack, call the worker, store the result. Calls between functions of one module go to the worker directly, and an inlined worker is plain SSA code in its caller.

- **The frame keeps its place on the Unison stack.** A worker is passed the frame pointer its frame would have, and its caller's stack check leaves room for its arguments there. When it exits, it writes its slots to that place, and each native caller above it writes its own frame and frame record as the status passes up, exactly as before. So the interpreter sees the same stack either way, and re-entry points, which the trampoline enters with the frame on the stack, keep the uniform signature and call workers like any other caller.
- **No pending arguments.** A worker isn't passed `ap`: native callers never pass pending arguments, and the wrapper hands a call that has them (an over-application) to the interpreter before the worker runs.
- **Tail calls.** Worker to worker is a real tail call (`musttail`, which `tailcc` allows between different signatures). A worker can't tail call a function with the uniform signature, since the return types differ. Instead it puts the arguments on the stack and returns the status "tail call this" with the code pointer; the wrapper at the bottom makes the call as a real tail call, and a worker that had called this one with a plain call makes it as a plain call. Either way the C stack doesn't grow.
- **Checks.** A worker checks the Unison stack, the poll and the C stack once, at entry (the C stack check used to be at each call site), and looks at the allocation budget only if it allocates. The high-water mark for marking the boxed stack is raised where a worker writes the stack, on its exit paths, instead of at every entry.
- **The fast entry.** Many functions start with a test and return at once in the base case. For such a worker a second, tiny function is what callers call: it holds only the paths that can't exit, call or loop, which need nothing checked, and returns from them directly; any other path tail calls the full worker, which starts again from the top (what it repeats is a few instructions without side effects). The base case then costs no stack check, no poll, and none of the register saving the full worker does on behalf of its exit paths. This was worth 15% on `fib`.
- **Cold paths are marked cold.** Every branch to an exit or a slow path carries a branch weight. Without them LLVM kept the values the exit paths need in callee-saved registers, which every call then paid to save and restore; this alone took `fib 20` from 89 to 74 µs before workers existed.

### Growing the stack and handling preemption

Native code can't grow a stack or give up the CPU by itself. Both are handled the same way: native code checks a condition at known points, and if the check fires, it exits to the interpreter, which does the work in Haskell and then calls back into native code.

#### Stack guards

There are two stacks to protect, and each has its own check.

**The C stack, checked before each non-tail native call.** Unison programs can recurse deeply. Above a depth limit, exit instead of calling, and the rest runs from the interpreter with heap-allocated frames. The limit is the stack pointer compared with a limit in `Ctx`, set at entry from the thread's stack bounds minus a reserve (D13). GHC's worker threads on macOS get 512 KB, which leaves about 250 KB for native frames, roughly 1600 frames of a small function; beyond that, recursion alternates between native and interpreted frames, one interpreted `Let` per limit hit. A dedicated large stack for native runs would make that rare, and is in the ideas document.

**The Unison stack, checked once at function entry.** Every supercombinator has a maximum frame size that's known at compile time: the most Unison stack it can use, including the argument slots for any call it makes. Every native function starts by checking that a frame of that size fits in the space remaining. This is the check the interpreter does with `ensure` when it enters a supercombinator.

- **If the check fails,** the function returns a `GrowStack` exit before doing anything else. Its arguments are already in their stack slots, so there's nothing to write back. The interpreter grows the stack and calls the same function again, and this time the check passes.
- **Exits never need more stack.** Once the entry check has passed, every slot the function could write, including those written on its exit paths, is inside the space that was checked.
- **It applies to every native function,** which includes both kinds of [re-entry point](#re-entry-points).
- **Why at entry and not at the call site.** It's one check per function, not one per call. It also works for calls to function values, where the caller doesn't know the callee's frame size.
- Only the interpreter reallocates the stack arrays, so their addresses stay valid for as long as native code runs.

#### Preemption

Native code must not monopolize the CPU. Haskell threads aren't preempted at arbitrary points: the runtime's timer (every 20 ms by default) sets a flag on the capability, and compiled Haskell code notices the flag the next time it allocates, then hands control to the scheduler. Native code inside an `unsafe` foreign call never reaches that check. Left alone, a long-running native loop would:

- starve the other Haskell threads on its capability,
- stall the whole program whenever any thread needs a GC, since a collection waits for every capability to stop,
- delay `killThread` and timeouts, which are only delivered once the thread is back in Haskell.

So native code polls, and exits when asked to:

- **What's checked.** Two things: the capability's context-switch request (the runtime sets `HpLim` to null when it wants the thread to stop), and an allocation budget kept in `Ctx` as an address, the point the bump pointer would reach by allocating everything the budget allows; the poll fires once the bump pointer has passed it. The bump limit is the nearer of the block's end and that address, so the budget costs the fast path nothing.
- **Where.** At the entry of every native function. That includes the top of a loop, since a self tail call branches back there. Unison has no loops other than calls, so any code that runs for an unbounded time must keep entering functions, and everything between two function entries is straight-line code of bounded length.
- **What happens when a poll fires.** The function returns a `Reenter` exit. At function entry its arguments are in their stack slots, so there's nothing to write back; at the top of a loop, the loop variables are written to their slots on the exit path. Back in Haskell, the runtime switches threads or runs the GC as it would for any Haskell code. When this thread next runs, the interpreter calls the same native function again, and it carries on where it left off.
- **Cost.** A load, a compare and a branch that's almost never taken, per function entry.

### Memory and GC

Native code allocates real Haskell heap objects. This is safe because GHC can't run a GC on this capability during an `unsafe` foreign call, and native code keeps no heap pointers once it returns.

**Allocation:**

- Use the RTS's `allocate(cap, nWords)`, then write the info pointer and fields directly.
- Each `Pack` calls `allocate` once for everything it builds; a constructor's fields are already evaluated, so nothing can exit between the call and the last store. Several consecutive `Pack`s could share one call as long as no instruction that can exit sits between them (an exit would leave the later objects unwritten, which the debug runtime's heap walker would object to). That, and bump allocation inline, are in [optimization-ideas.md](optimization-ideas.md).
- Read constructor info pointers (`GData1`, `GData2`, `GDataG`, `GEnum`, type-tag closures, …) at startup. Derive field layouts by probing sample closures rather than hard-coding them, because the `UNPACK` pragmas on `GClosure`/`Val` affect the layout, and they only take effect in optimized builds.
- `GDataG` holds a `Seg`, a tuple of a `ByteArray` and an `Array`. A tuple's fields can't be unpacked, so the closure points at two lifted boxes that each point at an array: building one is five objects, and reading a field is three loads. Native code matches this representation; a flat one would be an interpreter change (see the ideas document).
- Taking a constructor apart needs its field count, which for three or more fields isn't in the closure's pointer tag. The interface registers every data type's constructor arities with the JIT as it loads them.
- Heap constants (`Reference`s, type tags, `GCombInfo`, literals) come from the constant pool (see [Appendix: the constant pool](#appendix-the-constant-pool)), never from addresses baked into code.

**Pointer tagging.** Pointers to evaluated constructors carry the constructor number in their low bits. `GClosure` has ten constructors, more than the seven tag values available, so only the first six (`GPAp`, `GEnum`, `GData1`, `GData2`, `GDataG`, `GCaptured`) are identified by the tag alone; for the rest the info table has to be consulted. Native code must untag before reading fields and must store correctly tagged pointers when it writes into `bstk` or a constructor field.

**Thunks.** `bpoke` is strict, and every field of `GClosure` and `Val` is strict, so everything reachable from `bstk` should be in weak head normal form, and native code doesn't have to evaluate anything. A debug-mode assertion that every pointer read from `bstk` or a field has a nonzero tag catches violations cheaply.

**Write barrier.** GHC's collector is generational, and a minor GC doesn't scan old-generation objects unless it has been told they changed. `bstk` is a long-lived mutable array, so it's in the old generation, and a pointer stored into it without telling the GC can leave a new object looking unreachable: it gets collected, and `bstk` is left pointing at garbage. Haskell's `writeArray#` handles this on every write by marking the array's header dirty and marking the *card* (one byte per 128 elements) that covers the slot.

Native code does this once per return to Haskell, not once per write. No GC can run during the `unsafe` call, so the marks only have to be in place by the time native code returns:

- Native code stores into `bstk` directly, with no barrier.
- Before returning to Haskell (on `OK`, an exit or an error), set the array's header to `stg_MUT_ARR_PTRS_DIRTY_info` and mark every card covering the slots native code could have written: from the `ap` it was entered with, up to the highest `sp` it reached. That's usually one or two bytes.
- This lives in one place, the wrapper's return path or a small helper it calls, which keeps it off the fast path and makes it easy to test.

`ustk` holds no pointers and needs no barrier. Other mutable objects native code writes to need their own: `Ref.write` on a `MutVar` calls a C helper that stores the value and then calls the runtime's `dirty_MUT_VAR`, exactly as compiled Haskell does (it moves a clean variable onto the mutable list); `MutableArray.write` stores, sets the array's info pointer to the dirty one and marks the card, again as compiled Haskell does, with no runtime call. Marking the header and cards has been tested and is sufficient on GHC 9.10.3: with it, the debug runtime's heap sanity checks pass, and without it they fail (see the runtime spike in `jit-spikes/runtime`). One thing still to verify: that the non-moving collector, which needs a different barrier, isn't in use.

**Why the allocation budget is required, not just polite.** The allocation budget is checked by the [preemption](#preemption) poll. `allocate` takes blocks from the nursery, and once the nursery is empty it takes fresh blocks from the block allocator without limit. GC is only triggered when a *Haskell* heap check fails after the nursery is exhausted; nothing native code does triggers one, and a null `HpLim` on its own is treated by the scheduler as a context switch, not as a GC request. So the budget is what bounds memory growth: after a budget exit, the interpreter's own allocation (the trampoline allocates `K` frames and `Stack` records) hits a heap check within a block or so, and the GC runs. This needs an explicit test: a native loop that allocates heavily must show bounded resident memory and regular GCs.

**Lists, text and bytes** (from M6). A Unison `List` is a `Unison.Util.Deque` (a strict finger tree) and a `Text` is a `Unison.Util.Rope`: the same finger tree with chunks of characters for elements and sizes counted in characters, sharing the Deque's code and constructors below the top level. `Bytes` is the same rope with chunks of bytes; the C rope functions take a description of the kind of chunk (its constructor, where its fields are, whether counts are characters or bytes), and that is the only difference between the text and the bytes helpers. Both are strict in every field, so everything reachable from one is an evaluated constructor, and native code can take them apart and build new ones with no thunk in sight. The array builtins (mutable and immutable, pointer and byte arrays), `Ref` and `Ticket` are C helpers in the same style (from 2026-10-05): the Haskell bounds checks reproduced exactly, the heap objects built as the RTS primops build them (dirty info pointers, card tables, `allocatePinned` for pinned arrays, `casMutVar#`'s barrier), and a failing check handed to the interpreter. (Lists used to be `Data.Sequence`, whose lazy spine made that impossible in general; a strict structure replaced it for this reason.) The operations themselves are C helpers in `cbits/jit_rt.c` that generated code calls directly, the way it calls the allocator:

- A helper is handed the closure from the stack, checks that it is what it expects, does the operation by reading the constructors and allocating the result with `allocate` (charging the allocation budget), and returns the result. It never calls into Haskell, so the core rule is untouched.
- The helpers are ports of the Haskell operations and take every case, for lists (size, `List.at`, the views and splits that list patterns compile to, `cons`, `snoc`, `++`, `take`, `drop`, list literals) for text and bytes (every primitive: size, `++`, `take`, `drop`, equality and the orderings, `uncons`/`unsnoc`, numbers to and from text, pack and unpack, `indexOf`, `Bytes.at` and `flatten`; and the pure foreign functions: `Text.repeat`, `reverse`, the ASCII case mappings, UTF-8 to and from bytes, `Char.toText`, the Bytes number encodings and reads, base 16/32/64); universal `==`, `<`, `<=` and `compare` on two texts or two bytes go through the same helpers. A helper that can't decide a case exactly (a number in a form the Haskell lexer might read differently, an encoding that isn't canonical, a Unicode case mapping) answers "not handled" and the interpreter does it. A helper returns "not handled" only for a closure that isn't a list, a text or a bytes, and generated code then takes the instruction's slow path, the ordinary call-out. The price is that a helper has to be kept in step with the Haskell operation it ports; the startup check below and a longer randomized test (`UNISON_JIT_STRESS=lists=N`, `texts=N`, `bytes=N`) are what hold them together.
- The constructor layouts are GHC's, not ours, so they are checked at startup like the closure layouts: the helpers learn the constructors from sample values, a C walk checks the shape and the stored counts of every constructor in a set of samples, and each helper is then run against the Haskell operation it stands in for. If anything differs, the JIT stays off. The four modules that define these types (`Deque.Internal`, `Rope`, `Text`, `Bytes`) are compiled with `-O2 -funbox-strict-fields` in every build, so that the layouts don't change with the build's optimization level (`UNPACK` is ignored at `-O0`; the bytes check caught exactly that).
- The linker drops C functions that nothing refers to, and only generated code refers to these, so the runtime keeps a table of them.
- A pointer taken from a stack slot may be untagged: the interpreter stores closures with `bpoke`, and a top-level constant stored as itself (such as `noneClo`) puts the static closure's untagged address there, on every store, however many times the CAF has been forced. Native code checks the pointer tag before reading through such a pointer (a match on it sends tag 0 to the interpreter) and before storing it into a strict field (a constructor's or a `Val`'s, in `Pack`, list literals and pushes, `Ref` and array writes), because GHC's optimized code reads strict fields without evaluating them. Arrays and the stack itself may hold untagged pointers.

**Exceptions.** Haskell exceptions can't unwind through native frames. Native code never calls Haskell, and errors come back as `EXIT_ERROR` with a payload that the trampoline rethrows.

## Alternatives considered

| Alternative | Why not (for now) |
| --- | --- |
| Tracing JIT | Little to gain from runtime type discovery in a statically typed language. Handles non-tail recursion badly. Specialization at call sites covers the higher-order case. |
| Generating Haskell/Core and compiling with GHC at runtime | Correct GC by construction, but compiles take seconds, and the GHC API plus a runtime linker would have to ship in the binary. Still useful as a quick experiment to see how much of the overhead is dispatch. |
| Closure compilation (MCode → a tree of Haskell closures) | No native code, so a lower ceiling. Worth building as a baseline to measure against. |
| Full GHC-ABI native code (`ghccc`, info tables, CPS, `stg_gc_*`) | Essentially a new GHC backend. GC bugs are very hard to find, it's tightly coupled to GHC versions, and it requires LLVM. |
| Hybrid: `foreign import prim` + `ghccc` + bump allocation on `Hp`, exiting when the heap check fails | Cheaper allocation and preemption, with no stack maps. Kept as an upgrade path if allocation or entry/exit costs dominate. |

## Risks and open questions

**Risks**

- **Write-barrier and layout bugs corrupt the heap silently.** Mitigations: a debug mode that uses a slow, Haskell-checked path; the existing `STACK_CHECK` sentinels; and tests that run a fully JIT'd program under the interpreter and compare results.
- **GHC coupling.** `allocate` and the `rCurrentAlloc` block whose free pointer native code bumps, info-pointer lookup, the array barrier and the `HpLim` poll depend on RTS internals. Check them on every GHC upgrade.
- **Frequent exits could erase the gains.** Call-outs keep a `ForeignCall` from demoting the rest of a function to the interpreter, but each one still costs two foreign calls, so builtins used in hot code need native implementations over time. Calls to function values are handled natively in the common case, but over- and under-saturated calls still exit. Profile exit and call-out counts from the beginning.
- **Missing polls stall the whole process.** Every native function must poll at its entry, including re-entry points, and the compiler should check for this.
- **Linking LLVM statically may be hard.** Statically linking LLVM and the C++ runtime into a GHC-built binary on macOS (arm64 and x86-64), Linux and Windows (which needs a mingw-built LLVM to match GHC's toolchain) could take a while and affects everyone's build. Static linking is preferred but not required: if a time-boxed build spike shows it's painful, dynamically link a pinned LLVM (shared library found at runtime, with the JIT disabled and a warning logged if it's missing), and revisit static linking later. Do the spike before writing any codegen, but don't let it block the project.

**Open questions**

- [x] LLVM ORC or Cranelift for the first backend? Decided: LLVM ORC.
- [x] Does the binary need to work without any external toolchain at runtime? Decided: ideally yes, by statically linking the LLVM components we use (OrcJIT, JITLink, the native target, the optimization passes). ORC links code in memory, so no clang or system linker is needed at runtime. This adds tens of MB to the binary, plus the C++ runtime library. If static linking turns out to be hard, that's acceptable: fall back to a dynamically linked LLVM, with the JIT disabled when the library isn't available (see [Risks](#risks-and-open-questions)).
- [x] Which Haskell builtins (`Text`, `Bytes`, arithmetic on `Nat`/`Int`) to reimplement natively so they don't cause exits? Decided: start with arithmetic, comparison, bitwise and conversion operations on Int, Nat, Float, Char and Boolean (with exactly the interpreter's overflow and truncation semantics), plus array read, write and size. Bounds failures exit, and the interpreter raises the error. Skip Text and Bytes for now. Other easy candidates: Ref.read and Ref.write (with the write barrier), and tag and equality tests on data.
- [x] How does this interact with the Racket/`run.native` backend going forward? Decided: that backend is defunct and will be deleted if the JIT succeeds.

## Appendix: the constant pool

Native code needs some Haskell heap objects: the `Reference` written into every constructor by `Pack`, the type-tag closures for unboxed values, constructors without fields, `GCombInfo` for building partial applications, and `Lit` values. These objects move on every GC, so their addresses can't be baked into machine code. Instead there is a *constant pool*: a `MutableArray Closure` owned by Haskell, whose address is passed to native code on every entry, exactly like `bstk`, through `Ctx`. Generated code refers to constants by pool index. Addresses are stable for the duration of an `unsafe` call, so this is safe for the same reason reading `bstk` is.

There is one global pool rather than one per module, because native code in one module calls into another without passing through the trampoline, so there would be no moment to switch pools. Entries are interned by key when a module is compiled, so two modules using the same `Reference` share an index. The array grows by copying, and superseded arrays are kept alive so that an address read at entry stays valid for the rest of that native run. Because code is installed while native code runs, a run can call a function that is newer than the array it holds. So a new entry is also written to the superseded arrays where it fits, and a function that uses an index beyond the size of the pool's first array checks the size of the array it was given at entry; if it is too small, it exits and is entered again with the current array. A `Reference` is kept as the field of a prebuilt field-less constructor of its type.
