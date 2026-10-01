# JIT benchmarks

Run this against the `jit_codebase` codebase, which has the `jit-tests` project with the base
library and the timing helpers (`printTime`, `repeat`) from `@pchiusano/misc-benchmarks` in it.
Use an optimized build:

```
stack build && stack exec unison -- -C jit_codebase transcript.fork unison-src/transcripts-manual/jit-benchmarks.md
```

The timings are printed to the console as the transcript runs.

These measure what the JIT compiles, without lists or functions that have Haskell replacements.

``` unison
structural type Cons a = Nil | Cons a (Cons a)

Cons.range : Nat -> Nat -> Cons Nat
Cons.range lo hi =
  go acc i = if i == lo then Cons.Cons i acc else go (Cons.Cons i acc) (i - 1)
  if hi <= lo then Cons.Nil else go Cons.Nil (hi - 1)

Cons.map : (a ->{g} b) -> Cons a ->{g} Cons b
Cons.map f = cases
  Cons.Nil -> Cons.Nil
  Cons.Cons h t -> Cons.Cons (f h) (Cons.map f t)

Cons.foldLeft : (b ->{g} a ->{g} b) -> b -> Cons a ->{g} b
Cons.foldLeft f z = cases
  Cons.Nil -> z
  Cons.Cons h t -> Cons.foldLeft f (f z h) t

structural type Tree = Leaf | Node Tree Nat Nat Tree

Tree.insert : Nat -> Nat -> Tree -> Tree
Tree.insert k v = cases
  Tree.Leaf -> Tree.Node Tree.Leaf k v Tree.Leaf
  Tree.Node l k2 v2 r ->
    if k < k2 then Tree.Node (Tree.insert k v l) k2 v2 r
    else if k > k2 then Tree.Node l k2 v2 (Tree.insert k v r)
    else Tree.Node l k v r

Tree.lookup : Nat -> Tree -> Optional Nat
Tree.lookup k = cases
  Tree.Leaf -> None
  Tree.Node l k2 v r ->
    if k < k2 then Tree.lookup k l
    else if k > k2 then Tree.lookup k r
    else Some v

Tree.build : Nat -> Tree
Tree.build n =
  go t i =
    if i == n then t
    else go (Tree.insert (Nat.mod (i * 7919) 10007) i t) (i + 1)
  go Tree.Leaf 0

Tree.lookupAll : Nat -> Tree -> Nat
Tree.lookupAll n t =
  go found i =
    if i == n then found
    else match Tree.lookup (Nat.mod (i * 7919) 10007) t with
      None -> go found (i + 1)
      Some _ -> go (found + 1) (i + 1)
  go 0 0

sumTo : Nat -> Nat
sumTo n =
  go acc i = if i > n then acc else go (acc + i) (i + 1)
  go 0 0

fib : Nat -> Nat
fib n = if n < 2 then n else fib (n - 1) + fib (n - 2)

applyN : Nat -> (a ->{g} a) -> a ->{g} a
applyN n f a = if n == 0 then a else applyN (n - 1) f (f a)

refLoop : Nat ->{IO} Nat
refLoop n =
  r = IO.ref 0
  go i =
    if i == n then Ref.read r
    else
      Ref.write r (Ref.read r + i)
      go (i + 1)
  go 0

-- A hot loop that calls small functions from other definitions: what
-- batching and direct calls are for.
collatzStep : Nat -> Nat
collatzStep n = if Nat.mod n 2 == 0 then n / 2 else 3 * n + 1

collatzSteps : Nat -> Nat
collatzSteps n =
  go steps x = if x == 1 then steps else go (steps + 1) (collatzStep x)
  go 0 n

collatzTotal : Nat -> Nat
collatzTotal n =
  go acc i = if i > n then acc else go (acc + collatzSteps i) (i + 1)
  go 0 1

jitSuite : '{IO, Exception} ()
jitSuite = do
  printTime "Sum 0 to 1 million" 1 (n -> repeat n do sumTo 1000000)
  printTime "fib 20" 1 (n -> repeat n do fib 20)
  printTime "Cons list: map with a lambda (1000 elements)" 1 let
    c = Cons.range 0 1000
    n -> repeat n do Cons.map (x -> x + 1) c
  printTime "Cons list: foldLeft with a lambda (1000 elements)" 1 let
    c = Cons.range 0 1000
    n -> repeat n do Cons.foldLeft (acc x -> acc + x) 0 c
  printTime "Binary tree: 1000 inserts" 1 (n -> repeat n do Tree.build 1000)
  printTime "Binary tree: 1000 lookups" 1 let
    t = Tree.build 1000
    n -> repeat n do Tree.lookupAll 1000 t
  printTime "Apply a function argument 10000 times" 1 (n -> repeat n do applyN 10000 (x -> x + 3) 0)
  printTime "Mutate a Ref 10000 times" 1 (n -> repeat n do refLoop 10000)
  printTime "Calls across definitions: Collatz steps for 1 to 1000" 1 (n -> repeat n do collatzTotal 1000)
```

``` ucm
jit-tests/main> run jitSuite
```
