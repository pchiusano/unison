# JIT tests

Programs that exercise the parts of the runtime the JIT compiles. Each one is small and has a
known answer. The transcript is run with the JIT off and on, and the output must be the same.
See `docs/jit-implementation-plan.md`.

This transcript uses only builtins, so it runs in an empty codebase.

``` ucm :hide
scratch/main> builtins.mergeio
```

A few definitions the base library would normally provide.

``` unison :hide
(Nat.-) : Nat -> Nat -> Nat
(Nat.-) a b = Nat.drop a b

(Nat.==) : Nat -> Nat -> Boolean
(Nat.==) a b = Nat.eq a b

(Nat.<) : Nat -> Nat -> Boolean
(Nat.<) a b = Nat.lt a b

(Nat.>) : Nat -> Nat -> Boolean
(Nat.>) a b = Nat.gt a b

(Nat.<=) : Nat -> Nat -> Boolean
(Nat.<=) a b = Nat.lteq a b

(Nat.>=) : Nat -> Nat -> Boolean
(Nat.>=) a b = Nat.gteq a b
```

``` ucm :hide
scratch/main> update
```

## Arithmetic and loops

Self tail calls, arithmetic and comparisons on unboxed values.

``` unison
use Nat + - * / == < > <= >=

sumTo : Nat -> Nat
sumTo n =
  go acc i = if i > n then acc else go (acc + i) (i + 1)
  go 0 0

countDown : Nat -> Nat
countDown n = if n == 0 then 0 else countDown (n - 1)

collatz : Nat -> Nat
collatz n =
  go steps k =
    if k == 1 then steps
    else if Nat.mod k 2 == 0 then go (steps + 1) (k / 2)
    else go (steps + 1) (3 * k + 1)
  go 0 n

intLoop : Int -> Int
intLoop n =
  go acc i = if Int.gt i n then acc else go (acc Int.- i Int.* i) (i Int.+ +1)
  go +0 -10

floatLoop : Nat -> Float
floatLoop n =
  go acc i = if i == n then acc else go (acc Float.+ (Nat.toFloat i Float./ 2.0)) (i + 1)
  go 0.0 0

-- mutual recursion through tail calls to known functions
isEven : Nat -> Boolean
isEven n = if n == 0 then true else isOdd (n - 1)

isOdd : Nat -> Boolean
isOdd n = if n == 0 then false else isEven (n - 1)

> sumTo 1000000
> countDown 1000000
> collatz 27
> intLoop +10
> floatLoop 1000
> (isEven 100000, isOdd 100000)
> (Nat.shiftLeft 1 40, Nat.and 255 1023, Nat.xor 5 3, Nat.pow 3 20, 0 - 1)
```

``` ucm :added-by-ucm
  Loading changes detected in scratch.u.

  + collatz   : Nat -> Nat
  + countDown : Nat -> Nat
  + floatLoop : Nat -> Float
  + intLoop   : Int -> Int
  + isEven    : Nat -> Boolean
  + isOdd     : Nat -> Boolean
  + sumTo     : Nat -> Nat

  Run `update` to apply these changes to your codebase.

    36 | > sumTo 1000000
           ⧩
           500000500000

    37 | > countDown 1000000
           ⧩
           0

    38 | > collatz 27
           ⧩
           111

    39 | > intLoop +10
           ⧩
           -770

    40 | > floatLoop 1000
           ⧩
           249750.0

    41 | > (isEven 100000, isOdd 100000)
           ⧩
           (true, false)

    42 | > (Nat.shiftLeft 1 40, Nat.and 255 1023, Nat.xor 5 3, Nat.pow 3 20, 0 - 1)
           ⧩
           (1099511627776, 255, 6, 3486784401, 0)
```

## Non-tail recursion

``` unison
use Nat + - * / == < > <= >=

fib : Nat -> Nat
fib n = if n < 2 then n else fib (n - 1) + fib (n - 2)

-- deep recursion: must not overflow the C stack
depth : Nat -> Nat
depth n = if n == 0 then 0 else 1 + depth (n - 1)

ackermann : Nat -> Nat -> Nat
ackermann m n =
  if m == 0 then n + 1
  else if n == 0 then ackermann (m - 1) 1
  else ackermann (m - 1) (ackermann m (n - 1))

-- mutual non-tail recursion across two definitions
evenDepth : Nat -> Nat
evenDepth n = if n == 0 then 0 else 1 + oddDepth (n - 1)

oddDepth : Nat -> Nat
oddDepth n = if n == 0 then 1 else 1 + evenDepth (n - 1)

-- three calls deep; only the innermost hands over to the interpreter
inner : Nat -> Nat
inner n = Text.size (Nat.toText n) + n

middle : Nat -> Nat
middle n = inner n + 1

outer : Nat -> Nat
outer n = middle n + 1

-- a call inside a `let` binding's own `let`: its frame is not the function's
letInBinding : Nat -> Nat
letInBinding n =
  x =
    y = inner n
    y + 1
  x * 2

> fib 20
> depth 1000000
> ackermann 2 10
> evenDepth 100001
> outer 41
> letInBinding 41
```

``` ucm :added-by-ucm
  Loading changes detected in scratch.u.

  + ackermann    : Nat -> Nat -> Nat
  + depth        : Nat -> Nat
  + evenDepth    : Nat -> Nat
  + fib          : Nat -> Nat
  + inner        : Nat -> Nat
  + letInBinding : Nat -> Nat
  + middle       : Nat -> Nat
  + oddDepth     : Nat -> Nat
  + outer        : Nat -> Nat

  Run `update` to apply these changes to your codebase.

    41 | > fib 20
           ⧩
           6765

    42 | > depth 1000000
           ⧩
           1000000

    43 | > ackermann 2 10
           ⧩
           23

    44 | > evenDepth 100001
           ⧩
           100002

    45 | > outer 41
           ⧩
           45

    46 | > letInBinding 41
           ⧩
           88
```

## Data: allocation and pattern matching

``` unison
use Nat + - * / == < > <= >=

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

Cons.size : Cons a -> Nat
Cons.size c = Cons.foldLeft (n _ -> n + 1) 0 c

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

Tree.size : Tree -> Nat
Tree.size = cases
  Tree.Leaf -> 0
  Tree.Node l _ _ r -> Tree.size l + 1 + Tree.size r

-- inserts keys in a scrambled order, so the tree is reasonably balanced
Tree.build : Nat -> Tree
Tree.build n =
  go t i =
    if i == n then t
    else go (Tree.insert (Nat.mod (i * 7919) 10007) i t) (i + 1)
  go Tree.Leaf 0

structural type Shape = Circle Nat | Rect Nat Nat | Tri Nat Nat Nat | Dot

area : Shape -> Nat
area = cases
  Shape.Circle r -> 3 * r * r
  Shape.Rect w h -> w * h
  Shape.Tri a b c -> a + b + c
  Shape.Dot -> 0

shapes : Nat -> Nat
shapes n =
  go acc i =
    if i == n then acc
    else
      s = match Nat.mod i 4 with
        0 -> Shape.Circle i
        1 -> Shape.Rect i 2
        2 -> Shape.Tri i i i
        _ -> Shape.Dot
      go (acc + area s) (i + 1)
  go 0 0

-- sums a fresh list of n cells, k times: allocates far more than the
-- nursery, so it needs many collections while native code holds the loop
Cons.sum : Cons Nat -> Nat
Cons.sum = cases
  Cons.Nil -> 0
  Cons.Cons h t -> h + Cons.sum t

churn : Nat -> Nat -> Nat
churn k n =
  go i acc = if i == 0 then acc else go (i - 1) (acc + Cons.sum (Cons.range 0 n))
  go k 0

> Cons.size (Cons.range 0 100000)
> Cons.foldLeft (+) 0 (Cons.map (x -> x * 2) (Cons.range 0 1000))
> Tree.size (Tree.build 5000)
> (Tree.lookup 7919 (Tree.build 5000), Tree.lookup 10008 (Tree.build 5000))
> shapes 10000
> (1, "two", 3.0, ?4, +5, (6, 7))
> churn 40 50000
```

``` ucm :added-by-ucm
  Loading changes detected in scratch.u.

  + structural type Cons a
  + structural type Shape
  + structural type Tree

  + area          : Shape -> Nat
  + churn         : Nat -> Nat -> Nat
  + Cons.foldLeft : (b ->{g} a ->{g} b) -> b -> Cons a ->{g} b
  + Cons.map      : (a ->{g} b) -> Cons a ->{g} Cons b
  + Cons.range    : Nat -> Nat -> Cons Nat
  + Cons.size     : Cons a -> Nat
  + Cons.sum      : Cons Nat -> Nat
  + shapes        : Nat -> Nat
  + Tree.build    : Nat -> Tree
  + Tree.insert   : Nat -> Nat -> Tree -> Tree
  + Tree.lookup   : Nat -> Tree -> Optional Nat
  + Tree.size     : Tree -> Nat

  Run `update` to apply these changes to your codebase.

    88 | > Cons.size (Cons.range 0 100000)
           ⧩
           100000

    89 | > Cons.foldLeft (+) 0 (Cons.map (x -> x * 2) (Cons.range 0 1000))
           ⧩
           999000

    90 | > Tree.size (Tree.build 5000)
           ⧩
           5000

    91 | > (Tree.lookup 7919 (Tree.build 5000), Tree.lookup 10008 (Tree.build 5000))
           ⧩
           (Some 1, None)

    92 | > shapes 10000
           ⧩
           249912515000

    93 | > (1, "two", 3.0, ?4, +5, (6, 7))
           ⧩
           (1, "two", 3.0, ?4, +5, (6, 7))

    94 | > churn 40 50000
           ⧩
           49999000000
```

## Calls to function values

``` unison
use Nat + - * / == < > <= >=

applyN : Nat -> (a ->{g} a) -> a ->{g} a
applyN n f a = if n == 0 then a else applyN (n - 1) f (f a)

compose : (b ->{g} c) -> (a ->{g} b) -> a ->{g} c
compose f g a = f (g a)

add3 : Nat -> Nat -> Nat -> Nat
add3 a b c = a + b + c

-- a closure that captures a variable
adder : Nat -> (Nat -> Nat)
adder k = x -> x + k

-- too few arguments, then the rest
partial : Nat
partial =
  f = add3 1
  g = f 2
  g 3

-- too many arguments: `pick` takes one argument and returns a function
pick : Boolean -> (Nat -> Nat -> Nat)
pick b = if b then (Nat.+) else (Nat.*)

> applyN 100000 (x -> x + 3) 0
> applyN 1000 (adder 7) 1
> applyN 10 (compose (adder 1) (x -> x * 2)) 1
> partial
> (pick true 6 7, pick false 6 7)
```

``` ucm :added-by-ucm
  Loading changes detected in scratch.u.

  + add3    : Nat -> Nat -> Nat -> Nat
  + adder   : Nat -> Nat -> Nat
  + applyN  : Nat -> (a ->{g} a) -> a ->{g} a
  + compose : (b ->{g} c) -> (a ->{g} b) -> a ->{g} c
  + partial : Nat
  + pick    : Boolean -> Nat -> Nat -> Nat

  Run `update` to apply these changes to your codebase.

    27 | > applyN 100000 (x -> x + 3) 0
           ⧩
           300000

    28 | > applyN 1000 (adder 7) 1
           ⧩
           7001

    29 | > applyN 10 (compose (adder 1) (x -> x * 2)) 1
           ⧩
           2047

    30 | > partial
           ⧩
           6

    31 | > (pick true 6 7, pick false 6 7)
           ⧩
           (13, 42)
```

## Builtins implemented in Haskell

Lists, text and bytes are handled by the interpreter, in the middle of compiled code.

``` unison
use Nat + - * / == < > <= >=

listSum : [Nat] -> Nat
listSum = cases
  [] -> 0
  h +: t -> h + listSum t

listRange : Nat -> [Nat]
listRange n =
  go acc i = if i == n then acc else go (acc :+ i) (i + 1)
  go [] 0

listRev : [a] -> [a]
listRev xs =
  go acc = cases
    [] -> acc
    init :+ last -> go (acc :+ last) init
  go [] xs

lastTwo : [Nat] -> Nat
lastTwo = cases
  _ ++ [a, b] -> a + b
  _ -> 0

textLoop : Nat -> Text
textLoop n =
  go acc i = if i == n then acc else go (acc Text.++ Nat.toText i) (i + 1)
  go "" 0

> listSum (listRange 1000)
> listRev (listRange 10)
> lastTwo (listRange 100)
> List.size (listRange 100000)
> textLoop 20
> Text.size (textLoop 1000)
```

``` ucm :added-by-ucm
  Loading changes detected in scratch.u.

  + lastTwo   : [Nat] -> Nat
  + listRange : Nat -> [Nat]
  + listRev   : [a] -> [a]
  + listSum   : [Nat] -> Nat
  + textLoop  : Nat -> Text

  Run `update` to apply these changes to your codebase.

    30 | > listSum (listRange 1000)
           ⧩
           499500

    31 | > listRev (listRange 10)
           ⧩
           [9, 8, 7, 6, 5, 4, 3, 2, 1, 0]

    32 | > lastTwo (listRange 100)
           ⧩
           197

    33 | > List.size (listRange 100000)
           ⧩
           100000

    34 | > textLoop 20
           ⧩
           "012345678910111213141516171819"

    35 | > Text.size (textLoop 1000)
           ⧩
           2890
```

## Abilities and continuations

These run in the interpreter. Compiled code has to hand over to it and take control back
correctly.

``` unison
use Nat + - * / == < > <= >=

structural ability Counter where
  next : Nat

structural ability Abort where
  abort : a

structural ability Choose where
  choose : Boolean

runCounter : Nat -> '{Counter, g} a ->{g} a
runCounter start thunk =
  h : Nat -> Request {Counter} a -> a
  h n = cases
    { a } -> a
    { Counter.next -> k } -> handle k n with h (n + 1)
  handle !thunk with h start

runAbort : '{Abort, g} a ->{g} Optional a
runAbort thunk =
  h : Request {Abort} a -> Optional a
  h = cases
    { a } -> Some a
    { Abort.abort -> _ } -> None
  handle !thunk with h

-- resumes each continuation twice
runChoose : '{Choose, g} a ->{g} [a]
runChoose thunk =
  h : Request {Choose} a -> [a]
  h = cases
    { a } -> [a]
    { Choose.choose -> k } -> (handle k true with h) List.++ (handle k false with h)
  handle !thunk with h

-- an ability operation below several frames of ordinary recursion
sumNext : Nat ->{Counter} Nat
sumNext n = if n == 0 then 0 else Counter.next + sumNext (n - 1)

findFirst : Nat -> Nat ->{Abort} Nat
findFirst target i =
  if i > 1000 then Abort.abort
  else if i * i == target then i
  else findFirst target (i + 1)

bits : Nat ->{Choose} Nat
bits n =
  if n == 0 then 0
  else
    b = if Choose.choose then 1 else 0
    b + 2 * bits (n - 1)

-- a non-tail call whose callee performs an operation: the caller's frame is
-- captured by the handler and resumed twice
pick : Nat ->{Choose} Nat
pick n = if Choose.choose then n else n + 1

addPicked : Nat ->{Choose} Nat
addPicked n =
  x = pick n
  x + 100

-- inline bindings (an `if` bound by a `let`), one of which performs an
-- operation from inside the binding
picky : Nat ->{Choose} Nat
picky n =
  x = if n == 0 then 1 else n
  y = if x > 10 then Nat.drop x 10 else x + 100
  z = if Choose.choose then y else x
  x + y + z

> runCounter 0 '(sumNext 1000)
> (runAbort '(findFirst 144 0), runAbort '(findFirst 145 0))
> runChoose '(bits 3)
> runCounter 10 '(runChoose '(Counter.next + bits 2))
> runChoose '(addPicked 5)
> (runChoose '(picky 5), runChoose '(picky 50))
```

``` ucm :added-by-ucm
  Loading changes detected in scratch.u.

  + structural ability Abort
  + structural ability Choose
  + structural ability Counter

  + addPicked  : Nat ->{Choose} Nat
  + bits       : Nat ->{Choose} Nat
  + findFirst  : Nat -> Nat ->{Abort} Nat
  + pick       : Nat ->{Choose} Nat
  + picky      : Nat ->{Choose} Nat
  + runAbort   : '{g, Abort} a ->{g} Optional a
  + runChoose  : '{g, Choose} a ->{g} [a]
  + runCounter : Nat -> '{g, Counter} a ->{g} a
  + sumNext    : Nat ->{Counter} Nat

  Run `update` to apply these changes to your codebase.

    73 | > runCounter 0 '(sumNext 1000)
           ⧩
           499500

    74 | > (runAbort '(findFirst 144 0), runAbort '(findFirst 145 0))
           ⧩
           (Some 12, None)

    75 | > runChoose '(bits 3)
           ⧩
           [7, 3, 5, 1, 6, 2, 4, 0]

    76 | > runCounter 10 '(runChoose '(Counter.next + bits 2))
           ⧩
           [13, 11, 12, 10]

    77 | > runChoose '(addPicked 5)
           ⧩
           [105, 106]

    78 | > (runChoose '(picky 5), runChoose '(picky 50))
           ⧩
           ([215, 115], [130, 140])
```

## Preemption

A thread stuck in a loop that never allocates must still be killable. Compiled code polls at
every function entry for this.

``` unison
use Nat + - * / == < > <= >=

spin : Nat -> Nat
spin n = spin (n + 1)

killTest : '{IO} Text
killTest = do
  t = IO.forkComp '(spin 0)
  _ = IO.delay.impl 200000
  match IO.kill.impl t with
    Right _ -> "killed"
    Left _ -> "could not kill"
```

``` ucm :added-by-ucm
  Loading changes detected in scratch.u.

  + killTest : '{IO} Text
  + spin     : Nat -> Nat

  Run `update` to apply these changes to your codebase.
```

``` ucm
scratch/main> run killTest

  "killed"
```

## Mutable state and errors

``` unison
use Nat + - * / == < > <= >=

refLoop : Nat ->{IO} Nat
refLoop n =
  r = IO.ref 0
  go i =
    if i == n then Ref.read r
    else
      Ref.write r (Ref.read r + i)
      go (i + 1)
  go 0

safeDiv : Nat -> Nat -> Either Text Nat
safeDiv a b = if b == 0 then Left "divide by zero" else Right (a / b)

refTest : '{IO} Nat
refTest = do refLoop 10000

> (safeDiv 10 2, safeDiv 1 0)
```

``` ucm :added-by-ucm
  Loading changes detected in scratch.u.

  + refLoop : Nat ->{IO} Nat
  + refTest : '{IO} Nat
  + safeDiv : Nat -> Nat -> Either Text Nat

  Run `update` to apply these changes to your codebase.

    19 | > (safeDiv 10 2, safeDiv 1 0)
           ⧩
           (Right 5, Left "divide by zero")
```

``` ucm
scratch/main> run refTest

  49995000
```
