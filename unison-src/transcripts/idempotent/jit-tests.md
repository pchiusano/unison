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

## Call-outs

Instructions native code has no version of are run by the interpreter, which then re-enters
native code after them. A foreign call that fails goes to the current `Exception` handler
instead, and a call-out inside an inline `let` binding re-enters in the middle of that binding.
A `let` inside such a binding gets its own re-entry point too.

``` unison
use Nat + - * / == < > <= >=

catchFailure : '{IO, Exception} a ->{IO} Either Text a
catchFailure c = handle c() with cases
  { a } -> Right a
  { Exception.raise f -> _ } -> match f with Failure _ msg _ -> Left msg

readPlus : MutableArray {IO} Nat -> Nat ->{IO, Exception} Nat
readPlus arr i = MutableArray.read arr i + 1

catching : '{IO} (Either Text Nat, Either Text Nat)
catching = do
  arr = IO.arrayOf 7 3
  (catchFailure '(readPlus arr 1), catchFailure '(readPlus arr 5))

callOutInBinding : Nat ->{IO} Nat
callOutInBinding n =
  r = IO.ref n
  x = if n == 0 then 1 else Ref.read r + 1
  y = if x > 100 then Ref.read r else x * 2
  y + 1

callOutTest : '{IO} (Nat, Nat, Nat)
callOutTest = do (callOutInBinding 0, callOutInBinding 20, callOutInBinding 200)

sizePlus : Nat -> Nat
sizePlus n = Text.size (Nat.toText n) + n

letInIf : Nat -> Nat
letInIf n =
  x = if n > 0 then
        y = sizePlus n
        y + 1
      else 0
  x * 2

> (letInIf 0, letInIf 41)

fill : MutableArray {IO} Nat -> Nat ->{IO, Exception} ()
fill arr i =
  if Nat.eq i (MutableArray.size arr) then ()
  else
    MutableArray.write arr i (i * i)
    fill arr (i + 1)

sumArr : MutableArray {IO} Nat -> Nat -> Nat ->{IO, Exception} Nat
sumArr arr i acc =
  if Nat.eq i (MutableArray.size arr) then acc
  else sumArr arr (i + 1) (acc + MutableArray.read arr i)

arrayTest : '{IO} (Either Text Nat, Either Text Nat, Either Text ())
arrayTest = do
  arr = IO.arrayOf 0 1000
  (catchFailure do
     fill arr 0
     sumArr arr 0 0,
   catchFailure '(MutableArray.read arr 1000),
   catchFailure '(MutableArray.write arr 1000 7))
```

``` ucm :added-by-ucm
  Loading changes detected in scratch.u.

  + arrayTest        : '{IO} ( Either Text Nat,
                         Either Text Nat,
                         Either Text ())
  + callOutInBinding : Nat ->{IO} Nat
  + callOutTest      : '{IO} (Nat, Nat, Nat)
  + catchFailure     : '{IO, Exception} a ->{IO} Either Text a
  + catching         : '{IO} (Either Text Nat, Either Text Nat)
  + fill             : MutableArray {IO} Nat
                       -> Nat
                       ->{IO, Exception} ()
  + letInIf          : Nat -> Nat
  + readPlus         : MutableArray {IO} Nat
                       -> Nat
                       ->{IO, Exception} Nat
  + sizePlus         : Nat -> Nat
  + sumArr           : MutableArray {IO} Nat
                       -> Nat
                       -> Nat
                       ->{IO, Exception} Nat

  Run `update` to apply these changes to your codebase.

    37 | > (letInIf 0, letInIf 41)
           ⧩
           (0, 88)
```

``` ucm
scratch/main> run catching

  (Right 8, Left "MutableArray.read: array index out of bounds")

scratch/main> run callOutTest

  (3, 43, 201)

scratch/main> run arrayTest

  ( Right 332833500
  , Left "MutableArray.read: array index out of bounds"
  , Left "MutableArray.write: array index out of bounds"
  )
```

## Function values

A call to a function value is native when the value is a closure with compiled code and the
call is exactly saturated. Over-application (`mk 1 2`, where `mk 1` returns a function) and
under-application (`adder 5`, which builds a closure) are left to the interpreter.

``` unison
use Nat + - * / < > <= >=

applyN : Nat -> (a -> a) -> a -> a
applyN n f a = if Nat.eq n 0 then a else applyN (Nat.drop n 1) f (f a)

twice : (Nat -> Nat) -> Nat -> Nat
twice f x =
  y = f x
  f y

adder : Nat -> Nat -> Nat
adder k x = x + k

mk : Nat -> (Nat -> Nat)
mk k = adder (k * 10)

compose2 : (Nat -> Nat) -> (Nat -> Nat) -> Nat -> Nat
compose2 f g x = f (g x)

sumWith : (Nat -> Nat) -> Nat -> Nat
sumWith f n =
  go acc i = if Nat.eq i n then acc else go (acc + f i) (i + 1)
  go 0 0

> applyN 10000 (x -> x + 3) 0
> twice (adder 5) 10
> applyN 3 (adder 7) 1
> compose2 (adder 1) (adder 2) 3
> applyN 5 (twice (adder 1)) 0
> mk 1 2
> applyN 4 (mk 2) 0
> sumWith (x -> x * x) 100
> sumWith (compose2 (mk 1) (adder 1)) 10
```

``` ucm :added-by-ucm
  Loading changes detected in scratch.u.

  + adder    : Nat -> Nat -> Nat
  + applyN   : Nat -> (a ->{g} a) -> a ->{g} a
  + compose2 : (Nat ->{g1} Nat)
               -> (Nat ->{g} Nat)
               -> Nat
               ->{g, g1} Nat
  + mk       : Nat -> Nat -> Nat
  + sumWith  : (Nat ->{g} Nat) -> Nat ->{g} Nat
  + twice    : (Nat ->{g} Nat) -> Nat ->{g} Nat

  Run `update` to apply these changes to your codebase.

    25 | > applyN 10000 (x -> x + 3) 0
           ⧩
           30000

    26 | > twice (adder 5) 10
           ⧩
           20

    27 | > applyN 3 (adder 7) 1
           ⧩
           22

    28 | > compose2 (adder 1) (adder 2) 3
           ⧩
           6

    29 | > applyN 5 (twice (adder 1)) 0
           ⧩
           10

    30 | > mk 1 2
           ⧩
           12

    31 | > applyN 4 (mk 2) 0
           ⧩
           80

    32 | > sumWith (x -> x * x) 100
           ⧩
           328350

    33 | > sumWith (compose2 (mk 1) (adder 1)) 10
           ⧩
           155
```

## Bits, booleans, numeric matches and top-level values

Counting bits, `not` on booleans that are values rather than branch conditions, a function
whose body starts with a match on a number, and top-level values that are evaluated once
when they are loaded.

``` unison
use Nat + - * / == < > <= >=

bits : Nat -> Nat
bits n =
  go acc i =
    if i == n then acc
    else go (acc + Nat.leadingZeros i + Nat.trailingZeros i + Nat.popCount i) (i + 1)
  go 0 0

structural type Flags = Flags Boolean Boolean

countFlags : Nat -> Nat
countFlags n =
  step = cases Flags a b -> Flags (Boolean.not b) a
  go f acc i =
    if i == n then acc
    else match f with
      Flags a b -> go (step f) (if Boolean.not a then acc + 1 else if b then acc + 2 else acc) (i + 1)
  go (Flags true false) 0 0

fibMatch : Nat -> Nat
fibMatch = cases
  0 -> 0
  1 -> 1
  n -> fibMatch (n - 1) + fibMatch (n - 2)

limit : Nat
limit = 3 * 1000 + 7

banner : Text
banner = "ab" Text.++ "cd"

useTop : Nat -> Nat
useTop n =
  go acc i = if i == n then acc else go (acc + limit + Text.size banner) (i + 1)
  go 0 0

-- a function that returns a function, called with more arguments than it
-- takes: the extra one is applied to its result
choose : Nat -> Nat -> Nat
choose x = if x == 0 then (y -> y + 1) else (y -> y * 2)

overApply : Nat -> Nat
overApply n =
  go acc i = if i == n then acc else go (acc + choose (Nat.mod i 2) i) (i + 1)
  go 0 0

> bits 1000
> countFlags 1000
> fibMatch 20
> useTop 1000
> (limit, banner)
> overApply 1000
```

``` ucm :added-by-ucm
  Loading changes detected in scratch.u.

  + structural type Flags

  + banner     : Text
  + bits       : Nat -> Nat
  + choose     : Nat -> Nat -> Nat
  + countFlags : Nat -> Nat
  + fibMatch   : Nat -> Nat
  + limit      : Nat
  + overApply  : Nat -> Nat
  + useTop     : Nat -> Nat

  Run `update` to apply these changes to your codebase.

    48 | > bits 1000
           ⧩
           61010

    49 | > countFlags 1000
           ⧩
           1000

    50 | > fibMatch 20
           ⧩
           6765

    51 | > useTop 1000
           ⧩
           3011000

    52 | > (limit, banner)
           ⧩
           (3007, "abcd")

    53 | > overApply 1000
           ⧩
           750000
```

## Lists

List primitives are native (C helpers that read and build the list's structure directly):
size, the views at both ends that pattern matching uses, cons, snoc, `List.at`, append,
take, drop, the patterns that split off a fixed number of elements, and list literals.

``` unison
use Nat + - * / == < > <= >=
use List +: :+

build : Nat -> [Nat]
build n =
  go acc i = if i == n then acc else go (acc :+ i) (i + 1)
  go [] 0

buildFront : Nat -> [Nat]
buildFront n =
  go acc i = if i == n then acc else go (i +: acc) (i + 1)
  go [] 0

sumFront : [Nat] -> Nat
sumFront l =
  go acc = cases
    [] -> acc
    h +: t -> go (acc + h) t
  go 0 l

sumBack : [Nat] -> Nat
sumBack l =
  go acc = cases
    [] -> acc
    i :+ x -> go (acc + x) i
  go 0 l

sumAt : [Nat] -> Nat
sumAt l =
  n = List.size l
  go acc i =
    if i == n then acc
    else match List.at i l with
      Some x -> go (acc + x * (Nat.mod i 3)) (i + 1)
      None -> acc
  go 0 0

-- a queue: add at the back, take from the front, at a steady size
queue : Nat -> Nat -> Nat
queue size n =
  go q acc i =
    if i == n then acc + List.size q
    else match q :+ i with
      h +: t -> go t (acc + h) (i + 1)
      [] -> acc
  go (build size) 0 0

-- small lists, both ends at once, and indices out of range
edges : Nat -> Nat
edges n =
  one = cases
    [] -> 0
    [x] -> x
    x +: (rest :+ y) -> x * 2 + y + List.size rest
  go acc i =
    if i == n then acc
    else
      l = build (Nat.mod i 13)
      missing = match List.at i l with
        None -> 1
        Some _ -> 0
      go (acc + one l + missing) (i + 1)
  go 0 0

-- lists the interpreter made in other ways: appended and cut
mixed : Nat -> Nat
mixed n =
  l = build n List.++ buildFront n
  m = List.drop 37 (List.take (n + 100) l)
  sumFront m + sumBack m + sumAt m + List.size m

-- elements that are boxed values
pairs : Nat -> Nat
pairs n =
  go acc i = if i == n then acc else go (acc :+ ("ab", i)) (i + 1)
  walk acc = cases
    [] -> acc
    (t, k) +: rest -> walk (acc + Text.size t + k) rest
  walk 0 (go [] 0)

-- take, drop and append in a loop, each on the result of the last
chop : Nat -> Nat
chop n =
  go l acc i =
    if i == n then acc + List.size l
    else
      k = Nat.mod (i * 7) (List.size l + 1)
      a = List.take k l
      b = List.drop k l
      l' = (b :+ i) List.++ a
      next = if List.size l' > 600 then List.drop 100 l' List.++ List.take 3 l' else l'
      go next (acc + List.size a * 3 + sumFront (List.take 2 b)) (i + 1)
  go (build 50) 0 0

-- doubling by append, then cuts in the middle
doubled : Nat -> Nat
doubled n =
  go l i = if i == n then l else go (l List.++ (i +: l)) (i + 1)
  l = go [1, 2, 3] 0
  m = List.take (List.size l / 2 + 17) (List.drop (List.size l / 3) l)
  List.size l + sumFront m + sumBack m + sumAt (List.take 5000 m)

-- patterns that split off a fixed number of elements, and literals
splits : Nat -> Nat
splits n =
  step = cases
    [a, b, c] ++ rest -> (a + b * 2 + c * 3, rest)
    rest -> (List.size rest, [])
  back = cases
    rest ++ [y, z] -> y * 5 + z + List.size rest
    _ -> 0
  go l acc =
    match step l with
      (v, []) -> acc + v
      (v, rest) -> go rest (acc + v + back rest)
  go (build n List.++ [n, n + 1, n + 2, n + 3]) 0

> sumFront (build 100000)
> sumBack (buildFront 100000)
> sumAt (build 30000)
> queue 10 100000
> queue 3000 100000
> edges 2000
> mixed 5000
> pairs 20000
> (sumFront [], sumBack [], sumAt [], List.at 0 [1, 2, 3], List.at 3 [1, 2, 3])
> chop 20000
> doubled 14
> splits 30000
> (List.take 2 [1, 2, 3], List.drop 2 [1, 2, 3], [1, 2] List.++ [3], List.take 0 [1], List.drop 5 [1], [] List.++ [7])
```

``` ucm :added-by-ucm
  Loading changes detected in scratch.u.

  + build      : Nat -> [Nat]
  + buildFront : Nat -> [Nat]
  + chop       : Nat -> Nat
  + doubled    : Nat -> Nat
  + edges      : Nat -> Nat
  + mixed      : Nat -> Nat
  + pairs      : Nat -> Nat
  + queue      : Nat -> Nat -> Nat
  + splits     : Nat -> Nat
  + sumAt      : [Nat] -> Nat
  + sumBack    : [Nat] -> Nat
  + sumFront   : [Nat] -> Nat

  Run `update` to apply these changes to your codebase.

    118 | > sumFront (build 100000)
            ⧩
            4999950000

    119 | > sumBack (buildFront 100000)
            ⧩
            4999950000

    120 | > sumAt (build 30000)
            ⧩
            450005000

    121 | > queue 10 100000
            ⧩
            4998950110

    122 | > queue 3000 100000
            ⧩
            4708953000

    123 | > edges 2000
            ⧩
            20594

    124 | > mixed 5000
            ⧩
            38978757

    125 | > pairs 20000
            ⧩
            200030000

    126 | > (sumFront [], sumBack [], sumAt [], List.at 0 [1, 2, 3], List.at 3 [1, 2, 3])
            ⧩
            (0, 0, 0, Some 1, None)

    127 | > chop 20000
            ⧩
            394214577

    128 | > doubled 14
            ⧩
            189028

    129 | > splits 30000
            ⧩
            2850305009

    130 | > (List.take 2 [1, 2, 3], List.drop 2 [1, 2, 3], [1, 2] List.++ [3], List.take 0 [1], List.drop 5 [1], [] List.++ [7])
            ⧩
            ([1, 2], [3], [1, 2, 3], [], [], [7])
```

## Text

Text primitives with native versions (C helpers that read and build the rope directly):
size, `++`, take, drop and equality.

``` unison
use Nat + - * / == < > <= >=
use Text ++

grow : Nat -> Text
grow n =
  go acc i = if i == n then acc else go (acc ++ "ab") (i + 1)
  go "" 0

growFront : Nat -> Text
growFront n =
  go acc i = if i == n then acc else go (Nat.toText i ++ acc) (i + 1)
  go "" 0

-- drop a character at a time
eat : Text -> Nat
eat t =
  go acc t = if Text.size t == 0 then acc else go (acc + Text.size t) (Text.drop 1 t)
  go 0 t

-- take all but the last character, until nothing is left
chop : Text -> Nat
chop t =
  go acc t = if Text.eq t "" then acc else go (acc + 1) (Text.take (Text.size t - 1) t)
  go 0 t

-- a text cut in two and put together again is the same text, in other chunks
recut : Text -> Nat
recut t =
  n = Text.size t
  go acc i =
    if i > n then acc
    else
      u = Text.take i t ++ Text.drop i t
      go (if Text.eq u t then acc + 1 else acc) (i + 7)
  go 0 0

-- characters of more than one byte
wide : Nat -> Nat
wide n =
  go acc t i =
    if i == n then acc + Text.size t
    else go (acc + Text.size (Text.take 3 t)) (Text.drop 1 (t ++ "é€😀")) (i + 1)
  go 0 "λx" 0

> Text.size (grow 20000)
> Text.size (growFront 3000)
> eat (grow 2000)
> chop (growFront 500)
> recut (grow 3000)
> wide 5000
> (Text.eq (grow 3) "ababab", Text.eq (grow 3) "ababa", Text.take 2 (grow 40), Text.drop 77 (grow 40), "" ++ "", Text.size "")
```

``` ucm :added-by-ucm
  Loading changes detected in scratch.u.

  + chop      : Text -> Nat
  + eat       : Text -> Nat
  + grow      : Nat -> Text
  + growFront : Nat -> Text
  + recut     : Text -> Nat
  + wide      : Nat -> Nat

  Run `update` to apply these changes to your codebase.

    45 | > Text.size (grow 20000)
           ⧩
           40000

    46 | > Text.size (growFront 3000)
           ⧩
           10890

    47 | > eat (grow 2000)
           ⧩
           8002000

    48 | > chop (growFront 500)
           ⧩
           1390

    49 | > recut (grow 3000)
           ⧩
           858

    50 | > wide 5000
           ⧩
           25001

    51 | > (Text.eq (grow 3) "ababab", Text.eq (grow 3) "ababa", Text.take 2 (grow 40), Text.drop 77 (grow 40), "" ++ "", Text.size "")
           ⧩
           (true, false, "ab", "bab", "", 0)
```

## Partial applications

A function with some of its arguments supplied is a value built natively (the `Name`
instruction): a copy of the function's closure with the arguments added.

``` unison
use Nat + - * / == < > <= >=

add3 : Nat -> Nat -> Nat -> Nat
add3 a b c = a + b * 2 + c * 3

applyAll : (Nat -> Nat) -> Nat -> Nat -> Nat
applyAll f n acc = if n == 0 then acc else applyAll f (n - 1) (f acc)

-- a closure made fresh in every iteration, from a known function
fresh : Nat -> Nat
fresh n =
  go acc i = if i == n then acc else
    f = add3 i (i + 1)
    go (acc + f 1 + f 2) (i + 1)
  go 0 0

-- more arguments added to a function value that already holds some
more : Nat -> Nat
more n =
  go acc i = if i == n then acc else
    f = add3 i
    g = f (i + 1)
    go (acc + g i + applyAll g 3 0) (i + 1)
  go 0 0

-- five captured values: more than the helper takes, so the interpreter builds it
five : Nat -> Nat
five n =
  go acc i = if i == n then acc else
    a = i + 1
    b = i + 2
    c = i + 3
    d = i + 4
    f x = a + b + c + d + i + x
    go (acc + applyAll f 2 0) (i + 1)
  go 0 0

> fresh 20000
> more 5000
> five 3000
```

``` ucm :added-by-ucm
  Loading changes detected in scratch.u.

  + add3     : Nat -> Nat -> Nat -> Nat
  + applyAll : (Nat ->{g} Nat) -> Nat -> Nat ->{g} Nat
  + five     : Nat -> Nat
  + fresh    : Nat -> Nat
  + more     : Nat -> Nat

  Run `update` to apply these changes to your codebase.

    38 | > fresh 20000
           ⧩
           1200200000

    39 | > more 5000
           ⧩
           562527500

    40 | > five 3000
           ⧩
           45045000
```
