# Lambda case syntax

```ucm:hide
.> builtins.merge
```

This function takes a single argument and immediately pattern matches on it. As we'll see below, it can be written using `cases` syntax:

```unison
isEmpty x = match x with
  [] -> true
  _ -> false
```

```ucm:hide
.> add
```

Here's the same function written using `cases` syntax:

```unison
isEmpty2 = cases
  [] -> true
  _ -> false
```

Notice that Unison detects this as an alias of `isEmpty`, and if we view `isEmpty`

```ucm
.> view isEmpty
```

it shows the definition using `cases` syntax opportunistically, even though the code was originally written without that syntax.

## Multi-argument cases

Functions that take multiple arguments and immediately match on a tuple of arguments can also be rewritten to use `cases`. Here's a version using regular `match` syntax on a tuple:

```unison:hide
merge : [a] -> [a] -> [a]
merge xs ys = match (xs, ys) with
  ([], ys) -> ys
  (xs, []) -> xs
  (h +: t, h2 +: t2) ->
    if h <= h2 then h  +: merge t (h2 +: t2)
    else            h2 +: merge (h +: t) t2
```

```ucm
.> add
```

And here's a version using `cases`. The patterns are separated by commas:

```unison
merge2 : [a] -> [a] -> [a]
merge2 = cases
  [], ys -> ys
  xs, [] -> xs
  h +: t, h2 +: t2 ->
    if h <= h2 then h  +: merge2 t (h2 +: t2)
    else            h2 +: merge2 (h +: t) t2
```

Notice that Unison detects this as an alias of `merge`, and if we view `merge`

```ucm
.> view merge
```

it again shows the definition using the multi-argument `cases` syntax opportunistically, even though the code was originally written without that syntax.

Here's another example:

```unison
type B = T | F

blah = cases
  T, x -> "hi"
  x, F -> "bye"

> blah T F
> blah F F
```
