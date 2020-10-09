# Lambda case syntax

This function takes a single argument and immediately pattern matches on it. As we'll see below, it can be written using `cases` syntax:

```unison
isEmpty x = match x with
  [] -> true
  _ -> false
```

```ucm

  I found and typechecked these definitions in scratch.u. If you
  do an `add` or `update`, here's how your codebase would
  change:
  
    ⍟ These new definitions are ok to `add`:
    
      isEmpty : [𝕩] -> Boolean

```
Here's the same function written using `cases` syntax:

```unison
isEmpty2 = cases
  [] -> true
  _ -> false
```

```ucm

  I found and typechecked these definitions in scratch.u. If you
  do an `add` or `update`, here's how your codebase would
  change:
  
    ⍟ These new definitions are ok to `add`:
    
      isEmpty2 : [𝕩] -> Boolean
        (also named isEmpty)

```
Notice that Unison detects this as an alias of `isEmpty`, and if we view `isEmpty`

```ucm
.> view isEmpty

  isEmpty : [𝕩] -> Boolean
  isEmpty = cases
    [] -> true
    _  -> false

```
it shows the definition using `cases` syntax opportunistically, even if the code was originally written without that syntax.

## Multi-argument cases

Functions that take multiple arguments and immediately match on a tuple of arguments can also be rewritten to use `cases`:

```unison
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

  ⍟ I've added these definitions:
  
    merge : [a] ->{𝕖} [a] ->{𝕖} [a]

```
Here's a version using `cases`:

```unison
merge2 : [a] -> [a] -> [a]
merge2 = cases
  [] ys -> ys
  xs [] -> xs
  (h +: t) (h2 +: t2) ->
    if h <= h2 then h  +: merge2 t (h2 +: t2)
    else            h2 +: merge2 (h +: t) t2
```

```ucm

  I found and typechecked these definitions in scratch.u. If you
  do an `add` or `update`, here's how your codebase would
  change:
  
    ⍟ These new definitions are ok to `add`:
    
      merge2 : [a] ->{𝕖} [a] ->{𝕖} [a]

```
Notice that Unison detects this as an alias of `merge` (it doesn't, but should), and if we view `merge`

```ucm
.> add

  ⍟ I've added these definitions:
  
    merge2 : [a] ->{𝕖} [a] ->{𝕖} [a]

.> names merge

  Term
  Hash:   #eesob77j7q
  Names:  merge

.> names merge2

  Term
  Hash:   #ouo7ronu9q
  Names:  merge2

.> view merge2 merge

  merge : [a] -> [a] -> [a]
  merge = cases
    [] ys               -> ys
    xs []               -> xs
    (h +: t) (h2 +: t2) ->
      if h <= h2 then h +: merge t (h2 +: t2)
      else h2 +: merge (h +: t) t2
  
  merge2 : [a] -> [a] -> [a]
  merge2 = cases
    [] ys               -> ys
    xs []               -> xs
    (h +: t) (h2 +: t2) ->
      if h <= h2 then h +: merge2 t (h2 +: t2)
      else h2 +: merge2 (h +: t) t2

```
it again shows the definition using the multi-argument `cases syntax opportunistically, even though the code was originally written without that syntax.
