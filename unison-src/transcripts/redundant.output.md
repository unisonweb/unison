The same kind of thing happens with `map`. Are we saying this is incorrect behaviour?

```unison
map : (a -> b) -> [a] -> [b]
map f = cases
  x +: xs -> f x +: map f xs
  [] -> []
```

```ucm

  I found and typechecked these definitions in scratch.u. If you
  do an `add` or `update`, here's how your codebase would
  change:

    ⍟ These new definitions are ok to `add`:

      map : (a ->{𝕖} b) ->{𝕖} [a] ->{𝕖} [b]

  Now evaluating any watch expressions (lines starting with
  `>`)... Ctrl+C cancels.

```
```ucm
.> add

  ⍟ I've added these definitions:

    map : (a ->{𝕖} b) ->{𝕖} [a] ->{𝕖} [b]

.> view map

  map : (a -> b) -> [a] -> [b]
  map f = cases
    x +: xs ->
      use builtin.List +:
      f x +: map f xs
    [] -> []

.> find map

  1. map : (a ->{𝕖} b) ->{𝕖} [a] ->{𝕖} [b]


```
