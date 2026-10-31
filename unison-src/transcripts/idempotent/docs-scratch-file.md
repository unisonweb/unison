# `docs` for definitions that are only in the scratch file

`docs` shows the type signature (or the heading of a type) and then the documentation, so you can preview the docs
while you are still writing them.

```unison:hide
{{ Adds one to a number. }}
inc : Nat -> Nat
inc n = n + 1

{{ A pair of things. }}
type Pair a b = Pair a b
```

```ucm
scratch/main> load
scratch/main> docs inc
scratch/main> docs Pair
```
