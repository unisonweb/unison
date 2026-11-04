An update that changes no type gives the updated term's dependents their new hashes without typechecking them again
(\#5790). They get the hashes that printing them and parsing them back would give: loading the definitions that `view`
prints, after the update, finds no changes.

``` ucm :hide
scratch/main> builtins.merge
```

``` unison
helper : Nat -> Nat
helper n = n + 1

direct : Nat
direct = helper 1

indirect : Nat
indirect = direct + helper 2

ping : Nat -> Nat
ping n = if n == 0 then helper 0 else pong (Nat.drop n 1)

pong : Nat -> Nat
pong n = if n == 0 then 0 else ping (Nat.drop n 1)
```

``` ucm :added-by-ucm
  Loading changes detected in scratch.u.

  + direct   : Nat
  + helper   : Nat -> Nat
  + indirect : Nat
  + ping     : Nat -> Nat
  + pong     : Nat -> Nat

  Run `update` to apply these changes to your codebase.
```

``` ucm
scratch/main> add

  Okay, I'm searching the branch for code that needs to be
  updated...

  Done.
```

`helper` keeps its type, so its dependents are not typechecked again; `ping` and `pong` stay one cycle.

``` unison
helper : Nat -> Nat
helper n = n + 2
```

``` ucm :added-by-ucm
  Loading changes detected in scratch.u.

  ~ helper : Nat -> Nat

  ~ (modified)

  Run `update` to apply these changes to your codebase.
```

``` ucm
scratch/main> update

  Okay, I'm searching the branch for code that needs to be
  updated...

  That's done. Nothing that code uses changed its type, so I'm
  saving the results without typechecking it again...

  Done.

scratch/main> view direct indirect ping pong

  direct : Nat
  direct = helper 1

  indirect : Nat
  indirect =
    use Nat +
    direct + helper 2

  ping : Nat -> Nat
  ping n = if n == 0 then helper 0 else pong (Nat.drop n 1)

  pong : Nat -> Nat
  pong n = if n == 0 then 0 else ping (Nat.drop n 1)
```

``` unison
direct : Nat
direct = helper 1

indirect : Nat
indirect = direct + helper 2

ping : Nat -> Nat
ping n = if n == 0 then helper 0 else pong (Nat.drop n 1)

pong : Nat -> Nat
pong n = if n == 0 then 0 else ping (Nat.drop n 1)
```

``` ucm :added-by-ucm
  Loading changes detected in scratch.u.

  No changes found.
```

A term that nothing refers to may change its type in the same update: only the types that dependents rely on have to
stay the same.

``` unison
helper : Nat -> Nat
helper n = n + 3

indirect : Int
indirect = +1
```

``` ucm :added-by-ucm
  Loading changes detected in scratch.u.

  ~ helper   : Nat -> Nat
  ~ indirect : Int

  ~ (modified)

  Run `update` to apply these changes to your codebase.
```

``` ucm
scratch/main> update

  Okay, I'm searching the branch for code that needs to be
  updated...

  That's done. Nothing that code uses changed its type, so I'm
  saving the results without typechecking it again...

  Done.

scratch/main> view direct ping

  direct : Nat
  direct = helper 1

  ping : Nat -> Nat
  ping n = if n == 0 then helper 0 else pong (Nat.drop n 1)
```

When a type that a dependent relies on changes, the dependents are typechecked again.

``` unison
helper : a -> Nat
helper _ = 4
```

``` ucm :added-by-ucm
  Loading changes detected in scratch.u.

  ~ helper : a -> Nat

  ~ (modified)

  Run `update` to apply these changes to your codebase.
```

``` ucm
scratch/main> update

  Okay, I'm searching the branch for code that needs to be
  updated...

  That's done. Now I'm making sure everything typechecks...

  Everything typechecks, so I'm saving the results...

  Done.

scratch/main> view direct

  direct : Nat
  direct = helper 1
```
