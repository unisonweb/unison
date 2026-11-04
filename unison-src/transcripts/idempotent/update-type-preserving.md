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

  That's done. Some of that code uses a type that changed, so
  I'm typechecking those parts again...

  That's done. Nothing that code uses changed its type, so I'm
  saving the results without typechecking it again...

  Done.

scratch/main> view direct ping

  direct : Nat
  direct = helper 1

  ping : Nat -> Nat
  ping n = if n == 0 then helper 0 else pong (Nat.drop n 1)
```

When a type that a dependent relies on changes, only the dependents that use it are typechecked again (\#904). Here
`direct` and `ping` use `helper` and are typechecked; `pong` is not, and stays in a cycle with `ping`.

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

  That's done. Some of that code uses a type that changed, so
  I'm typechecking those parts again...

  Everything typechecks, so I'm saving the results...

  Done.

scratch/main> view direct indirect ping pong

  direct : Nat
  direct = helper 1

  indirect : Int
  indirect = +1

  ping : Nat -> Nat
  ping n = if n == 0 then helper 0 else pong (Nat.drop n 1)

  pong : Nat -> Nat
  pong n = if n == 0 then 0 else ping (Nat.drop n 1)
```

``` unison
direct : Nat
direct = helper 1

indirect : Int
indirect = +1

ping : Nat -> Nat
ping n = if n == 0 then helper 0 else pong (Nat.drop n 1)

pong : Nat -> Nat
pong n = if n == 0 then 0 else ping (Nat.drop n 1)
```

``` ucm :added-by-ucm
  Loading changes detected in scratch.u.

  No changes found.
```

One update can do both: `twice` keeps its type and `label` doesn't, so only `shown` is typechecked again.

``` unison
twice : Nat -> Nat
twice n = n * 2

label : Nat -> Text
label n = "n"

shown : Text
shown = label (twice 3)

doubled : Nat
doubled = twice 4 + Text.size shown
```

``` ucm :added-by-ucm
  Loading changes detected in scratch.u.

  + doubled : Nat
  + label   : Nat -> Text
  + shown   : Text
  + twice   : Nat -> Nat

  Run `update` to apply these changes to your codebase.
```

``` ucm
scratch/main> update

  Okay, I'm searching the branch for code that needs to be
  updated...

  Done.
```

``` unison
twice : Nat -> Nat
twice n = n + n

label : a -> Text
label _ = "a"
```

``` ucm :added-by-ucm
  Loading changes detected in scratch.u.

  ~ label : a -> Text
  ~ twice : Nat -> Nat

  ~ (modified)

  Run `update` to apply these changes to your codebase.
```

``` ucm
scratch/main> update

  Okay, I'm searching the branch for code that needs to be
  updated...

  That's done. Some of that code uses a type that changed, so
  I'm typechecking those parts again...

  Everything typechecks, so I'm saving the results...

  Done.

scratch/main> view shown doubled

  doubled : Nat
  doubled =
    use Nat +
    twice 4 + Text.size shown

  shown : Text
  shown = label (twice 3)
```

``` unison
shown : Text
shown = label (twice 3)

doubled : Nat
doubled = twice 4 + Text.size shown
```

``` ucm :added-by-ucm
  Loading changes detected in scratch.u.

  No changes found.
```
