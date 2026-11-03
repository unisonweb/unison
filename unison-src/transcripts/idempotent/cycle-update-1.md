Update a member of a cycle, but retain the cycle.

``` ucm :hide
> builtins.merge
```

``` unison
ping : 'Nat
ping _ = !pong + 1

pong : 'Nat
pong _ = !ping + 2
```

``` ucm :added-by-ucm
  Loading changes detected in scratch.u.

  + ping : 'Nat
  + pong : 'Nat

  Run `update` to apply these changes to your codebase.
```

``` ucm
> add

  Okay, I'm searching the branch for code that needs to be
  updated...

  Done.
```

``` unison
ping : 'Nat
ping _ = !pong + 3
```

``` ucm :added-by-ucm
  Loading changes detected in scratch.u.

  Added to the file, from the codebase: pong. They use what this
  file redefines, so they are part of the file now, and `update`
  saves them rebuilt on the new definition. A comment above them
  in the file says so; change them if the new definition doesn't
  fit the way they use it.

  ~ ping : 'Nat
  ~ pong : 'Nat

  ~ (modified)

  Run `update` to apply these changes to your codebase.
```

``` unison :added-by-ucm scratch.u
ping : 'Nat
ping _ = !pong + 3

-- Added these definitions from the branch, since they depend on
-- `ping`, which this file changes. Edit them here if they no longer
-- typecheck, and `update` will save them with the rest.

pong : 'Nat
pong _ =
  use Nat +
  ping() + 2

```

``` ucm
> update

  Okay, I'm searching the branch for code that needs to be
  updated...

  Done.

> view ping pong

  ping : 'Nat
  ping _ =
    use Nat +
    pong() + 3

  pong : 'Nat
  pong _ =
    use Nat +
    ping() + 2
```
