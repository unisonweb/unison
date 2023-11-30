This transcript tests that UCM can always access the definition of
`IsPropagated`/`isPropagated`, which is used internally.

`y` depends on `x`,
```unison
x = 3
y = x + 1
```

```ucm
.> add

  ⍟ I've added these definitions:
  
    x : Nat
    y : Nat

```
so the `update` of `x` causes a propagated update of `y`, and UCM links the
`isPropagated` metadata to such resulting terms:

```unison
x = 4
```

```ucm
.> update.old

  ⍟ I've updated these names to your new definition:
  
    x : Nat

.> links y

```

```ucm
.> update.old.> links y.> view 1
```


🛑

The transcript failed due to an error in the stanza above. The error is:

⚠️
I don't know how to links. Type `help` or `?` to get help.
