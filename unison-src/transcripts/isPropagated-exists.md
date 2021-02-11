This transcript tests that UCM can always access the definition of 
`IsPropagated`, which is used internally.

```ucm:hide
.> alias.term ##Nat.+ +
.> alias.type ##Nat Nat
```

y depends on x
```unison
x = 3
y = x + 1
```

```ucm
.> add
```

```unison
x = 4
```

The `update` of `x` causes a propagated update of `y`, and UCM links the 
`isPropagated` metadata to such resulting terms:

```ucm
.> update
.> links y
.> view 1
```

Well, it's hard to tell from those hashes, but those are right.  We can confirm
by running `builtins.merge` to have UCM add names for them.

```ucm
.> builtins.merge
.> links y
.> view 1
```

