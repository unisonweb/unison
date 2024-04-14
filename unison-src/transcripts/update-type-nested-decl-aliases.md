```ucm:hide
.> builtins.merge lib.builtin
```

```unison
unique type Foo = Bar Nat

structural type A.B = OneAlias Foo
structural type A = B.TheOtherAlias Foo
```

```ucm
.> add
```

```unison
unique type Foo = Bar Nat Nat
```

Bug: we want this update to be rejected earlier, because it violates the "decl coherency" precondition that there's
only one name for each constructor. We instead get too far in the update process, and are delivered a bogus scratch.u
file to stare at.

```ucm:error
.> update
```
