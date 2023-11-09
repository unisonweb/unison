```ucm:hide
.> builtins.merge
.> move.namespace builtin lib.builtin
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

Bug: this update doesn't do the right thing; we simply don't properly update all of the names because
each decl, in isolation, has two equally good names to pick for its one constructor:

    -- These are the same thing, but which do we render?
    type A = B.OneAlias Foo
    type A = B.TheOtherAlias Foo

    -- Whichever one we picked, we want to pick the other one here
    type A.B = OneAlias Foo
    type A.B = TheOtherAlias Foo

Long story short, we should reject this update as it violates the "decl coherency" precondition.

```ucm
.> update
.> find.verbose
```

