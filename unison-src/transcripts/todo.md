# Test the `todo` command

## Simple type-changing update.

```ucm:hide
.> builtins.mergeio
```

```unison:hide
x = 1
useX = x + 10

structural type MyType = MyType Nat
useMyType = match MyType 1 with
  MyType a -> a + 10
```

```ucm:hide
.simple> add
```

Perform a type-changing update so dependents are added to our update frontier.

```unison:hide
x = -1

structural type MyType = MyType Text
```

```ucm:error
.simple> update
.simple> todo
```

## A merge with conflicting updates.

```unison:hide
x = 1
structural type MyType = MyType
```

Set up two branches with the same starting point.

```ucm:hide
.mergeA> add
.> fork .mergeA .mergeB
```

Update `x` to a different term in each branch.

```unison:hide
x = 2
structural type MyType = MyType Nat
```

```ucm:hide
.mergeA> update
```

```unison:hide
x = 3
structural type MyType = MyType Int
```

```ucm:hide
.mergeB> update
```

```ucm:error
.mergeA> merge .mergeB
.mergeA> todo
```
