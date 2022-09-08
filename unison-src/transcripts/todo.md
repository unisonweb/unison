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
.> cd .
```

Perform a type-changing update so dependents are added to our update frontier.

```unison:hide
x = -1

structural type MyType = MyType Text
```

```ucm:error
.simple> update
.simple> todo
.> cd .
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
.> cd .
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

## An update to one element of a cycle, but not the other

```ucm
.cycle> builtins.mergeio
```

```unison
even = cases
  0 -> true
  n -> odd (drop 1 n)

odd = cases
  0 -> false
  n -> even (drop 1 n)
```

```ucm
.cycle> add
```

```unison
even = 17
```

```ucm
.cycle> update
.cycle> view odd
.cycle> view.patch patch
```

```ucm:error
.cycle> todo
```
