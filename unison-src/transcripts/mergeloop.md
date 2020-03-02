# Merge loop test

This tests for regressions of https://github.com/unisonweb/unison/issues/1276 where trivial merges cause loops in the history.

Let's make three identical namespaces with different histories:

```unison
a = 1
```

```ucm
.x> add
```

```unison
b = 2
```

```ucm
.x> add
```

```unison
b = 2
```

```ucm
.y> add
```

```unison
a = 1
```

```ucm
.y> add
```

```unison
a = 1
b = 2
```

```ucm
.z> add
.> merge x y
.> merge y z
.> history z
```


