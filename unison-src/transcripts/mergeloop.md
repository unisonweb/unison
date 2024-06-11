# Merge loop test

This tests for regressions of https://github.com/unisonweb/unison/issues/1276 where trivial merges cause loops in the history.

Let's make three identical namespaces with different histories:

```unison
a = 1
```

```ucm
scratch/main x> add
```

```unison
b = 2
```

```ucm
scratch/main x> add
```

```unison
b = 2
```

```ucm
scratch/main y> add
```

```unison
a = 1
```

```ucm
scratch/main y> add
```

```unison
a = 1
b = 2
```

```ucm
scratch/main z> add
scratch/main> merge.old x y
scratch/main> merge.old y z
scratch/main> history z
```


