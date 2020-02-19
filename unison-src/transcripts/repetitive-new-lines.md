##### Steps to reproduce the "new line on update" bug

```ucm:hide
.> builtins.merge
.> pull https://github.com/unisonweb/base .base
```

```unison
f: Nat -> Nat
f x = 0

test> path.to.one = check (f 1 == 2)
```

```ucm:error
.> add
.> test
```

```unison
f x = x + 1
```

```ucm
.> update
.> test
```
