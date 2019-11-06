Check that doc literals are pretty-printed OK.

```unison
List.take.ex1 = take 0 [1,2,3,4,5]
List.take.ex2 n = take n [1,2,3,4,5]

docs.ex3 = [: foo :]
```

```ucm
.> add
```

```unison
use .builtin

docs.List.take = [:

`@List.take n xs` returns the first `n` elements of `xs`.

@[source] List.take.ex1

@[evaluate] List.take.ex1

@[signature] List.take.ex2

@[include] docs.ex3

:]
```

```ucm
.> add
```

```ucm
.> view docs.List.take
```