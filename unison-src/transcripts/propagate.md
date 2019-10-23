# Propagating type edits

```unison
use .builtin

unique type Foo = Foo

fooToInt : Foo -> Int
fooToInt _ = +42
```

```ucm
.subpath> add
.subpath> find.verbose
.subpath> view fooToInt
```

```unison
type Foo = Foo | Bar
```

```ucm
.subpath> update
```

