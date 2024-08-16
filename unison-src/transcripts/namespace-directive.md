A `namespace foo` directive is optional, and may only appear at the top of a file.

It affects the contents of the file as follows:

1. All bindings like `x.y.z` are prefixed with the namespace; note that when this file is saved, the feedback mentions
the full bindings' names.

```ucm
scratch/main> builtins.mergeio lib.builtins
```

```unison
namespace foo

baz : Nat
baz = 17
```

2. Free variables whose names exactly match bindings in the file are rewritten to refer to the prefixed binder instead.
That is, a term like `factorial = ... factorial ...` is rewritten to `foo.factorial = ... foo.factorial ...`.

```unison
namespace foo

factorial : Int -> Int
factorial = cases
    +0 -> +1
    n -> n * factorial (n - +1)

longer.evil.factorial : Int -> Int
longer.evil.factorial n = n
```

```ucm
scratch/main> add
scratch/main> view factorial
```

Note that in the above example, we do not want the existence of a `namespace foo` directive to determine whether the
reference to the name `factorial` within the body of `factorial` is a recursive reference (good, behavior without
namespace directive, exact-name-match-wins semantics) or an ambiguous reference (bad, as would be the case if the
bindings were expanded to `foo.factorial` and `foo.longer.evil.factorial`, but the variables left alone).

Here are a few more examples demonstrating that type names, constructor names, and generated record accessor names are
all properly handled.

```unison
type longer.foo.Foo = Bar
type longer.foo.Baz = { qux : Nat }
```

```ucm
scratch/main> add
```

```unison
namespace foo

type Foo = Bar
type Baz = { qux : Nat }

type RefersToFoo = RefersToFoo Foo

refersToBar = cases
  Bar -> 17

refersToQux baz =
  Baz.qux baz + Baz.qux baz
```

```ucm
scratch/main> add
scratch/main> view RefersToFoo refersToBar refersToQux
scratch/main> todo
```
