`alias.type` makes a new name for a type.

``` ucm :hide
scratch/main> builtins.mergeio lib.builtins
```

``` ucm
scratch/main> alias.type lib.builtins.Nat Foo

  Done.

scratch/main> ls .

  1. Foo  (builtin type)
  2. lib. (857 terms, 126 types)
```

It won't create a conflicted name, though.

``` ucm :error
scratch/main> alias.type lib.builtins.Int Foo

  ⚠️

  A type by that name already exists.
```

``` ucm
scratch/main> ls .

  1. Foo  (builtin type)
  2. lib. (857 terms, 126 types)
```

You can use `debug.alias.type.force` for that.

``` ucm
scratch/main> debug.alias.type.force lib.builtins.Int Foo

  Done.

scratch/main> ls .

  1. Foo  (builtin type)
  2. Foo  (builtin type)
  3. lib. (857 terms, 126 types)
```

``` ucm :hide
scratch/main> project.delete scratch

scratch/main> builtins.mergeio
```

`alias.type` moves over constructors, too. Here, we demonstrate `view <type>` shows the constructors have names, for
types created with `alias.type`.

``` unison :hide
type Foo = Bar
type Baz = Qux | Honk
```

``` ucm :hide
scratch/main> update

scratch/main> move Baz lib.dep.Baz

scratch/main> alias.type Foo Foo2

scratch/main> alias.type lib.dep.Baz Baz2
```

``` ucm
scratch/main> view Foo2 Baz2

  type Baz2 = Qux | Honk

  type Foo2 = Bar

scratch/main> ls Foo2

  1. Bar (Foo)

scratch/main> ls Baz2

  1. Honk (Baz2)
  2. Qux  (Baz2)
```

If there's already some term in the way of a constructor, though, `alias.type` will fail.

``` unison :hide
Foo3.Bar = 17
```

``` ucm :hide
scratch/main> update
```

``` ucm :error
scratch/main> alias.type Foo Foo3

  ⚠️

  A term named Foo3.Bar already exists.
```

``` ucm :hide
scratch/main> project.delete scratch
```
