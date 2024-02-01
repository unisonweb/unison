# Suffix-based resolution of names

```ucm:hide
.> builtins.merge
```

Any unique name suffix can be used to refer to a definition. For instance:

```unison:hide
-- No imports needed even though FQN is `builtin.{Int,Nat}`
foo.bar.a : Int
foo.bar.a = +99

-- No imports needed even though FQN is `builtin.Optional.{None,Some}`
optional.isNone = cases
  None -> true
  Some _ -> false
```

This also affects commands like find. Notice lack of qualified names in output:

```ucm
.> add
.> find take
```

The `view` and `display` commands also benefit from this:

```ucm
.> view List.drop
.> display bar.a
```

In the signature, we don't see `base.Nat`, just `Nat`. The full declaration name is still shown for each search result though.

Type-based search also benefits from this, we can just say `Nat` rather than `.base.Nat`:

```ucm
.> find : Nat -> [a] -> [a]
```

## Preferring names not in `lib`

Suffix-based resolution prefers names with fewer name segments that are equal to "lib". This
has the effect of preferring names defined in your project to names from dependencies of your project, and names from indirect dependencies have even lower weight.

```unison
cool.abra.cadabra = "my project"
lib.distributed.abra.cadabra = "direct dependency 1"
lib.distributed.baz.qux = "direct dependency 2"
lib.distributed.lib.baz.qux = "indirect dependency"
```

```ucm
.> add
```

```unison
> abra.cadabra
> baz.qux
```

```ucm
.> view abra.cadabra
.> view baz.qux
```

Note that we can always still view indirect dependencies by using more name segments:

```ucm
.> view distributed.abra.cadabra
.> names distributed.lib.baz.qux
```

## Corner cases

If a definition is given in a scratch file, its suffixes shadow existing definitions that exist in the codebase with the same suffixes. For example:

```unison:hide
unique type A = Thing1 Nat | thing2 Nat

foo.a = 23
bar = 100
```

```ucm
.> add
```

```unison
unique type B = Thing1 Text | thing2 Text | Thing3 Text

zoink.a = "hi"

-- verifying that the `a` here references `zoink.a`
foo.baz.qux.bar : Text
foo.baz.qux.bar = a

-- verifying that the `bar` is resolving to `foo.baz.qux.bar`
-- and that `Thing1` references `B.Thing1` from the current file
fn = cases
  Thing1 msg -> msg Text.++ bar
  thing2 msg -> msg Text.++ bar
  _ -> todo "hmm"
```
