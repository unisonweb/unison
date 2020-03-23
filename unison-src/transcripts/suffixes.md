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
