# Suffix-based resolution of names

Any unique name suffix can be used to refer to a definition. For instance:

```unison
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

  ⍟ I've added these definitions:
  
    foo.bar.a       : Int
    optional.isNone : Optional a -> Boolean

.> find take

  1. builtin.Bytes.take : Nat -> Bytes -> Bytes
  2. builtin.List.take : Nat -> [a] -> [a]
  3. builtin.Text.take : Nat -> Text -> Text
  

```
The `view` and `display` commands also benefit from this:

```ucm
.> view List.drop

  -- builtin.List.drop is built-in.

.> display bar.a

  +99

```
In the signature, we don't see `base.Nat`, just `Nat`. The full declaration name is still shown for each search result though.

Type-based search also benefits from this, we can just say `Nat` rather than `.base.Nat`:

```ucm
.> find : Nat -> [a] -> [a]

  1. builtin.List.drop : Nat -> [a] -> [a]
  2. builtin.List.take : Nat -> [a] -> [a]
  

```
