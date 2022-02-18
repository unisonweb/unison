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
  4. builtin.io2.MVar.take.impl : MVar a ->{IO} Either Failure a
  5. builtin.io2.MVar.tryTake : MVar a ->{IO} Optional a
  

```
The `view` and `display` commands also benefit from this:

```ucm
.> view List.drop

  builtin builtin.List.drop : builtin.Nat -> [a] -> [a]

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
## Corner cases

If a definition is given in a scratch file, its suffixes shadow existing definitions that exist in the codebase with the same suffixes. For example:

```unison
unique type A = Thing1 Nat | thing2 Nat

foo.a = 23
bar = 100
```

```ucm
.> add

  ⍟ I've added these definitions:
  
    unique type A
    bar   : Nat
    foo.a : Nat

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

```ucm

  I found and typechecked these definitions in scratch.u. If you
  do an `add` or `update`, here's how your codebase would
  change:
  
    ⍟ These new definitions are ok to `add`:
    
      unique type B
      fn              : B -> Text
      foo.baz.qux.bar : Text
      zoink.a         : Text

```
