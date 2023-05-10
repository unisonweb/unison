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

  builtin builtin.List.drop : Nat -> [a] -> [a]

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

  I found and typechecked these definitions in scratch.u. If you
  do an `add` or `update`, here's how your codebase would
  change:
  
    ⍟ These new definitions are ok to `add`:
    
      cool.abra.cadabra            : Text
      lib.distributed.abra.cadabra : Text
      lib.distributed.baz.qux      : Text
      lib.distributed.lib.baz.qux  : Text

```
```ucm
.> add

  ⍟ I've added these definitions:
  
    cool.abra.cadabra            : Text
    lib.distributed.abra.cadabra : Text
    lib.distributed.baz.qux      : Text
    lib.distributed.lib.baz.qux  : Text

```
```unison
> abra.cadabra
> baz.qux
```

```ucm

  ✅
  
  scratch.u changed.
  
  Now evaluating any watch expressions (lines starting with
  `>`)... Ctrl+C cancels.

    1 | > abra.cadabra
          ⧩
          "my project"
  
    2 | > baz.qux
          ⧩
          "direct dependency 2"

```
```ucm
.> view abra.cadabra

  cool.abra.cadabra : Text
  cool.abra.cadabra = "my project"

.> view baz.qux

  lib.distributed.baz.qux : Text
  lib.distributed.baz.qux = "direct dependency 2"

```
Note that we can always still view indirect dependencies by using more name segments:

```ucm
.> view distributed.abra.cadabra

  lib.distributed.abra.cadabra : Text
  lib.distributed.abra.cadabra = "direct dependency 1"

.> names distributed.lib.baz.qux

  Term
  Hash:   #nhup096n2s
  Names:  lib.distributed.lib.baz.qux
  
  Tip: Use `names.global` to see more results.

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
