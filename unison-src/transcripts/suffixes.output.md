# Suffix-based resolution of names

Any unique name suffix can be used to refer to a definition. For instance:

``` unison
-- No imports needed even though FQN is `builtin.{Int,Nat}`
foo.bar.a : Int
foo.bar.a = +99

-- No imports needed even though FQN is `builtin.Optional.{None,Some}`
optional.isNone = cases
  None -> true
  Some _ -> false
```

This also affects commands like find. Notice lack of qualified names in output:

``` ucm
scratch/main> add

  ⍟ I've added these definitions:
  
    foo.bar.a       : Int
    optional.isNone : Optional a -> Boolean

scratch/main> find take

  1. builtin.Bytes.take : Nat -> Bytes -> Bytes
  2. builtin.List.take : Nat -> [a] -> [a]
  3. builtin.Text.take : Nat -> Text -> Text
  4. builtin.io2.MVar.take.impl : MVar a ->{IO} Either Failure a
  5. builtin.io2.MVar.tryTake : MVar a ->{IO} Optional a
  

```
The `view` and `display` commands also benefit from this:

``` ucm
scratch/main> view List.drop

  builtin builtin.List.drop : builtin.Nat -> [a] -> [a]

scratch/main> display bar.a

  +99

```
In the signature, we don't see `base.Nat`, just `Nat`. The full declaration name is still shown for each search result though.

Type-based search also benefits from this, we can just say `Nat` rather than `.base.Nat`:

``` ucm
scratch/main> find : Nat -> [a] -> [a]

  1. builtin.List.drop : Nat -> [a] -> [a]
  2. builtin.List.take : Nat -> [a] -> [a]
  

```
## Preferring names not in `lib.*.lib.*`

Suffix-based resolution prefers names that are not in an indirect dependency.

``` unison
cool.abra.cadabra = "my project"
lib.distributed.abra.cadabra = "direct dependency 1"
lib.distributed.baz.qux = "direct dependency 2"
lib.distributed.lib.baz.qux = "indirect dependency"
```

``` ucm

  Loading changes detected in scratch.u.

  I found and typechecked these definitions in scratch.u. If you
  do an `add` or `update`, here's how your codebase would
  change:
  
    ⍟ These new definitions are ok to `add`:
    
      cool.abra.cadabra            : Text
      lib.distributed.abra.cadabra : Text
      lib.distributed.baz.qux      : Text
      lib.distributed.lib.baz.qux  : Text

```
``` ucm
scratch/main> add

  ⍟ I've added these definitions:
  
    cool.abra.cadabra            : Text
    lib.distributed.abra.cadabra : Text
    lib.distributed.baz.qux      : Text
    lib.distributed.lib.baz.qux  : Text

```
``` unison
> abra.cadabra
```

``` ucm

  Loading changes detected in scratch.u.

  I couldn't figure out what abra.cadabra refers to here:
  
      1 | > abra.cadabra
  
  The name abra.cadabra is ambiguous. I couldn't narrow it down
  by type, as any type would work here.
  
  I found some terms in scope that have matching names and
  types. Maybe you meant one of these:
  
  cool.abra.cadabra : Text
  distributed.abra.cadabra : Text

```
``` unison
> baz.qux
```

``` ucm

  Loading changes detected in scratch.u.

  ✅
  
  scratch.u changed.
  
  Now evaluating any watch expressions (lines starting with
  `>`)... Ctrl+C cancels.

    1 | > baz.qux
          ⧩
          "direct dependency 2"

```
``` ucm
scratch/main> view abra.cadabra

  cool.abra.cadabra : Text
  cool.abra.cadabra = "my project"
  
  lib.distributed.abra.cadabra : Text
  lib.distributed.abra.cadabra = "direct dependency 1"

scratch/main> view baz.qux

  lib.distributed.baz.qux : Text
  lib.distributed.baz.qux = "direct dependency 2"

```
Note that we can always still view indirect dependencies by using more name segments:

``` ucm
scratch/main> view distributed.abra.cadabra

  lib.distributed.abra.cadabra : Text
  lib.distributed.abra.cadabra = "direct dependency 1"

scratch/main> names distributed.lib.baz.qux

  Term
  Hash:   #nhup096n2s
  Names:  lib.distributed.lib.baz.qux
  
  Tip: Use `names.global` to see more results.

```
## Corner cases

If a definition is given in a scratch file, its suffixes shadow existing definitions that exist in the codebase with the same suffixes. For example:

``` unison
unique type A = Thing1 Nat | thing2 Nat

foo.a = 23
bar = 100
```

``` ucm
scratch/main> add

  ⍟ I've added these definitions:
  
    type A
    bar   : Nat
    foo.a : Nat

```
``` unison
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

``` ucm

  Loading changes detected in scratch.u.

  I found and typechecked these definitions in scratch.u. If you
  do an `add` or `update`, here's how your codebase would
  change:
  
    ⍟ These new definitions are ok to `add`:
    
      type B
      fn              : B -> Text
      foo.baz.qux.bar : Text
      zoink.a         : Text

```
