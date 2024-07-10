# Nothing to do

When there's nothing to do, `todo` says this:

```ucm
project/main> todo

  You have no pending todo items. Good work! ✅

```
# Conflicted names

The todo command shows conflicted names (not demonstrated here yet because it is not easy to create them for tests, yet).

# Dependents of `todo`

The `todo` command shows local (outside `lib`) terms that directly call `todo`.

``` unison
foo : Nat
foo = todo "implement foo"

bar : Nat
bar = foo + foo
```

```ucm

  Loading changes detected in scratch.u.

  I found and typechecked these definitions in scratch.u. If you
  do an `add` or `update`, here's how your codebase would
  change:
  
    ⍟ These new definitions are ok to `add`:
    
      bar : Nat
      foo : Nat

```
```ucm
project/main> add

  ⍟ I've added these definitions:
  
    bar : Nat
    foo : Nat

project/main> todo

  These terms call `todo`:
  
    1. foo

```
# Direct dependencies without names

The `todo` command shows hashes of direct dependencies of local (outside `lib`) definitions that don't have names in
the current namespace.

``` unison
foo.bar = 15
baz = foo.bar + foo.bar
```

```ucm

  Loading changes detected in scratch.u.

  I found and typechecked these definitions in scratch.u. If you
  do an `add` or `update`, here's how your codebase would
  change:
  
    ⍟ These new definitions are ok to `add`:
    
      baz     : Nat
      foo.bar : Nat

```
```ucm
project/main> add

  ⍟ I've added these definitions:
  
    baz     : Nat
    foo.bar : Nat

project/main> delete.namespace.force foo

  Done.

  ⚠️
  
  Of the things I deleted, the following are still used in the
  following definitions. They now contain un-named references.
  
  Dependency   Referenced In
  bar          1. baz

project/main> todo

  These terms do not have any names in the current namespace:
  
    1. #1jujb8oelv

```
