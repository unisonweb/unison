# Conflicted names

The todo command shows conflicted names (not demonstrated here yet because it is not easy to create them for tests, yet).

# Direct dependencies without names

The `todo` command shows hashes of direct dependencies of local (outside `lib`) definitions that don't have names in
the current namespace.

```unison
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
