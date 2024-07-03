# Nothing to do

When there's nothing to do, `todo` says this:

```ucm
scratch/main> todo

  You have no pending todo items. Good work! ✅

```
# Dependents of `todo`

The `todo` command shows local (outside `lib`) terms that directly call `todo`.

```unison
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
scratch/main> add

  ⍟ I've added these definitions:
  
    bar : Nat
    foo : Nat

scratch/main> todo

  These terms call `todo`:
  
    1. foo

```
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
scratch/main> add

  ⍟ I've added these definitions:
  
    baz     : Nat
    foo.bar : Nat

scratch/main> delete.namespace.force foo

  Done.

  ⚠️
  
  Of the things I deleted, the following are still used in the
  following definitions. They now contain un-named references.
  
  Dependency   Referenced In
  bar          1. baz

scratch/main> todo

  These terms do not have any names in the current namespace:
  
    1. #1jujb8oelv

```
# Conflicted names

The `todo` command shows conflicted names.

```unison
foo = 16
bar = 17
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
scratch/main> add

  ⍟ I've added these definitions:
  
    bar : Nat
    foo : Nat

scratch/main> debug.alias.term.force foo bar

  Done.

scratch/main> todo

  ❓
  
  The term bar has conflicting definitions: 1. foo 2.
  bar#cq22mm4sca
  
  Tip: Use `move.term` or `delete.term` to resolve the
       conflicts.

```
# Definitions in lib

The `todo` command complains about terms and types directly in `lib`.

```unison
lib.foo = 16
```

```ucm

  Loading changes detected in scratch.u.

  I found and typechecked these definitions in scratch.u. If you
  do an `add` or `update`, here's how your codebase would
  change:
  
    ⍟ These new definitions are ok to `add`:
    
      lib.foo : Nat

```
```ucm
scratch/main> add

  ⍟ I've added these definitions:
  
    lib.foo : Nat

scratch/main> todo

  There's a type or term at the top level of the `lib`
  namespace, where I only expect to find subnamespaces
  representing library dependencies. Please move or remove it.

```
# Constructor aliases

The `todo` command complains about constructor aliases.

```unison
type Foo = One
```

```ucm

  Loading changes detected in scratch.u.

  I found and typechecked these definitions in scratch.u. If you
  do an `add` or `update`, here's how your codebase would
  change:
  
    ⍟ These new definitions are ok to `add`:
    
      type Foo

```
```ucm
scratch/main> add

  ⍟ I've added these definitions:
  
    type Foo

scratch/main> alias.term Foo.One Foo.Two

  Done.

scratch/main> todo

  The type Foo has a constructor with multiple names. Please
  delete all but one name for each constructor.
  
    1. Foo.One
    2. Foo.Two

```
# Missing constructor names

The `todo` command complains about missing constructor names.

```unison
type Foo = Bar
```

```ucm

  Loading changes detected in scratch.u.

  I found and typechecked these definitions in scratch.u. If you
  do an `add` or `update`, here's how your codebase would
  change:
  
    ⍟ These new definitions are ok to `add`:
    
      type Foo

```
```ucm
scratch/main> add

  ⍟ I've added these definitions:
  
    type Foo

scratch/main> delete.term Foo.Bar

  Done.

scratch/main> todo

  These types have some constructors with missing names:
  
    1. Foo

```
