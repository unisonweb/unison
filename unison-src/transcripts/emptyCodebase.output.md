# The empty codebase

The Unison codebase, when first initialized, contains no definitions in its namespace.

Not even `Nat` or `+`!

BEHOLD!!!

```ucm
.> ls

  nothing to show

```
Technically, the definitions all exist, but they have no names. `builtins.merge` brings them into existence, under the current namespace:

```ucm
  ☝️  The namespace .foo is empty.

.foo> builtins.merge

  Done.

.foo> ls

  1. builtin/ (415 terms, 63 types)

```
And for a limited time, you can get even more builtin goodies:

```ucm
.foo> builtins.mergeio

  Done.

.foo> ls

  1. builtin/ (587 terms, 81 types)

```
More typically, you'd start out by pulling `base.
