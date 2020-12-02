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

<<<<<<< HEAD
  1. builtin/ (290 definitions)
=======
  1. builtin/ (289 definitions)
>>>>>>> trunk

```
And for a limited time, you can get even more builtin goodies:

```ucm
.foo> builtins.mergeio

  Done.

.foo> ls

<<<<<<< HEAD
  1. builtin/ (453 definitions)
=======
  1. builtin/ (452 definitions)
>>>>>>> trunk

```
More typically, you'd start out by pulling `base.
