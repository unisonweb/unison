# Delete

The delete command can delete both terms and types.

First, let's make sure it complains when we try to delete a name that doesn't
exist.

``` ucm :hide
scratch/main> builtins.mergeio lib.builtins
```

``` ucm :error
scratch/main> delete.verbose foo

  ⚠️
  I don't know how to delete.verbose. Type `help` or `?` to get
  help.
```

Now for some easy cases. Deleting an unambiguous term, then deleting an unambiguous type.

``` unison
foo = 1
structural type Foo = Foo ()
```

``` ucm :added-by-ucm
  Loading changes detected in scratch.u.

  + structural type Foo

  + foo : Nat

  Run `update` to apply these changes to your codebase.
```

``` ucm
scratch/main> update

  Okay, I'm searching the branch for code that needs to be
  updated...

  Done.

scratch/main> delete foo

  I deleted these definitions:

    term foo

  Tip: You can use `undo` or use a hash from `reflog` to undo
       this change.

scratch/main> delete Foo

  I deleted these definitions:

    type Foo

  Tip: You can use `undo` or use a hash from `reflog` to undo
       this change.

scratch/main> ls

  1. lib. (838 terms, 121 types)
```

``` ucm :hide
scratch/main> project.delete scratch
```

You can delete by suffix, which matches everything, ignoring constructors. (This behavior might soon change to simply
give an ambiguous-error instead).

``` ucm :hide
scratch/main> builtins.mergeio lib.builtins
```

``` unison
foo.x = 17
bar.x = 18
type Baz = x
type foo.x = y
```

``` ucm :added-by-ucm
  Loading changes detected in scratch.u.

  + type Baz
  + type foo.x

  + bar.x : Nat
  + foo.x : Nat

  Run `update` to apply these changes to your codebase.
```

``` ucm
scratch/main> update

  Okay, I'm searching the branch for code that needs to be
  updated...

  Done.

scratch/main> delete x

  I deleted these definitions:

    type foo.x
    term bar.x
    term foo.x

  Tip: You can use `undo` or use a hash from `reflog` to undo
       this change.
```

``` ucm :hide
scratch/main> project.delete scratch
```

You can't delete a constructor with `delete`, but you can with `delete.force`.

``` ucm :hide
scratch/main> builtins.mergeio lib.builtins
```

``` unison
type Foo = Foo
```

``` ucm :added-by-ucm
  Loading changes detected in scratch.u.

  + type Foo

  Run `update` to apply these changes to your codebase.
```

``` ucm
scratch/main> update

  Okay, I'm searching the branch for code that needs to be
  updated...

  Done.
```

``` ucm :error
scratch/main> delete Foo.Foo

  ⚠️

  I can't delete the constructor Foo.Foo.

  You may only delete terms and types with `delete`. Use
  `delete.force` instead.
```

``` ucm
scratch/main> delete.force Foo.Foo

  I deleted these definitions:

    term Foo.Foo

  Tip: You can use `undo` or use a hash from `reflog` to undo
       this change.

scratch/main> ls

  1. Foo  (type)
  2. lib. (838 terms, 121 types)
```

``` ucm :hide
scratch/main> project.delete scratch
```

Deleting an ambiguous term is possible with `delete.force`, not `delete`.

``` ucm :hide
scratch/main> builtins.mergeio lib.builtins
```

``` unison
a.foo = 1
a.bar = 2
```

``` ucm :added-by-ucm
  Loading changes detected in scratch.u.

  + a.bar : Nat
  + a.foo : Nat

  Run `update` to apply these changes to your codebase.
```

``` ucm
scratch/main> update

  Okay, I'm searching the branch for code that needs to be
  updated...

  Done.

scratch/main> debug.alias.term.force a.bar a.foo

  Done.
```

A delete should remove both versions of the term.

``` ucm :error
scratch/main> delete a.foo

  Sorry, I can't do that right now, because there's more than
  one term with the name `a.foo`. Please rename all but one of
  them, then try again.
```

``` ucm
scratch/main> delete.force a.foo

  I deleted these definitions:

    term a.foo

  Tip: You can use `undo` or use a hash from `reflog` to undo
       this change.

scratch/main> ls a

  1. bar (Nat)
```

``` ucm :hide
scratch/main> project.delete scratch
```

Let's repeat all that on a type, for completeness.

``` ucm :hide
scratch/main> builtins.mergeio lib.builtins
```

``` unison
structural type a.Foo = Foo ()
structural type a.Bar = Bar
```

``` ucm :added-by-ucm
  Loading changes detected in scratch.u.

  + structural type a.Bar
  + structural type a.Foo

  Run `update` to apply these changes to your codebase.
```

``` ucm
scratch/main> update

  Okay, I'm searching the branch for code that needs to be
  updated...

  Done.

scratch/main> debug.alias.type.force a.Bar a.Foo

  Done.
```

``` ucm :error
scratch/main> delete a.Foo

  Sorry, I can't do that right now, because there's more than
  one type with the name `a.Foo`. Please rename all but one of
  them, then try again.
```

``` ucm
scratch/main> delete.force a.Foo

  I deleted these definitions:

    type a.Foo

  Tip: You can use `undo` or use a hash from `reflog` to undo
       this change.

scratch/main> delete.force a.Foo.Foo

  I deleted these definitions:

    term a.Foo.Foo

  Tip: You can use `undo` or use a hash from `reflog` to undo
       this change.
```

``` ucm :hide
scratch/main> project.delete scratch
```

Finally, let's try to delete a term and a type with the same name.

``` ucm :hide
scratch/main> builtins.mergeio lib.builtins
```

``` unison
foo = 1
structural type foo = Foo ()
```

``` ucm :added-by-ucm
  Loading changes detected in scratch.u.

  + structural type foo

  + foo : Nat

  Run `update` to apply these changes to your codebase.
```

``` ucm
scratch/main> update

  Okay, I'm searching the branch for code that needs to be
  updated...

  Done.

scratch/main> delete foo

  I deleted these definitions:

    type foo
    term foo

  Tip: You can use `undo` or use a hash from `reflog` to undo
       this change.

scratch/main> ls

  1. lib. (838 terms, 121 types)
```

``` ucm :hide
scratch/main> project.delete scratch
```

We want to be able to delete multiple terms at once

``` ucm :hide
scratch/main> builtins.mergeio lib.builtins
```

``` unison
a = "a"
b = "b"
c = "c"
```

``` ucm :added-by-ucm
  Loading changes detected in scratch.u.

  + a : Text
  + b : Text
  + c : Text

  Run `update` to apply these changes to your codebase.
```

``` ucm
scratch/main> update

  Okay, I'm searching the branch for code that needs to be
  updated...

  Done.

scratch/main> delete a b c

  I deleted these definitions:

    term a
    term b
    term c

  Tip: You can use `undo` or use a hash from `reflog` to undo
       this change.
```

``` ucm :hide
scratch/main> project.delete scratch
```

We can delete terms and types in the same invocation of delete

``` ucm :hide
scratch/main> builtins.mergeio lib.builtins
```

``` unison
structural type Foo = Foo ()
a = "a"
b = "b"
c = "c"
```

``` ucm :added-by-ucm
  Loading changes detected in scratch.u.

  + structural type Foo

  + a : Text
  + b : Text
  + c : Text

  Run `update` to apply these changes to your codebase.
```

``` ucm
scratch/main> update

  Okay, I'm searching the branch for code that needs to be
  updated...

  Done.

scratch/main> delete a b c Foo

  I deleted these definitions:

    type Foo
    term a
    term b
    term c

  Tip: You can use `undo` or use a hash from `reflog` to undo
       this change.
```

``` ucm :hide
scratch/main> project.delete scratch
```

If your `delete` would create unnamed dependencies, you'll be put on an update branch to resolve the delete.

``` ucm :hide
scratch/main> builtins.mergeio lib.builtins
```

``` unison
a = 1
b = 2
c = 3
d = a + b + c
```

``` ucm :added-by-ucm
  Loading changes detected in scratch.u.

  + a : Nat
  + b : Nat
  + c : Nat
  + d : Nat

  Run `update` to apply these changes to your codebase.
```

``` ucm
scratch/main> add

  Okay, I'm searching the branch for code that needs to be
  updated...

  Done.
```

``` ucm :error
scratch/main> delete a b c

  Some definitions depend on the ones you're trying to delete.
  I've added them to scratch.u, where you can fix them or
  comment them out. Once the file is compiling, run `update`.

  I've also switched you to a new branch update-main for this
  work. On `update`, it will be merged back into main.
```

``` unison :added-by-ucm scratch.u
-- The definitions below depend on the deleted definitions.
-- Please fix the errors and run `update`.

d : Nat
d =
  use Nat +
  a + b + c

```

``` ucm :hide
scratch/main> project.delete scratch
```

But you should be able to delete all terms which reference each other in a single command

``` ucm :hide
scratch/main> builtins.mergeio lib.builtins
```

``` unison
e = 11
f = 12 + e
g = 13 + f
h = e + f + g
```

``` ucm :added-by-ucm
  Loading changes detected in scratch.u.

  + e : Nat
  + f : Nat
  + g : Nat
  + h : Nat

  Run `update` to apply these changes to your codebase.
```

``` ucm
scratch/main> add

  Okay, I'm searching the branch for code that needs to be
  updated...

  Done.

scratch/main> delete e f g h

  I deleted these definitions:

    term e
    term f
    term g
    term h

  Tip: You can use `undo` or use a hash from `reflog` to undo
       this change.
```

``` ucm :hide
scratch/main> project.delete scratch
```

You should be able to delete a type and all the functions that reference it in a single command

``` ucm :hide
scratch/main> builtins.mergeio lib.builtins
```

``` unison
structural type Foo = Foo Nat

incrementFoo : Foo -> Nat
incrementFoo = cases
  (Foo.Foo n) -> n + 1
```

``` ucm :added-by-ucm
  Loading changes detected in scratch.u.

  + structural type Foo

  + incrementFoo : Foo -> Nat

  Run `update` to apply these changes to your codebase.
```

``` ucm
scratch/main> update

  Okay, I'm searching the branch for code that needs to be
  updated...

  Done.

scratch/main> delete Foo incrementFoo

  I deleted these definitions:

    type Foo
    term incrementFoo

  Tip: You can use `undo` or use a hash from `reflog` to undo
       this change.
```

``` ucm :hide
scratch/main> project.delete scratch
```

If you mess up on one of the names of your command, delete short circuits

``` ucm :hide
scratch/main> builtins.mergeio lib.builtins
```

``` unison
e = 11
f = 12 + e
g = 13 + f
h = e + f + g
```

``` ucm :added-by-ucm
  Loading changes detected in scratch.u.

  + e : Nat
  + f : Nat
  + g : Nat
  + h : Nat

  Run `update` to apply these changes to your codebase.
```

``` ucm
scratch/main> add

  Okay, I'm searching the branch for code that needs to be
  updated...

  Done.
```

``` ucm :error
scratch/main> delete e f gg

  ⚠️

  I couldn't find any terms or types that match the name gg.
```

``` ucm :hide
scratch/main> project.delete scratch
```

If you try to delete only part of a cycle, you'll be put on an update branch.

``` ucm :hide
scratch/main> builtins.mergeio lib.builtins
```

``` unison
ping _ = 1 Nat.+ !pong
pong _ = 4 Nat.+ !ping
```

``` ucm :added-by-ucm
  Loading changes detected in scratch.u.

  + ping : 'Nat
  + pong : 'Nat

  Run `update` to apply these changes to your codebase.
```

``` ucm
scratch/main> update

  Okay, I'm searching the branch for code that needs to be
  updated...

  Done.
```

``` ucm :error
scratch/main> delete ping

  Some definitions depend on the ones you're trying to delete.
  I've added them to scratch.u, where you can fix them or
  comment them out. Once the file is compiling, run `update`.

  I've also switched you to a new branch update-main for this
  work. On `update`, it will be merged back into main.
```

``` unison :added-by-ucm scratch.u
-- The definitions below depend on the deleted definitions.
-- Please fix the errors and run `update`.

ping : 'Nat
ping _ =
  use Nat +
  1 + pong()

pong : 'Nat
pong _ =
  use Nat +
  4 + ping()

```

``` ucm :hide
scratch/main> project.delete scratch
```
