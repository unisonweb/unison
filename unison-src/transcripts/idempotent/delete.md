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

  I deleted these terms:

    1. foo

  Tip: You can use `undo` or use a hash from `reflog` to undo
       this change.

scratch/main> delete Foo

  I deleted these types:

    1. Foo

  Tip: You can use `undo` or use a hash from `reflog` to undo
       this change.

scratch/main> ls

  1. lib. (1011 terms, 150 types)
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

  I deleted these types:

    1. foo.x

  I deleted these terms:

    2. bar.x
    3. foo.x

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

  I deleted these terms:

    1. Foo.Foo

  Tip: You can use `undo` or use a hash from `reflog` to undo
       this change.

scratch/main> ls

  1. Foo  (type)
  2. lib. (1011 terms, 150 types)
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
  one term with the name `a.foo`. Please `move.term` or
  `delete.term.force` all but one of them, then try again.
```

``` ucm
scratch/main> delete.force a.foo

  I deleted these terms:

    1. a.foo

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
  one type with the name `a.Foo`. Please `move.type` or
  `delete.type.force` all but one of them, then try again.
```

``` ucm
scratch/main> delete.force a.Foo

  I deleted these types:

    1. a.Foo

  Tip: You can use `undo` or use a hash from `reflog` to undo
       this change.

scratch/main> delete.force a.Foo.Foo

  I deleted these terms:

    1. a.Foo.Foo

  Tip: You can use `undo` or use a hash from `reflog` to undo
       this change.
```

``` ucm :hide
scratch/main> project.delete scratch
```

Deleting something from `lib.*` is possible with `delete.force`, not `delete`.

``` ucm :hide
scratch/main> builtins.mergeio lib.builtins
```

``` unison :hide
foo = 17
```

``` ucm :hide
scratch/main> update

scratch/main> move foo lib.foo.foo
```

``` ucm :error
scratch/main> delete lib.foo.foo

  ⚠️

  I couldn't find any terms or types that match the name
  lib.foo.foo.
```

``` ucm
scratch/main> delete.force lib.foo.foo

  I deleted these terms:

    1. lib.foo.foo

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

  I deleted these types:

    1. foo

  I deleted these terms:

    2. foo

  Tip: You can use `undo` or use a hash from `reflog` to undo
       this change.

scratch/main> ls

  1. lib. (1011 terms, 150 types)
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

  I deleted these terms:

    1. a
    2. b
    3. c

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

  I deleted these types:

    1. Foo

  I deleted these terms:

    2. a
    3. b
    4. c

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

  I couldn't complete the delete, because some definitions are
  still in use.

  I've created a temporary branch and added the affected
  definitions to scratch.u, where you can fix them up or remove
  any that are obsolete.

  Once you're happy with the results, use `update` to merge them
  back into main, or `cancel` if you change your mind.
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

  I deleted these terms:

    1. e
    2. f
    3. g
    4. h

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

  I deleted these types:

    1. Foo

  I deleted these terms:

    2. incrementFoo

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

  I couldn't complete the delete, because some definitions are
  still in use.

  I've created a temporary branch and added the affected
  definitions to scratch.u, where you can fix them up or remove
  any that are obsolete.

  Once you're happy with the results, use `update` to merge them
  back into main, or `cancel` if you change your mind.
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
