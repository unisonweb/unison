# Resolving edit conflicts in `ucm`

The `ucm` tool tracks edits to hashes in an object called a _patch_. When patches get merged, sometimes those patches will have conflicting edits. The `replace.term` command helps resolve such conflicts.

First, let's make a new namespace, `example.resolve`:

```ucm
.> cd example.resolve

  ☝️  The namespace .example.resolve is empty.

```
Now let's add a term named `a.foo`:

```unison
a.foo = 42
```

```ucm

  I found and typechecked these definitions in scratch.u. If you
  do an `add` or `update`, here's how your codebase would
  change:
  
    ⍟ These new definitions are ok to `add`:
    
      a.foo : Nat
   
  Now evaluating any watch expressions (lines starting with
  `>`)... Ctrl+C cancels.

```
```ucm
.example.resolve> add

  ⍟ I've added these definitions:
  
    a.foo : Nat

```
We'll fork the namespace `a` into a new namespace `b`, so we can edit the two concurrently.

```ucm
.example.resolve> fork a b

  Done.

```
We'll also make a second fork `c` which we'll use as the target for our patch later.

```ucm
.example.resolve> fork a c

  Done.

```
Now let's make a change to `foo` in the `a` namespace:

```ucm
.example.resolve> cd a

```
```unison
foo = 43
```

```ucm

  I found and typechecked these definitions in scratch.u. If you
  do an `add` or `update`, here's how your codebase would
  change:
  
    ⍟ These new definitions will replace existing ones of the
      same name and are ok to `update`:
    
      foo : Nat
   
  Now evaluating any watch expressions (lines starting with
  `>`)... Ctrl+C cancels.

```
```ucm
.example.resolve.a> update

  ⍟ I've updated to these definitions:
  
    foo : .builtin.Nat

```
And make a different change in the `b` namespace:

```ucm
.example.resolve> cd .example.resolve.b

```
```unison
foo = 44
```

```ucm

  I found and typechecked these definitions in scratch.u. If you
  do an `add` or `update`, here's how your codebase would
  change:
  
    ⍟ These new definitions will replace existing ones of the
      same name and are ok to `update`:
    
      foo : Nat
   
  Now evaluating any watch expressions (lines starting with
  `>`)... Ctrl+C cancels.

```
```ucm
.example.resolve.b> update

  ⍟ I've updated to these definitions:
  
    foo : .builtin.Nat

```
The `a` and `b` namespaces now each contain a patch named `patch`. We can view these:

```ucm
.example.resolve.b> cd .example.resolve

.example.resolve> view.patch a.patch

  Edited Terms: c.foo -> a.foo

.example.resolve> view.patch b.patch

  Edited Terms: c.foo -> b.foo

```
Let's now merge these namespaces into `c`:

```ucm
.example.resolve> merge a c

  Here's what's changed in c after the merge:
  
  Updates:
  
    1. foo : Nat
       ↓
    2. foo : Nat
  
  Added definitions:
  
    3. patch patch (added 1 updates)
  
  Tip: You can use `todo` to see if this generated any work to
       do in this namespace and `test` to run the tests. Or you
       can use `undo` or `reflog` to undo the results of this
       merge.

```
```ucm
.example.resolve> merge b c

  Here's what's changed in c after the merge:
  
  New name conflicts:
  
    1. foo#jdqo : Nat
       ↓
    2. ┌ foo#8e68 : Nat
    3. └ foo#jdqo : Nat
  
  Updates:
  
    4. patch patch (added 1 updates)
  
  Tip: You can use `todo` to see if this generated any work to
       do in this namespace and `test` to run the tests. Or you
       can use `undo` or `reflog` to undo the results of this
       merge.

  I tried to auto-apply the patch, but couldn't because it
  contained contradictory entries.

```
The namespace `c` now has an edit conflict, since the term `foo` was edited in two different ways.

```ucm
.example.resolve> cd c

.example.resolve.c> todo

  ❓
  
  These definitions were edited differently in namespaces that
  have been merged into this one. You'll have to tell me what to
  use as the new definition:
  
    The term foo#4495 was replaced with foo#8e68 and foo#jdqo

```
We see that `#44954ulpdf` (the original hash of `a.foo`) got replaced with _both_ the `#8e68dvpr0a` and `#jdqoenu794`.

We can resolve this conflict by picking one of the terms as the "winner":

```ucm
.example.resolve.c> replace.term #44954ulpdf #8e68dvpr0a

  Done.

```
This changes the merged `c.patch` so that only the edit from #44954ulpdf to  #8e68dvpr0a remains:

```ucm
.example.resolve.c> view.patch

  Edited Terms: foo#4495 -> foo#8e68

```
We still have a remaining _name conflict_ since it just so happened that both of the definitions in the edits were named `foo`.

```ucm
.example.resolve.c> todo

  ❓
  
  These terms have conflicting definitions: foo
  
  Tip: This occurs when merging branches that both independently
       introduce the same name. Use `view foo` to see the
       conflicting defintions, then use `move.term` to resolve
       the conflicts.

```
We can resolve the name conflict by deleting one of the names.

```ucm
.example.resolve.c> delete.term foo#jdqoenu794

  Resolved name conflicts:
  
    1. ┌ example.resolve.c.foo#8e68 : Nat
    2. └ example.resolve.c.foo#jdqo : Nat
       ↓
    3. example.resolve.c.foo#8e68 : Nat
  
  Name changes:
  
    Original                         Changes
    4. example.resolve.a.foo      ┐  5. example.resolve.c.foo#jdqo (removed)
    6. example.resolve.c.foo#jdqo ┘  
  
  Tip: You can use `undo` or `reflog` to undo this change.

```
And that's how you resolve edit conflicts with UCM.
