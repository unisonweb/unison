# Delete

The delete command can delete both terms and types, as long as it's given an
unambiguous name.

First, let's make sure it complains when we try to delete a name that doesn't
exist.

```ucm
.> delete foo

  ⚠️
  
  I don't know about that name.

```
Now for some easy cases. Deleting an unambiguous term, then deleting an
unambiguous type.

```unison
foo = 1
type Foo = Foo Nat
```

```ucm

  I found and typechecked these definitions in scratch.u. If you
  do an `add` or `update`, here's how your codebase would
  change:
  
    ⍟ These new definitions are ok to `add`:
    
      type Foo
      foo : Nat
   
  Now evaluating any watch expressions (lines starting with
  `>`)... Ctrl+C cancels.

```
```ucm
.> add

  ⍟ I've added these definitions:
  
    type Foo
    foo : Nat

.> delete foo

  🆕
  
  Here's what's changed after the delete:
  
  - Deletes:
  
    foo
  
  Tip: You can always `undo` if this wasn't what you wanted.

.> delete Foo

  🆕
  
  Here's what's changed after the delete:
  
  - Deletes:
  
    Foo
  
  Tip: You can always `undo` if this wasn't what you wanted.

.> delete Foo.Foo

  🆕
  
  Here's what's changed after the delete:
  
  - Deletes:
  
    Foo.Foo
  
  Tip: You can always `undo` if this wasn't what you wanted.

```
How about an ambiguous term?

```unison
foo = 1
```

```ucm

  I found and typechecked these definitions in scratch.u. If you
  do an `add` or `update`, here's how your codebase would
  change:
  
    ⍟ These new definitions are ok to `add`:
    
      foo : Nat
   
  Now evaluating any watch expressions (lines starting with
  `>`)... Ctrl+C cancels.

```
```ucm
  ☝️  The namespace .a is empty.

.a> add

  ⍟ I've added these definitions:
  
    foo : Nat

```
```unison
foo = 2
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
  ☝️  The namespace .b is empty.

.b> add

  ⍟ I've added these definitions:
  
    foo : Nat

.a> merge .b

  🆕
  
  Here's what's changed in the current namespace after the merge:
  
  + Adds / updates:
  
    foo
  
  Tip: You can always `undo` if this wasn't what you wanted.

```
```ucm
.a> delete foo

  🤔
  
  That name is ambiguous. It could refer to any of the following
  definitions:
  
    foo#0ja1qfpej6
    foo#jk19sm5bf8
  
  You may:
  
    * Delete one by an unambiguous name, given above.
    * Delete them all by re-issuing the previous command.

```
I can force my delete through by re-issuing the command.

```ucm
.a> delete foo

  🆕
  
  Here's what's changed after the delete:
  
  - Deletes:
  
    a.foo
  
  Tip: You can always `undo` if this wasn't what you wanted.

```
Let's repeat all that on a type, for completeness.

```unison
type Foo = Foo Nat
```

```ucm

  I found and typechecked these definitions in scratch.u. If you
  do an `add` or `update`, here's how your codebase would
  change:
  
    ⍟ These new definitions are ok to `add`:
    
      type Foo
   
  Now evaluating any watch expressions (lines starting with
  `>`)... Ctrl+C cancels.

```
```ucm
.a> add

  ⍟ I've added these definitions:
  
    type Foo

```
```unison
type Foo = Foo Boolean
```

```ucm

  I found and typechecked these definitions in scratch.u. If you
  do an `add` or `update`, here's how your codebase would
  change:
  
    ⍟ These new definitions will replace existing ones of the
      same name and are ok to `update`:
    
      type Foo
   
  Now evaluating any watch expressions (lines starting with
  `>`)... Ctrl+C cancels.

```
```ucm
.b> add

  ⍟ I've added these definitions:
  
    type Foo

.a> merge .b

  🆕
  
  Here's what's changed in the current namespace after the merge:
  
  + Adds / updates:
  
    Foo foo Foo.Foo
  
  Tip: You can always `undo` if this wasn't what you wanted.

```
```ucm
.a> delete Foo

  🤔
  
  That name is ambiguous. It could refer to any of the following
  definitions:
  
    Foo#d97e0jhkmd
    Foo#gq9inhvg9h
  
  You may:
  
    * Delete one by an unambiguous name, given above.
    * Delete them all by re-issuing the previous command.

```
```ucm
.a> delete Foo

  🆕
  
  Here's what's changed after the delete:
  
  - Deletes:
  
    a.Foo
  
  Tip: You can always `undo` if this wasn't what you wanted.

```
```ucm
.a> delete Foo.Foo

  🤔
  
  That name is ambiguous. It could refer to any of the following
  definitions:
  
    Foo.Foo#d97e0jhkmd#0
    Foo.Foo#gq9inhvg9h#0
  
  You may:
  
    * Delete one by an unambiguous name, given above.
    * Delete them all by re-issuing the previous command.

```
```ucm
.a> delete Foo.Foo

  🆕
  
  Here's what's changed after the delete:
  
  - Deletes:
  
    a.Foo.Foo
  
  Tip: You can always `undo` if this wasn't what you wanted.

```
Finally, let's try to delete a term and a type with the same name.

```unison
foo = 1
type foo = Foo Nat
```

```ucm

  I found and typechecked these definitions in scratch.u. If you
  do an `add` or `update`, here's how your codebase would
  change:
  
    ⍟ These new definitions are ok to `add`:
    
      type foo
    
    ⍟ These new definitions will replace existing ones of the
      same name and are ok to `update`:
    
      foo : Nat
   
  Now evaluating any watch expressions (lines starting with
  `>`)... Ctrl+C cancels.

```
```ucm
.> add

  ⍟ I've added these definitions:
  
    type foo
    foo : Nat

```
```ucm
.> delete foo

  🤔
  
  That name is ambiguous. It could refer to any of the following
  definitions:
  
    foo#jk19sm5bf8
    foo#d97e0jhkmd
  
  You may:
  
    * Delete one by an unambiguous name, given above.
    * Delete them all by re-issuing the previous command.

```
```ucm
.> delete foo

  🆕
  
  Here's what's changed after the delete:
  
  - Deletes:
  
    foo
  
  Tip: You can always `undo` if this wasn't what you wanted.

```
