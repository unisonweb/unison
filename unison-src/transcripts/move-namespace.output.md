# Tests for `move.namespace`

## Happy path

Create a namespace and add some history to it

```unison
a.termInA = 1
unique type a.T = T
```

```ucm

  Loading changes detected in scratch.u.

  I found and typechecked these definitions in scratch.u. If you
  do an `add` or `update`, here's how your codebase would
  change:
  
    ⍟ These new definitions are ok to `add`:
    
      type a.T
      a.termInA : Nat

```
```ucm
.happy> add

  ⍟ I've added these definitions:
  
    type a.T
    a.termInA : Nat

```
```unison
a.termInA = 2
unique type a.T = T1 | T2
```

```ucm

  Loading changes detected in scratch.u.

  I found and typechecked these definitions in scratch.u. If you
  do an `add` or `update`, here's how your codebase would
  change:
  
    ⍟ These names already exist. You can `update` them to your
      new definition:
    
      type a.T
      a.termInA : Nat

```
```ucm
.happy> update

  Okay, I'm searching the branch for code that needs to be
  updated...

  Done.

```
Should be able to move the namespace, including its types, terms, and sub-namespaces.

```ucm
.happy> move.namespace a b

  Done.

.happy> ls b

  1. T       (type)
  2. T/      (2 terms)
  3. termInA (Nat)

.happy> history b

  Note: The most recent namespace hash is immediately below this
        message.
  
  ⊙ 1. #4j747vnmdk
  
    + Adds / updates:
    
      T T.T1 T.T2 termInA
    
    - Deletes:
    
      T.T
  
  □ 2. #r71j4144fe (start of history)

```
## Namespace history


Create some namespaces and add some history to them

```unison
a.termInA = 1
b.termInB = 10
```

```ucm

  Loading changes detected in scratch.u.

  I found and typechecked these definitions in scratch.u. If you
  do an `add` or `update`, here's how your codebase would
  change:
  
    ⍟ These new definitions are ok to `add`:
    
      a.termInA : Nat
      b.termInB : Nat

```
```ucm
.history> add

  ⍟ I've added these definitions:
  
    a.termInA : Nat
    b.termInB : Nat

```
```unison
a.termInA = 2
b.termInB = 11
```

```ucm

  Loading changes detected in scratch.u.

  I found and typechecked these definitions in scratch.u. If you
  do an `add` or `update`, here's how your codebase would
  change:
  
    ⍟ These names already exist. You can `update` them to your
      new definition:
    
      a.termInA : Nat
      b.termInB : Nat

```
```ucm
.history> update

  Okay, I'm searching the branch for code that needs to be
  updated...

  Done.

```
Deleting a namespace should not leave behind any history,
if we move another to that location we expect the history to simply be the history
of the moved namespace. 

```ucm
.history> delete.namespace b

  Done.

.history> move.namespace a b

  Done.

-- Should be the history from 'a'
.history> history b

  Note: The most recent namespace hash is immediately below this
        message.
  
  ⊙ 1. #j0cjjqepb3
  
    + Adds / updates:
    
      termInA
  
  □ 2. #m8smmmgjso (start of history)

-- Should be empty
.history> history a

  ☝️  The namespace .history.a is empty.

```
## Moving over an existing branch 

Create some namespace and add some history to them

```unison
a.termInA = 1
b.termInB = 10
```

```ucm

  Loading changes detected in scratch.u.

  I found and typechecked these definitions in scratch.u. If you
  do an `add` or `update`, here's how your codebase would
  change:
  
    ⍟ These new definitions are ok to `add`:
    
      a.termInA : Nat
      b.termInB : Nat

```
```ucm
.existing> add

  ⍟ I've added these definitions:
  
    a.termInA : Nat
    b.termInB : Nat

```
```unison
a.termInA = 2
b.termInB = 11
```

```ucm

  Loading changes detected in scratch.u.

  I found and typechecked these definitions in scratch.u. If you
  do an `add` or `update`, here's how your codebase would
  change:
  
    ⍟ These names already exist. You can `update` them to your
      new definition:
    
      a.termInA : Nat
      b.termInB : Nat

```
```ucm
.existing> update

  Okay, I'm searching the branch for code that needs to be
  updated...

  Done.

.existing> move.namespace a b

  ⚠️
  
  A branch existed at the destination: b so I over-wrote it.
  
  Tip: You can use `undo` or `reflog` to undo this change.

  Done.

```
## Moving the Root 

I should be able to move the root into a sub-namespace

```ucm
-- Should request confirmation
scratch/main> move.namespace . .root.at.path

  ⚠️
  
  Moves which affect the root branch cannot be undone, are you sure?
  Re-run the same command to proceed.

scratch/main> move.namespace . .root.at.path

  Done.

scratch/main> ls

  nothing to show

```

```ucm
-- Should request confirmationscratch/main> move.namespace . .root.at.pathscratch/main> move.namespace . .root.at.pathscratch/main> lsscratch/main> history
```


🛑

The transcript failed due to an error in the stanza above. The error is:


  nothing to show

