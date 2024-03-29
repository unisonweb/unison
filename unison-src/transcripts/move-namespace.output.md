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
.> move.namespace . .root.at.path

  ⚠️
  
  Moves which affect the root branch cannot be undone, are you sure?
  Re-run the same command to proceed.

.> move.namespace . .root.at.path

  Done.

.> ls

  1. root/ (1373 terms, 214 types)

.> history

  Note: The most recent namespace hash is immediately below this
        message.
  
  
  
  □ 1. #vrn80pdffk (start of history)

```
```ucm
.> ls .root.at.path

  1. existing/ (457 terms, 71 types)
  2. happy/    (459 terms, 72 types)
  3. history/  (457 terms, 71 types)

.> history .root.at.path

  Note: The most recent namespace hash is immediately below this
        message.
  
  ⊙ 1. #g3ri07hi09
  
    - Deletes:
    
      existing.b.termInB
    
    > Moves:
    
      Original name      New name
      existing.a.termInA existing.b.termInA
  
  ⊙ 2. #ifjg1bj57v
  
    + Adds / updates:
    
      existing.a.termInA existing.b.termInB
    
    = Copies:
    
      Original name     New name(s)
      happy.b.termInA   existing.a.termInA
      history.b.termInA existing.a.termInA
  
  ⊙ 3. #bdn8f7vhg1
  
    + Adds / updates:
    
      existing.a.termInA existing.b.termInB
  
  ⊙ 4. #5dqmgnr0lt
  
    > Moves:
    
      Original name     New name
      history.a.termInA history.b.termInA
  
  ⊙ 5. #vd3d37rn3c
  
    - Deletes:
    
      history.b.termInB
  
  ⊙ 6. #gi32sh566a
  
    + Adds / updates:
    
      history.a.termInA history.b.termInB
    
    = Copies:
    
      Original name   New name(s)
      happy.b.termInA history.a.termInA
  
  ⊙ 7. #u2bs53f2hl
  
    + Adds / updates:
    
      history.a.termInA history.b.termInB
  
  ⊙ 8. #48hsm89mgl
  
    > Moves:
    
      Original name   New name
      happy.a.T       happy.b.T
      happy.a.T.T1    happy.b.T.T1
      happy.a.T.T2    happy.b.T.T2
      happy.a.termInA happy.b.termInA
  
  ⊙ 9. #pqd79g3q7l
  
    + Adds / updates:
    
      happy.a.T happy.a.T.T1 happy.a.T.T2 happy.a.termInA
    
    - Deletes:
    
      happy.a.T.T
  
  ⊙ 10. #allrjqq7ga
  
    + Adds / updates:
    
      happy.a.T happy.a.T.T happy.a.termInA
  
  There's more history before the versions shown here. Use
  `history #som3n4m3space` to view history starting from a given
  namespace hash.
  
  ⠇
  
  ⊙ 11. #ohd0a9rim1
  

```
I should be able to move a sub namespace _over_ the root.

```ucm
-- Should request confirmation
.> move.namespace .root.at.path.happy .

  ⚠️
  
  Moves which affect the root branch cannot be undone, are you sure?
  Re-run the same command to proceed.

.> move.namespace .root.at.path.happy .

  Done.

.> ls

  1. b/       (3 terms, 1 type)
  2. builtin/ (456 terms, 71 types)

.> history

  Note: The most recent namespace hash is immediately below this
        message.
  
  ⊙ 1. #lf3m1s2e7i
  
    + Adds / updates:
    
      b.T b.T.T1 b.T.T2 b.termInA
  
  ⊙ 2. #b1cg22v7s1
  
    - Deletes:
    
      a.T a.T.T1 a.T.T2 a.termInA
  
  ⊙ 3. #r83v608ifd
  
    + Adds / updates:
    
      a.T a.T.T1 a.T.T2 a.termInA
    
    - Deletes:
    
      a.T.T
  
  ⊙ 4. #pmm6a0f6fj
  
    + Adds / updates:
    
      a.T a.T.T a.termInA
  
  □ 5. #nmcjvlnbk1 (start of history)

```
```ucm
-- should be empty
.> ls .root.at.path.happy

  nothing to show

.> history .root.at.path.happy

  ☝️  The namespace .root.at.path.happy is empty.

```
