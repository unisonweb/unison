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

  1. root/ (1364 terms, 211 types)

.> history

  Note: The most recent namespace hash is immediately below this
        message.
  
  
  
  □ 1. #2vg6il9d4g (start of history)

```
```ucm
.> ls .root.at.path

  1. existing/ (454 terms, 70 types)
  2. happy/    (456 terms, 71 types)
  3. history/  (454 terms, 70 types)

.> history .root.at.path

  Note: The most recent namespace hash is immediately below this
        message.
  
  ⊙ 1. #m44k1k58ou
  
    - Deletes:
    
      existing.b.termInB
    
    > Moves:
    
      Original name      New name
      existing.a.termInA existing.b.termInA
  
  ⊙ 2. #7r4j4dmhru
  
    + Adds / updates:
    
      existing.a.termInA existing.b.termInB
    
    = Copies:
    
      Original name     New name(s)
      happy.b.termInA   existing.a.termInA
      history.b.termInA existing.a.termInA
  
  ⊙ 3. #qlh1ogmt6v
  
    + Adds / updates:
    
      existing.a.termInA existing.b.termInB
  
  ⊙ 4. #v3oiqufnsl
  
    > Moves:
    
      Original name     New name
      history.a.termInA history.b.termInA
  
  ⊙ 5. #g19oumvmue
  
    - Deletes:
    
      history.b.termInB
  
  ⊙ 6. #73204ctkae
  
    + Adds / updates:
    
      history.a.termInA history.b.termInB
    
    = Copies:
    
      Original name   New name(s)
      happy.b.termInA history.a.termInA
  
  ⊙ 7. #cgiold3l98
  
    + Adds / updates:
    
      history.a.termInA history.b.termInB
  
  ⊙ 8. #8qrf3qbi9i
  
    > Moves:
    
      Original name   New name
      happy.a.T       happy.b.T
      happy.a.T.T1    happy.b.T.T1
      happy.a.T.T2    happy.b.T.T2
      happy.a.termInA happy.b.termInA
  
  ⊙ 9. #e90bu6v600
  
    + Adds / updates:
    
      happy.a.T happy.a.T.T1 happy.a.T.T2 happy.a.termInA
    
    - Deletes:
    
      happy.a.T.T
  
  ⊙ 10. #lc2ce2arha
  
    + Adds / updates:
    
      happy.a.T happy.a.T.T happy.a.termInA
  
  There's more history before the versions shown here. Use
  `history #som3n4m3space` to view history starting from a given
  namespace hash.
  
  ⠇
  
  ⊙ 11. #9ck3j8r41m
  

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
  2. builtin/ (453 terms, 70 types)

.> history

  Note: The most recent namespace hash is immediately below this
        message.
  
  ⊙ 1. #ian8vro0cp
  
    + Adds / updates:
    
      b.T b.T.T1 b.T.T2 b.termInA
  
  ⊙ 2. #gt9figld7p
  
    - Deletes:
    
      a.T a.T.T1 a.T.T2 a.termInA
  
  ⊙ 3. #i8i5on9i6t
  
    + Adds / updates:
    
      a.T a.T.T1 a.T.T2 a.termInA
    
    - Deletes:
    
      a.T.T
  
  ⊙ 4. #ucfoesjvhd
  
    + Adds / updates:
    
      a.T a.T.T a.termInA
  
  □ 5. #1t2hb5o115 (start of history)

```
```ucm
-- should be empty
.> ls .root.at.path.happy

  nothing to show

.> history .root.at.path.happy

  ☝️  The namespace .root.at.path.happy is empty.

```
