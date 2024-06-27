# reset loose code
```unison
a = 5
```

```ucm

  Loading changes detected in scratch.u.

  I found and typechecked these definitions in scratch.u. If you
  do an `add` or `update`, here's how your codebase would
  change:
  
    ⍟ These new definitions are ok to `add`:
    
      a : Nat

```
```ucm
scratch/main> add

  ⍟ I've added these definitions:
  
    a : Nat

scratch/main> history

  Note: The most recent namespace hash is immediately below this
        message.
  
  ⊙ 1. #d079vet1oj
  
    + Adds / updates:
    
      a
  
  □ 2. #4bigcpnl7t (start of history)

scratch/main> reset 2

  Done.

scratch/main> history

  Note: The most recent namespace hash is immediately below this
        message.
  
  
  
  □ 1. #4bigcpnl7t (start of history)

```
```unison
foo.a = 5
```

```ucm

  Loading changes detected in scratch.u.

  I found and typechecked these definitions in scratch.u. If you
  do an `add` or `update`, here's how your codebase would
  change:
  
    ⍟ These new definitions are ok to `add`:
    
      foo.a : Nat

```
```ucm
scratch/main> add

  ⍟ I've added these definitions:
  
    foo.a : Nat

scratch/main> ls foo

  1. a (Nat)

scratch/main> history

  Note: The most recent namespace hash is immediately below this
        message.
  
  ⊙ 1. #tfg7r9359n
  
    + Adds / updates:
    
      foo.a
  
  □ 2. #4bigcpnl7t (start of history)

scratch/main> reset 1 foo

  scratch/foo does not exist.

```

```ucm
scratch/main> addscratch/main> ls fooscratch/main> historyscratch/main> reset 1 fooscratch/main> ls foo.foo
```


🛑

The transcript failed due to an error in the stanza above. The error is:


  scratch/foo does not exist.

