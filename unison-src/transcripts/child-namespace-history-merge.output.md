# Behaviour of namespace histories during a merge.

Note: This is a descriptive test meant to capture the current behaviour of 
branch histories during a merge.
It isn't prescriptive about how merges _should_ work with respect to child branches, 
but I think we should at least notice if we change things by accident.


## Setting up some history

```unison
parent.top = "top"
parent.child.thing = "parent.child.thing"
```

The child branch has a single history node representing the addition of `parent.child.thing`.

```ucm
.> add

  ⍟ I've added these definitions:
  
    parent.child.thing : Text
    parent.top         : Text

.> history parent.child

  Note: The most recent namespace hash is immediately below this
        message.
  
  
  
  □ #0pu6u21kb4 (start of history)

```
If we add another thing to the child namespace it should add another history node to both the child and parent.

```unison
parent.child.thing2 = "parent.child.thing2"
```

```ucm
.> add

  ⍟ I've added these definitions:
  
    parent.child.thing2 : Text

.> history parent

  Note: The most recent namespace hash is immediately below this
        message.
  
  ⊙ #9uakh0rhhe
  
    + Adds / updates:
    
      child.thing2
  
  □ #gdahjt281d (start of history)

.> history parent.child

  Note: The most recent namespace hash is immediately below this
        message.
  
  ⊙ #o0ig5fooud
  
    + Adds / updates:
    
      thing2
  
  □ #0pu6u21kb4 (start of history)

```
## Forking off some history on a separate branch

Now we fork the parent namespace to make some changes.

```ucm
.> fork parent parent_fork

  Done.

```
```unison
parent_fork.child.thing3 = "parent_fork.child.thing3"
```

The child should have a new history node after adding `thing3`

```ucm
.> add

  ⍟ I've added these definitions:
  
    parent_fork.child.thing3 : Text

.> history parent_fork.child

  Note: The most recent namespace hash is immediately below this
        message.
  
  ⊙ #tppoolbkem
  
    + Adds / updates:
    
      thing3
  
  ⊙ #o0ig5fooud
  
    + Adds / updates:
    
      thing2
  
  □ #0pu6u21kb4 (start of history)

```
## Saving our parent state

Split off two separate forks, one for testing squash merges, one for standard merges.

## Squash merge

For a squash merge, when I squash-merge back into parent, we expect `parent_fork.child.thing3` to be added.

```ucm
.> merge.squash parent_fork parent_squash_base

  Here's what's changed in parent_squash_base after the merge:
  
  Added definitions:
  
    1. child.thing3 : Text
  
  Tip: You can use `todo` to see if this generated any work to
       do in this namespace and `test` to run the tests. Or you
       can use `undo` or `reflog` to undo the results of this
       merge.

.> history parent_squash_base

  Note: The most recent namespace hash is immediately below this
        message.
  
  ⊙ #0ddrpnkqfj
  
    + Adds / updates:
    
      child.thing3
  
  ⊙ #9uakh0rhhe
  
    + Adds / updates:
    
      child.thing2
  
  □ #gdahjt281d (start of history)

```
Notice that with the current behaviour, the history of `parent.child` is completely wiped out, containing nothing from the source OR destination.

```ucm
.> history parent.child

  Note: The most recent namespace hash is immediately below this
        message.
  
  ⊙ #o0ig5fooud
  
    + Adds / updates:
    
      thing2
  
  □ #0pu6u21kb4 (start of history)

.> history parent_fork.child

  Note: The most recent namespace hash is immediately below this
        message.
  
  ⊙ #tppoolbkem
  
    + Adds / updates:
    
      thing3
  
  ⊙ #o0ig5fooud
  
    + Adds / updates:
    
      thing2
  
  □ #0pu6u21kb4 (start of history)

.> history parent_squash_base.child

  Note: The most recent namespace hash is immediately below this
        message.
  
  
  
  □ #mjdnabl5c5 (start of history)

```
## Standard merge

For a standard merge, if I merge back into parent, we expect `parent_fork.child.thing3` to be added.

```ucm
.> merge parent_fork parent_merge_base

  Here's what's changed in parent_merge_base after the merge:
  
  Added definitions:
  
    1. child.thing3 : Text
  
  Tip: You can use `todo` to see if this generated any work to
       do in this namespace and `test` to run the tests. Or you
       can use `undo` or `reflog` to undo the results of this
       merge.

.> history parent_merge_base

  Note: The most recent namespace hash is immediately below this
        message.
  
  ⊙ #ug6q7n5bos
  
    + Adds / updates:
    
      child.thing3
  
  ⊙ #9uakh0rhhe
  
    + Adds / updates:
    
      child.thing2
  
  □ #gdahjt281d (start of history)

```
Child histories should also be *merged*.

```ucm
.> history parent.child

  Note: The most recent namespace hash is immediately below this
        message.
  
  ⊙ #o0ig5fooud
  
    + Adds / updates:
    
      thing2
  
  □ #0pu6u21kb4 (start of history)

.> history parent_fork.child

  Note: The most recent namespace hash is immediately below this
        message.
  
  ⊙ #tppoolbkem
  
    + Adds / updates:
    
      thing3
  
  ⊙ #o0ig5fooud
  
    + Adds / updates:
    
      thing2
  
  □ #0pu6u21kb4 (start of history)

.> history parent_merge_base.child

  Note: The most recent namespace hash is immediately below this
        message.
  
  ⊙ #tppoolbkem
  
    + Adds / updates:
    
      thing3
  
  ⊙ #o0ig5fooud
  
    + Adds / updates:
    
      thing2
  
  □ #0pu6u21kb4 (start of history)

```
