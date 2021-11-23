# Behaviour of namespace histories during a merge.

Note: This is a descriptive test meant to capture the current behaviour of 
branch histories during a merge.
It isn't prescriptive about how merges _should_ work with respect to child branches, 
but I think we should at least notice if we change things by accident.


```unison
parent.top = "top"
parent.child.thing = "parent.child.thing"
```

```ucm

  I found and typechecked these definitions in scratch.u. If you
  do an `add` or `update`, here's how your codebase would
  change:
  
    ⍟ These new definitions are ok to `add`:
    
      parent.child.thing : Text
      parent.top         : Text

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
If we add another thing to the child namespace it should add another history node.

```unison
parent.child.thing2 = "parent.child.thing2"
```

```ucm

  I found and typechecked these definitions in scratch.u. If you
  do an `add` or `update`, here's how your codebase would
  change:
  
    ⍟ These new definitions are ok to `add`:
    
      parent.child.thing2 : Text

```
```ucm
.> add

  ⍟ I've added these definitions:
  
    parent.child.thing2 : Text

.> history parent.child

  Note: The most recent namespace hash is immediately below this
        message.
  
  ⊙ #o0ig5fooud
  
    + Adds / updates:
    
      thing2
  
  □ #0pu6u21kb4 (start of history)

```
Now we fork the parent namespace to make some changes.

```ucm
.> fork parent parentfork

  Done.

```
```unison
parentfork.child.thing3 = "parentfork.child.thing3"
```

```ucm

  I found and typechecked these definitions in scratch.u. If you
  do an `add` or `update`, here's how your codebase would
  change:
  
    ⍟ These new definitions are ok to `add`:
    
      parentfork.child.thing3 : Text

```
```ucm
.> add

  ⍟ I've added these definitions:
  
    parentfork.child.thing3 : Text

.> history parentfork.child

  Note: The most recent namespace hash is immediately below this
        message.
  
  ⊙ #8gv5c01mvv
  
    + Adds / updates:
    
      thing3
  
  ⊙ #o0ig5fooud
  
    + Adds / updates:
    
      thing2
  
  □ #0pu6u21kb4 (start of history)

```
Now, if I squash-merge parentfork back into parent, we expect `parentfork.child.thing3` to be added.

```ucm
.> merge.squash parentfork parent

  Here's what's changed in parent after the merge:
  
  Added definitions:
  
    1. child.thing3 : Text
  
  Tip: You can use `todo` to see if this generated any work to
       do in this namespace and `test` to run the tests. Or you
       can use `undo` or `reflog` to undo the results of this
       merge.

.> history parentfork

  Note: The most recent namespace hash is immediately below this
        message.
  
  ⊙ #ee2tj05pfq
  
    + Adds / updates:
    
      child.thing3
  
  ⊙ #9uakh0rhhe
  
    + Adds / updates:
    
      child.thing2
  
  □ #gdahjt281d (start of history)

```
Notice that with the current behaviour, the history of `parent.child` is completely wiped out.
This doesn't seem desirable.

```ucm
.> history parent.child

  Note: The most recent namespace hash is immediately below this
        message.
  
  
  
  □ #8u7hu07qs1 (start of history)

```
