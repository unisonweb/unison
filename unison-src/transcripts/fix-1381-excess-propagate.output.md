We were seeing an issue where (it seemed) that every namespace that was visited during a propagate would get a new history node, even when it didn't contain any dependents.

Example:
```unison
a = "a term"
X.foo = "a namespace"
```

```ucm
.> add

  ⍟ I've added these definitions:
  
    X.foo : ##Text
    a     : ##Text

```
Here is an update which should not affect `X`:
```unison
a = "an update"
```

```ucm
.> update

  ⍟ I've updated these names to your new definition:
  
    a : ##Text

```
As of the time of this writing, the history for `X` should be a single node, `#4eeuo5bsfr`;
```ucm
.> history X

  Note: The most recent namespace hash is immediately below this
        message.
  
  
  
  □ #das1se4g2i (start of history)

```
however, as of release/M1i, we saw an extraneous node appear.  If your `ucm` is fixed, you won't see it below:
```ucm
.> history #7nl6ppokhg

  😶
  
  I don't know of a namespace with that hash.

```
