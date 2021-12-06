# Empty namespace behaviours

## Operations on empty namespaces

Add and then delete a term to add some history to a deleted namespace.

```unison
deleted.x = 1
stuff.thing = 2
```

I should be allowed to fork over a deleted namespace

```ucm
.> fork stuff deleted

  Done.

```
The history from the `deleted` namespace should have been overwritten by the history from `stuff`.

```ucm
.> history stuff

  Note: The most recent namespace hash is immediately below this
        message.
  
  
  
  □ #3bm1524lb7 (start of history)

.> history deleted

  Note: The most recent namespace hash is immediately below this
        message.
  
  
  
  □ #3bm1524lb7 (start of history)

```
