<<<<<<< HEAD
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
||||||| 138799f92
=======
# Empty namespace behaviours

```unison
mynamespace.x = 1
```

The deleted namespace shouldn't appear in `ls` output.
```ucm
.> ls

  nothing to show

```
```ucm
.> ls.verbose

  😶
  
  No results. Check your spelling, or try using tab completion
  to supply command arguments.

```
```ucm
.> find mynamespace

  😶
  
  No results. Check your spelling, or try using tab completion
  to supply command arguments.

```
The history of the namespace should still exist if requested explicitly.

```ucm
.> history mynamespace

  Note: The most recent namespace hash is immediately below this
        message.
  
  ⊙ #qjc20aua9h
  
    - Deletes:
    
      x
  
  □ #hkrqt3tm05 (start of history)

```
Merging an empty namespace should still copy its history if it has some.

```ucm
  ☝️  The namespace .empty is empty.

.empty> history

  ☝️  The namespace .empty is empty.

.empty> merge .mynamespace

  Nothing changed as a result of the merge.

.empty> history

  Note: The most recent namespace hash is immediately below this
        message.
  
  ⊙ #qjc20aua9h
  
    - Deletes:
    
      x
  
  □ #hkrqt3tm05 (start of history)

```
>>>>>>> trunk
