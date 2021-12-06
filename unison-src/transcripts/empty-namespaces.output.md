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
