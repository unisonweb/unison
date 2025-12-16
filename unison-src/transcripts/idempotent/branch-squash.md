# `branch.squash`

``` ucm :hide
scratch/main> builtins.merge
```

Build up some history:

``` unison :hide
x = 1
```

``` ucm :hide
scratch/main> add
```

``` unison :hide
x = 2
```

``` ucm :hide
scratch/main> update
```

``` unison :hide
x = 3
```

``` ucm :hide
scratch/main> update
```

``` unison :hide
x = 4
```

``` ucm :hide
scratch/main> update
```

`branch.squash` with both a source and destination.

``` ucm
scratch/empty> branch.squash scratch/main /squashed

  I squashed scratch/main into scratch/squashed

scratch/main> history

  Note: The most recent namespace hash is immediately below this
        message.

  ⊙ 1. #cpvvv9m2dk

    + Adds / updates:
    
      x

  ⊙ 2. #5eq1vtjb7c

    + Adds / updates:
    
      x

  ⊙ 3. #5f3b9fk6eh

    + Adds / updates:
    
      x

  ⊙ 4. #eog1kfie9m

    + Adds / updates:
    
      x

  □ 5. #h650g9m6mf (start of history)

scratch/squashed> history

  Note: The most recent namespace hash is immediately below this
        message.



  □ 1. #vobmfemjjf (start of history)
```
