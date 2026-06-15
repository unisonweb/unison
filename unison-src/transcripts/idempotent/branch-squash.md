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

  ⊙ 1. #lgkul0pd7j

    + Adds / updates:
    
      x

  ⊙ 2. #47p1f3sh8n

    + Adds / updates:
    
      x

  ⊙ 3. #7rro124cg2

    + Adds / updates:
    
      x

  ⊙ 4. #g2a7gjijlo

    + Adds / updates:
    
      x

  □ 5. #9019oct232 (start of history)

scratch/squashed> history

  Note: The most recent namespace hash is immediately below this
        message.



  □ 1. #j8im1b5gr1 (start of history)
```
