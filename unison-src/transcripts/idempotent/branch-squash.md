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

  ⊙ 1. #ulrioipiij

    + Adds / updates:
    
      x

  ⊙ 2. #tht7fh7hpr

    + Adds / updates:
    
      x

  ⊙ 3. #0g4innasp3

    + Adds / updates:
    
      x

  ⊙ 4. #bap8t2bbbd

    + Adds / updates:
    
      x

  □ 5. #lf5t3qlp7c (start of history)

scratch/squashed> history

  Note: The most recent namespace hash is immediately below this
        message.



  □ 1. #efjdidn6on (start of history)
```
