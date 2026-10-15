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

  ⊙ 1. #mbdd9lv1t9

    + Adds / updates:
    
      x

  ⊙ 2. #9vnl841itn

    + Adds / updates:
    
      x

  ⊙ 3. #pi16u3ob5c

    + Adds / updates:
    
      x

  ⊙ 4. #4kr509n35m

    + Adds / updates:
    
      x

  □ 5. #tv6soc7dvf (start of history)

scratch/squashed> history

  Note: The most recent namespace hash is immediately below this
        message.



  □ 1. #icis4t990n (start of history)
```
