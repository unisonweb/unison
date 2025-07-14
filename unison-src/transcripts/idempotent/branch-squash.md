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

  ⊙ 1. #23blkqptvu

    + Adds / updates:
    
      x

  ⊙ 2. #befp1th624

    + Adds / updates:
    
      x

  ⊙ 3. #i6e4fr9c5i

    + Adds / updates:
    
      x

  ⊙ 4. #oluch5r7s9

    + Adds / updates:
    
      x

  □ 5. #vom70vas4m (start of history)

scratch/squashed> history

  Note: The most recent namespace hash is immediately below this
        message.



  □ 1. #h7be92hjd5 (start of history)
```
