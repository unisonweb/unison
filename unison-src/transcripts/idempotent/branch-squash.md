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

  ⊙ 1. #rfu9t6autm

    + Adds / updates:
    
      x

  ⊙ 2. #raor2k4vkl

    + Adds / updates:
    
      x

  ⊙ 3. #gphfrcekln

    + Adds / updates:
    
      x

  ⊙ 4. #arr94osl4n

    + Adds / updates:
    
      x

  □ 5. #5kp2m26mue (start of history)

scratch/squashed> history

  Note: The most recent namespace hash is immediately below this
        message.



  □ 1. #lqgns9j9e8 (start of history)
```
