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

  ⊙ 1. #flc0smih4l

    + Adds / updates:
    
      x

  ⊙ 2. #al8a7k4371

    + Adds / updates:
    
      x

  ⊙ 3. #17fj4051lo

    + Adds / updates:
    
      x

  ⊙ 4. #iva047vpnv

    + Adds / updates:
    
      x

  □ 5. #di82t6tedj (start of history)

scratch/squashed> history

  Note: The most recent namespace hash is immediately below this
        message.



  □ 1. #8altlvm3ne (start of history)
```
