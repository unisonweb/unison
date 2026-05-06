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

  ⊙ 1. #1dq700mljf

    + Adds / updates:
    
      x

  ⊙ 2. #q1usj4cf3e

    + Adds / updates:
    
      x

  ⊙ 3. #re62cihv49

    + Adds / updates:
    
      x

  ⊙ 4. #k6t1iq0ntr

    + Adds / updates:
    
      x

  □ 5. #5fabskb628 (start of history)

scratch/squashed> history

  Note: The most recent namespace hash is immediately below this
        message.



  □ 1. #7chdscmffv (start of history)
```
