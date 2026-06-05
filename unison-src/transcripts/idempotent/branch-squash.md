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

  ⊙ 1. #mr8j40c256

    + Adds / updates:
    
      x

  ⊙ 2. #rlq0n9jaoi

    + Adds / updates:
    
      x

  ⊙ 3. #t1dpnor0ta

    + Adds / updates:
    
      x

  ⊙ 4. #5cviggrv9e

    + Adds / updates:
    
      x

  □ 5. #sgjtfcs1c0 (start of history)

scratch/squashed> history

  Note: The most recent namespace hash is immediately below this
        message.



  □ 1. #svco42bsps (start of history)
```
