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

  ⊙ 1. #ur0rbdhu3k

    + Adds / updates:
    
      x

  ⊙ 2. #9cghq8s9fe

    + Adds / updates:
    
      x

  ⊙ 3. #bp5434du8e

    + Adds / updates:
    
      x

  ⊙ 4. #6ibnj1eqiv

    + Adds / updates:
    
      x

  □ 5. #9eib022e46 (start of history)

scratch/squashed> history

  Note: The most recent namespace hash is immediately below this
        message.



  □ 1. #u4v1qgd97n (start of history)
```
