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

  ⊙ 1. #nk8fadau8b

    + Adds / updates:
    
      x

  ⊙ 2. #5k1fa3g3ca

    + Adds / updates:
    
      x

  ⊙ 3. #re6grrt6br

    + Adds / updates:
    
      x

  ⊙ 4. #27q4dmc096

    + Adds / updates:
    
      x

  □ 5. #mtk142cec7 (start of history)

scratch/squashed> history

  Note: The most recent namespace hash is immediately below this
        message.



  □ 1. #1rgc0h5et4 (start of history)
```
