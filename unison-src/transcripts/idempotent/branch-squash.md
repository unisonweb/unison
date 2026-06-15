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

  ⊙ 1. #v19atr9817

    + Adds / updates:
    
      x

  ⊙ 2. #kkqmq48vmd

    + Adds / updates:
    
      x

  ⊙ 3. #g7unnmegs4

    + Adds / updates:
    
      x

  ⊙ 4. #urt27ba2o4

    + Adds / updates:
    
      x

  □ 5. #3o7lgbl4nb (start of history)

scratch/squashed> history

  Note: The most recent namespace hash is immediately below this
        message.



  □ 1. #d53eaafj3b (start of history)
```
