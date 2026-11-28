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

  ⊙ 1. #q0shvop9cc

    + Adds / updates:
    
      x

  ⊙ 2. #lkrkiuikba

    + Adds / updates:
    
      x

  ⊙ 3. #pq8e4uajjs

    + Adds / updates:
    
      x

  ⊙ 4. #f47qerrnns

    + Adds / updates:
    
      x

  □ 5. #f560v5e22a (start of history)

scratch/squashed> history

  Note: The most recent namespace hash is immediately below this
        message.



  □ 1. #t7i6qpvl0d (start of history)
```
