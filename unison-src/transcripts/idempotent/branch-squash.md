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

  ⊙ 1. #t3eju4ghdi

    + Adds / updates:
    
      x

  ⊙ 2. #739g0tn75d

    + Adds / updates:
    
      x

  ⊙ 3. #38849uunfj

    + Adds / updates:
    
      x

  ⊙ 4. #qp8r2qp50b

    + Adds / updates:
    
      x

  □ 5. #ih6a59ec8v (start of history)

scratch/squashed> history

  Note: The most recent namespace hash is immediately below this
        message.



  □ 1. #f1bsrkultm (start of history)
```
