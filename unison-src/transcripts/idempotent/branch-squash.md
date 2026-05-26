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

  ⊙ 1. #ng0fmbmje9

    + Adds / updates:
    
      x

  ⊙ 2. #clejlftt7g

    + Adds / updates:
    
      x

  ⊙ 3. #s8siq5bu1q

    + Adds / updates:
    
      x

  ⊙ 4. #pihphke6jf

    + Adds / updates:
    
      x

  □ 5. #itq9ivpi7l (start of history)

scratch/squashed> history

  Note: The most recent namespace hash is immediately below this
        message.



  □ 1. #20icu97fmp (start of history)
```
