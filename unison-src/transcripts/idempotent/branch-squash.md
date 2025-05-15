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

  ⊙ 1. #espq7p1b0h

    + Adds / updates:
    
      x

  ⊙ 2. #jh9nta9qsm

    + Adds / updates:
    
      x

  ⊙ 3. #a7tu5cnnns

    + Adds / updates:
    
      x

  ⊙ 4. #3rl91blbdm

    + Adds / updates:
    
      x

  □ 5. #6lfucgcbgh (start of history)

scratch/squashed> history

  Note: The most recent namespace hash is immediately below this
        message.



  □ 1. #59schd8j6u (start of history)
```
