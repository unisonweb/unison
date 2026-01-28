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

  ⊙ 1. #hg2blpoh2c

    + Adds / updates:
    
      x

  ⊙ 2. #0lpf6e060a

    + Adds / updates:
    
      x

  ⊙ 3. #g4vcgfnl13

    + Adds / updates:
    
      x

  ⊙ 4. #smp5fnn4oo

    + Adds / updates:
    
      x

  □ 5. #2fjg519rpg (start of history)

scratch/squashed> history

  Note: The most recent namespace hash is immediately below this
        message.



  □ 1. #2mvajvqrfa (start of history)
```
