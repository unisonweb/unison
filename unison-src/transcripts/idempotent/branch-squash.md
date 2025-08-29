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

  ⊙ 1. #g5q8j1tnat

    + Adds / updates:
    
      x

  ⊙ 2. #beipks6cla

    + Adds / updates:
    
      x

  ⊙ 3. #udob7go8p8

    + Adds / updates:
    
      x

  ⊙ 4. #ru3lrp3lf3

    + Adds / updates:
    
      x

  □ 5. #0tb16313an (start of history)

scratch/squashed> history

  Note: The most recent namespace hash is immediately below this
        message.



  □ 1. #tbhenebf83 (start of history)
```
