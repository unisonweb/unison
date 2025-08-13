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

  ⊙ 1. #hil73o0lp6

    + Adds / updates:
    
      x

  ⊙ 2. #2c48a507c7

    + Adds / updates:
    
      x

  ⊙ 3. #ijos4847ph

    + Adds / updates:
    
      x

  ⊙ 4. #7mf86j6b2q

    + Adds / updates:
    
      x

  □ 5. #jb7eefpht0 (start of history)

scratch/squashed> history

  Note: The most recent namespace hash is immediately below this
        message.



  □ 1. #eropq36mq0 (start of history)
```
