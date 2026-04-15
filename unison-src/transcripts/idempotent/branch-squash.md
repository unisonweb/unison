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

  ⊙ 1. #43pu31p07u

    + Adds / updates:
    
      x

  ⊙ 2. #ae4ub8ehgk

    + Adds / updates:
    
      x

  ⊙ 3. #mqo83bm08q

    + Adds / updates:
    
      x

  ⊙ 4. #snstc7o035

    + Adds / updates:
    
      x

  □ 5. #mnb570q8mh (start of history)

scratch/squashed> history

  Note: The most recent namespace hash is immediately below this
        message.



  □ 1. #67ud50objj (start of history)
```
