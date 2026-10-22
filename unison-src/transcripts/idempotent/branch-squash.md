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

  ⊙ 1. #9iqtlos824

    + Adds / updates:
    
      x

  ⊙ 2. #mdqegtkv67

    + Adds / updates:
    
      x

  ⊙ 3. #4mu45bnrfa

    + Adds / updates:
    
      x

  ⊙ 4. #h1gmvqfk17

    + Adds / updates:
    
      x

  □ 5. #v9kc666jae (start of history)

scratch/squashed> history

  Note: The most recent namespace hash is immediately below this
        message.



  □ 1. #8r2409u5ls (start of history)
```
