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

  ⊙ 1. #3fhetii4r1

    + Adds / updates:
    
      x

  ⊙ 2. #k9tpqec6l4

    + Adds / updates:
    
      x

  ⊙ 3. #kk8foejrsc

    + Adds / updates:
    
      x

  ⊙ 4. #r2agp6v66h

    + Adds / updates:
    
      x

  □ 5. #amjnc7l29n (start of history)

scratch/squashed> history

  Note: The most recent namespace hash is immediately below this
        message.



  □ 1. #n6bh8al7s0 (start of history)
```
