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

  ⊙ 1. #59jkfrkg7i

    + Adds / updates:
    
      x

  ⊙ 2. #ufle5spg6j

    + Adds / updates:
    
      x

  ⊙ 3. #k5u0rhqrbp

    + Adds / updates:
    
      x

  ⊙ 4. #522l0st8ge

    + Adds / updates:
    
      x

  □ 5. #8met3khuqi (start of history)

scratch/squashed> history

  Note: The most recent namespace hash is immediately below this
        message.



  □ 1. #5i9rr5jlvl (start of history)
```
