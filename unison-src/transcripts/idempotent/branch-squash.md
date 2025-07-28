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

  ⊙ 1. #pomn1vh95l

    + Adds / updates:
    
      x

  ⊙ 2. #0ri4if59ae

    + Adds / updates:
    
      x

  ⊙ 3. #l2shcvq9uh

    + Adds / updates:
    
      x

  ⊙ 4. #dj2hvpuiml

    + Adds / updates:
    
      x

  □ 5. #pvkf11c9e1 (start of history)

scratch/squashed> history

  Note: The most recent namespace hash is immediately below this
        message.



  □ 1. #54lk0h1sd5 (start of history)
```
