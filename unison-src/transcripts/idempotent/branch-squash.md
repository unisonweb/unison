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

  ⊙ 1. #roi1cdeff0

    + Adds / updates:
    
      x

  ⊙ 2. #4h5qpk8kp0

    + Adds / updates:
    
      x

  ⊙ 3. #0pg92a97br

    + Adds / updates:
    
      x

  ⊙ 4. #5sfsmugq1b

    + Adds / updates:
    
      x

  □ 5. #jb1kikr96l (start of history)

scratch/squashed> history

  Note: The most recent namespace hash is immediately below this
        message.



  □ 1. #c71ojh03gh (start of history)
```
