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

  ⊙ 1. #845vmgicj1

    + Adds / updates:
    
      x

  ⊙ 2. #1bcakfffd7

    + Adds / updates:
    
      x

  ⊙ 3. #rfl30jobh4

    + Adds / updates:
    
      x

  ⊙ 4. #sin1cq54jj

    + Adds / updates:
    
      x

  □ 5. #j56j30h46o (start of history)

scratch/squashed> history

  Note: The most recent namespace hash is immediately below this
        message.



  □ 1. #jjplou6296 (start of history)
```
