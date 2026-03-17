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

  ⊙ 1. #qk35gi5ng3

    + Adds / updates:
    
      x

  ⊙ 2. #5b23c7f4rj

    + Adds / updates:
    
      x

  ⊙ 3. #akk9mr254l

    + Adds / updates:
    
      x

  ⊙ 4. #9vt20e0mk7

    + Adds / updates:
    
      x

  □ 5. #4jjs29c5kl (start of history)

scratch/squashed> history

  Note: The most recent namespace hash is immediately below this
        message.



  □ 1. #85bvopi5qp (start of history)
```
