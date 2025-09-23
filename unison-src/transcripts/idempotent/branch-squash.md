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

  ⊙ 1. #fnj0stqh6g

    + Adds / updates:
    
      x

  ⊙ 2. #mbmgtvli5m

    + Adds / updates:
    
      x

  ⊙ 3. #q0o0hd9nt5

    + Adds / updates:
    
      x

  ⊙ 4. #07c7og75u8

    + Adds / updates:
    
      x

  □ 5. #ok139603jg (start of history)

scratch/squashed> history

  Note: The most recent namespace hash is immediately below this
        message.



  □ 1. #f75c3n8sul (start of history)
```
