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

  ⊙ 1. #i0sej67h4a

    + Adds / updates:
    
      x

  ⊙ 2. #nlngksjc1l

    + Adds / updates:
    
      x

  ⊙ 3. #ob550hsn42

    + Adds / updates:
    
      x

  ⊙ 4. #0bnqj45uk5

    + Adds / updates:
    
      x

  □ 5. #59hpgl25eu (start of history)

scratch/squashed> history

  Note: The most recent namespace hash is immediately below this
        message.



  □ 1. #4pja85eqc3 (start of history)
```
