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

  ⊙ 1. #6eq23mu1oc

    + Adds / updates:
    
      x

  ⊙ 2. #j81ekmq0nv

    + Adds / updates:
    
      x

  ⊙ 3. #1b6r6k71gp

    + Adds / updates:
    
      x

  ⊙ 4. #57n9csp92a

    + Adds / updates:
    
      x

  □ 5. #jd2fp908q2 (start of history)

scratch/squashed> history

  Note: The most recent namespace hash is immediately below this
        message.



  □ 1. #as1jsqns7j (start of history)
```
