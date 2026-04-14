# History Comments transcript

``` ucm :hide
scratch/main> builtins.merge lib.builtins
```

``` unison :hide
x = 1
```

``` ucm
scratch/main> add

  Okay, I'm searching the branch for code that needs to be
  updated...

  Done.

scratch/main> config.set author.name Unison

scratch/main> history.comment /main: "Initial commit with variable x set to 1"

  Done.

scratch/main> alias.term x y

  Done.

scratch/main> history.comment /main: "Renamed x to y"

  Done.

scratch/main> history

  Note: The most recent namespace hash is immediately below this
        message.

  ⊙ Unison
    ┃ Renamed x to y

  ⊙ 1. #tnqqdjcf4b

    + Adds / updates:
    
      y
    
    = Copies:
    
      Original name New name(s)
      x             y

  ⊙ Unison
    ┃ Initial commit with variable x set to 1

  ⊙ 2. #imllp7sf69

    + Adds / updates:
    
      x

  □ 3. #qm2u4k0jdr (start of history)
```
