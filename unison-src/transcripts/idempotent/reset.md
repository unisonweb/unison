``` ucm :hide
scratch/main> builtins.merge
```

``` unison
def = "first value"
```

``` ucm :added-by-ucm
  Loading changes detected in scratch.u.

  + def : Text

  Run `update` to apply these changes to your codebase.
```

``` ucm :hide
scratch/main> update
```

``` unison :hide
def = "second value"
```

Can reset to a value from history by number.

``` ucm
scratch/main> update

  Okay, I'm searching the branch for code that needs to be
  updated...

  Done.

scratch/main> history

  Note: The most recent namespace hash is immediately below this
        message.

  ⊙ 1. #i9aum8pf6t

    + Adds / updates:
    
      def

  ⊙ 2. #69ajhkth8f

    + Adds / updates:
    
      def

  □ 3. #2fjg519rpg (start of history)

scratch/main> reset 2

  Done.

scratch/main> view def

  def : Text
  def = "first value"

scratch/main> history

  Note: The most recent namespace hash is immediately below this
        message.

  ⊙ 1. #69ajhkth8f

    + Adds / updates:
    
      def

  □ 2. #2fjg519rpg (start of history)
```

Can reset to a value from reflog by number.

``` ucm
scratch/main> reflog

  Below is a record of recent changes, you can use
  `reset #abcdef` to reset the current branch to a previous
  state.

  Tip: Use `diff.namespace 1 7` to compare between points in
       history.

       Branch         Hash          Description
  1.   scratch/main   #69ajhkth8f   reset 69ajhkth8fl63u6f9bmcjftp81u1g1jarvb16v91kpaa7jcsf7oo7h...
  2.   scratch/main   #i9aum8pf6t   update
  3.   scratch/main   #69ajhkth8f   update
  4.   scratch/main   #2fjg519rpg   builtins.merge
  5.   scratch/main   #sg60bvjo91   Project Created

-- Reset the current branch to the first history element

scratch/main> reset 2

  Done.

scratch/main> view def

  def : Text
  def = "second value"

scratch/main> history

  Note: The most recent namespace hash is immediately below this
        message.

  ⊙ 1. #i9aum8pf6t

    + Adds / updates:
    
      def

  ⊙ 2. #69ajhkth8f

    + Adds / updates:
    
      def

  □ 3. #2fjg519rpg (start of history)
```

# reset branch

``` ucm
foo/main> history

  Note: The most recent namespace hash is immediately below this
        message.



  □ 1. #sg60bvjo91 (start of history)
```

``` unison :hide
a = 5
```

``` ucm
foo/main> update

  Okay, I'm searching the branch for code that needs to be
  updated...

  Done.

foo/empty> reset /main:

  Done.

foo/empty> view a

  a : ##Nat
  a = 5

foo/empty> history

  Note: The most recent namespace hash is immediately below this
        message.



  □ 1. #5l94rduvel (start of history)
```

## second argument is always interpreted as a branch

``` unison :hide
main.a = 3
```

``` ucm
foo/main> update

  Okay, I'm searching the branch for code that needs to be
  updated...

  Done.

foo/main> history

  Note: The most recent namespace hash is immediately below this
        message.

  ⊙ 1. #0i64kpfccl

    + Adds / updates:
    
      main.a

  □ 2. #5l94rduvel (start of history)

foo/main> reset 2 main

  Done.
```
