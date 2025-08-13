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

  ⊙ 1. #fu5h6cvpmr

    + Adds / updates:
    
      def

  ⊙ 2. #9lsqqgreh4

    + Adds / updates:
    
      def

  □ 3. #di82t6tedj (start of history)

scratch/main> reset 2

  Done.

scratch/main> view def

  def : Text
  def = "first value"

scratch/main> history

  Note: The most recent namespace hash is immediately below this
        message.

  ⊙ 1. #9lsqqgreh4

    + Adds / updates:
    
      def

  □ 2. #di82t6tedj (start of history)
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
  1.   scratch/main   #9lsqqgreh4   reset 9lsqqgreh4kum13a1ml5t6db3hjai463s6ci91fugbaeids1o1575s...
  2.   scratch/main   #fu5h6cvpmr   update
  3.   scratch/main   #9lsqqgreh4   update
  4.   scratch/main   #di82t6tedj   builtins.merge
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

  ⊙ 1. #fu5h6cvpmr

    + Adds / updates:
    
      def

  ⊙ 2. #9lsqqgreh4

    + Adds / updates:
    
      def

  □ 3. #di82t6tedj (start of history)
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
