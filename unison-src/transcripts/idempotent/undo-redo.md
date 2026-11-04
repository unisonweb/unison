# Undo and redo

`undo` and `redo` move a branch along its reflog, one command at a time, however many history steps a command made.

``` ucm :hide
scratch/main> builtins.merge lib.builtins
```

``` unison :hide
foo = 1
foo.tests.example = 2
other = 3
```

``` ucm
scratch/main> update

  Okay, I'm searching the branch for code that needs to be
  updated...

  Done.
```

A `move` of a term and a namespace is several steps of history, and one `undo` reverses all of it.

``` ucm
scratch/main> move foo bar

  Done.

scratch/main> undo

  Here are the changes I undid

  Name changes:

    Original                Changes
    1. foo.tests.example    2. bar.tests.example (added)
                            3. foo.tests.example (removed)
    
    4. foo                  5. bar (added)
                            6. foo (removed)

scratch/main> find-in foo

  1. tests.example : Nat

scratch/main> names foo

  'foo':
  Hash          Kind   Names
  #gjmq673r1v   Term   foo
```

`redo` brings it back, and they can be repeated in any mix.

``` ucm
scratch/main> redo

  Here are the changes I redid

  Name changes:

    Original                Changes
    1. foo.tests.example    2. bar.tests.example (added)
                            3. foo.tests.example (removed)
    
    4. foo                  5. bar (added)
                            6. foo (removed)

scratch/main> names bar

  'bar':
  Hash          Kind   Names
  #gjmq673r1v   Term   bar

scratch/main> alias.term other other2

  Done.

scratch/main> alias.term other other3

  Done.

scratch/main> undo

  Here are the changes I undid

  Name changes:

    Original     Changes
    1. other  ┐  2. other3 (added)
    3. other2 ┘  

scratch/main> undo

  Here are the changes I undid

  Name changes:

    Original    Changes
    1. other    2. other2 (added)

scratch/main> undo

  Here are the changes I undid

  Name changes:

    Original                Changes
    1. foo.tests.example    2. bar.tests.example (added)
                            3. foo.tests.example (removed)
    
    4. foo                  5. bar (added)
                            6. foo (removed)

scratch/main> names foo

  'foo':
  Hash          Kind   Names
  #gjmq673r1v   Term   foo

scratch/main> redo

  Here are the changes I redid

  Name changes:

    Original                Changes
    1. foo.tests.example    2. bar.tests.example (added)
                            3. foo.tests.example (removed)
    
    4. foo                  5. bar (added)
                            6. foo (removed)

scratch/main> redo

  Here are the changes I redid

  Name changes:

    Original    Changes
    1. other    2. other2 (added)

scratch/main> undo

  Here are the changes I undid

  Name changes:

    Original    Changes
    1. other    2. other2 (added)

scratch/main> redo

  Here are the changes I redid

  Name changes:

    Original    Changes
    1. other    2. other2 (added)

scratch/main> redo

  Here are the changes I redid

  Name changes:

    Original     Changes
    1. other  ┐  2. other3 (added)
    3. other2 ┘  

scratch/main> names other3

  'other3':
  Hash          Kind   Names
  #f3lgjvjqoo   Term   other, other2, other3
```

`redo` stops at the newest state.

``` ucm :error
scratch/main> redo

  ⚠️

  Nothing more to redo.
```

A new change after an `undo` replaces what could be redone.

``` ucm
scratch/main> undo

  Here are the changes I undid

  Name changes:

    Original     Changes
    1. other  ┐  2. other3 (added)
    3. other2 ┘  

scratch/main> alias.term other other4

  Done.
```

``` ucm :error
scratch/main> redo

  ⚠️

  Nothing more to redo.
```

``` ucm
scratch/main> undo

  Here are the changes I undid

  Name changes:

    Original     Changes
    1. other  ┐  2. other4 (added)
    3. other2 ┘  

scratch/main> redo

  Here are the changes I redid

  Name changes:

    Original     Changes
    1. other  ┐  2. other4 (added)
    3. other2 ┘  

scratch/main> names other4

  'other4':
  Hash          Kind   Names
  #f3lgjvjqoo   Term   other, other2, other4
```

Undo and redo get reflog entries of their own.

``` ucm
scratch/main> reflog

  Below is a record of recent changes, you can use
  `reset #abcdef` to reset the current branch to a previous
  state.

  Tip: Use `diff.namespace 1 7` to compare between points in
       history.

        Branch         Hash          Description
  1.    scratch/main   #hqd02rho7l   redo
  2.    scratch/main   #nh9dvgtl3j   undo
  3.    scratch/main   #hqd02rho7l   alias.term other scratch/main:other4
  4.    scratch/main   #nh9dvgtl3j   undo
  5.    scratch/main   #apfturkgj4   redo
  6.    scratch/main   #nh9dvgtl3j   redo
  7.    scratch/main   #upfgaihnpj   undo
  8.    scratch/main   #nh9dvgtl3j   redo
  9.    scratch/main   #upfgaihnpj   redo
  10.   scratch/main   #5f4qmi85sl   undo
  11.   scratch/main   #upfgaihnpj   undo
  12.   scratch/main   #nh9dvgtl3j   undo
  13.   scratch/main   #apfturkgj4   alias.term other scratch/main:other3
  14.   scratch/main   #nh9dvgtl3j   alias.term other scratch/main:other2
  15.   scratch/main   #upfgaihnpj   redo
  16.   scratch/main   #5f4qmi85sl   undo
  17.   scratch/main   #upfgaihnpj   move scratch/main:foo scratch/main:bar
  18.   scratch/main   #5f4qmi85sl   update
  19.   scratch/main   #8v22j44e44   builtins.merge scratch/main:lib.builtins
  20.   scratch/main   #sg60bvjo91   Project Created
```

A merge can be undone.

``` unison :hide
fromTopic = 5
```

``` ucm
scratch/main> branch topic

  Done. I've created the topic branch based off of main.

  Tip: To merge your work back into the main branch, first
       `switch /main` then `merge /topic`.

scratch/topic> update

  Okay, I'm searching the branch for code that needs to be
  updated...

  Done.

scratch/topic> switch /main
```

``` unison :hide
fromMain = 6
```

``` ucm
scratch/main> update

  Okay, I'm searching the branch for code that needs to be
  updated...

  Done.

scratch/main> merge /topic

  Loading namespaces...

  Computing diff...

  Loading definitions...

  Computing merge...

  I merged scratch/topic into scratch/main.

scratch/main> undo

  Here are the changes I undid

  Added definitions:

    1. fromTopic : Nat

scratch/main> redo

  Here are the changes I redid

  Added definitions:

    1. fromTopic : Nat

scratch/main> undo

  Here are the changes I undid

  Added definitions:

    1. fromTopic : Nat
```

## Reset

`undo` and `redo` don't look past a `reset`. Here `undo` can go back to the state the `reset` landed on, but no further, and `redo` can't bring back what the `reset` left.

``` ucm :hide
scratch/main> alias.term other other5
```

``` unison :hide
afterReset = 7
```

``` ucm :hide
scratch/main> reset /topic:
```

``` ucm :error
scratch/main> undo

  ⚠️

  Nothing more to undo. Use `reflog` or `history` to find an
  earlier state, and `reset` to go back to it.
```

``` ucm :error
scratch/main> redo

  ⚠️

  Nothing more to redo.
```

``` ucm
scratch/main> update

  Okay, I'm searching the branch for code that needs to be
  updated...

  Done.

scratch/main> undo

  Here are the changes I undid

  Added definitions:

    1. afterReset : Nat
```

``` ucm :error
scratch/main> undo

  ⚠️

  Nothing more to undo. Use `reflog` or `history` to find an
  earlier state, and `reset` to go back to it.
```

A `reset` on another branch doesn't stop this one.

``` ucm
scratch/topic> reset /main:

  Done.

scratch/main> redo

  Here are the changes I redid

  Added definitions:

    1. afterReset : Nat

scratch/main> undo

  Here are the changes I undid

  Added definitions:

    1. afterReset : Nat
```
