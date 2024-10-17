``` ucm
scratch/main> builtins.merge

  Done.
```

``` unison  /private/tmp/scratch.u
foo = 123

bar = 456

mytest = [Ok "ok"]
```

``` ucm :added-by-ucm

  Loading changes detected in /private/tmp/scratch.u.

  I found and typechecked these definitions in
  /private/tmp/scratch.u. If you do an `add` or `update`, here's
  how your codebase would change:

    ⍟ These new definitions are ok to `add`:
    
      bar    : Nat
      foo    : Nat
      mytest : [Result]
```

``` ucm
scratch/main> add

  ⍟ I've added these definitions:

    bar    : Nat
    foo    : Nat
    mytest : [Result]
scratch/main> edit foo bar

  ☝️

  I added 2 definitions to the top of /private/tmp/scratch.u

  You can edit them there, then run `update` to replace the
  definitions currently in this namespace.
scratch/main> edit mytest

  ☝️

  I added 1 definitions to the top of /private/tmp/scratch.u

  You can edit them there, then run `update` to replace the
  definitions currently in this namespace.
```

``` unison :added-by-ucm /private/tmp/scratch.u
bar : Nat
bar = 456

foo : Nat
foo = 123
```

``` unison :added-by-ucm /private/tmp/scratch.u
test> mytest = [Ok "ok"]
```

``` ucm :error
scratch/main> edit missing

  ⚠️

  The following names were not found in the codebase. Check your spelling.
    missing
```
