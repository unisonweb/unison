``` ucm :error
scratch/main> delete foo

  ⚠️

  The following names were not found in the codebase. Check your spelling.
    foo
```

``` unison :hide
foo = 1
structural type Foo = Foo ()
```

``` ucm
scratch/main> add

  Okay, I'm searching the branch for code that needs to be
  updated...

  Done.

scratch/main> delete foo

  Done.

scratch/main> delete.type Foo

  Done.

scratch/main> delete.term Foo.Foo

  Done.
```
