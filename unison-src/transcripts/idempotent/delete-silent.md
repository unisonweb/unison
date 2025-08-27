``` ucm :error
> delete foo

  ⚠️

  The following names were not found in the codebase. Check your spelling.
    foo
```

``` unison :hide
foo = 1
structural type Foo = Foo ()
```

``` ucm
> add

  Okay, I'm searching the branch for code that needs to be
  updated...

  Done.

> delete foo

  Done.

> delete.type Foo

  Done.

> delete.term Foo.Foo

  Done.
```
